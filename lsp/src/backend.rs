use ropey::Rope;
use serde_json::Value;

use std::collections::HashMap;
use std::str::FromStr;
use std::sync::Arc;
use tokio::sync::RwLock;

use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::{
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidChangeWorkspaceFoldersParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse,
    ExecuteCommandParams, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, Location,
    MarkupContent, MarkupKind, MessageType, OneOf, Range, ReferenceParams, SaveOptions,
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensResult, SemanticTokensServerCapabilities, ServerCapabilities, SignatureHelp,
    SignatureHelpOptions, SignatureHelpParams, SymbolKind, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Uri,
    WorkDoneProgressOptions, WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
};
use tower_lsp_server::{Client, LanguageServer};

use miniscript::iter::TreeLike;
use simplicityhl::{
    ast,
    error::{RichError, WithFile},
    parse,
    parse::ParseFromStr,
};

use crate::completion::{self, CompletionProvider};
use crate::error::LspError;
use crate::function::Functions;
use crate::utils::{
    create_signature_info, find_all_references, find_builtin_signature, find_function_call_context,
    find_function_name_range, find_key_position, find_related_call, get_call_span,
    get_comments_from_lines, position_to_span, span_contains, span_to_positions,
};

/// Semantic token type indices - must match the legend order
mod semantic_token_types {
    pub const FUNCTION: u32 = 0;
    pub const NAMESPACE: u32 = 5;
}

/// Get the semantic token legend for this server
fn get_semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::FUNCTION,
            SemanticTokenType::PARAMETER,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::TYPE,
            SemanticTokenType::KEYWORD,
            SemanticTokenType::NAMESPACE,
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
        ],
    }
}

#[derive(Debug)]
struct Document {
    functions: Functions,
    text: Rope,
}

#[derive(Debug)]
pub struct Backend {
    client: Client,

    document_map: Arc<RwLock<HashMap<Uri, Document>>>,

    completion_provider: CompletionProvider,
}

struct TextDocumentItem<'a> {
    uri: Uri,
    text: &'a str,
    version: Option<i32>,
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![":".to_string(), "<".to_string()]),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: Some(vec![",".to_string()]),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions::default(),
                            legend: get_semantic_token_legend(),
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {}

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {}

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {}

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {}

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        Ok(None)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: &params.text_document.text,
            version: Some(params.text_document.version),
        })
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            text: &params.content_changes[0].text,
            uri: params.text_document.uri,
            version: Some(params.text_document.version),
        })
        .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.on_change(TextDocumentItem {
                uri: params.text_document.uri,
                text: &text,
                version: None,
            })
            .await;
        }
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        // .wit files don't have semantic tokens
        if uri.path().as_str().ends_with(".wit") {
            return Ok(None);
        }

        let documents = self.document_map.read().await;

        // Return None if document not found (e.g., file has parse errors)
        let Some(doc) = documents.get(uri) else {
            return Ok(None);
        };

        let functions = doc.functions.functions();
        let mut raw_tokens: Vec<(u32, u32, u32, u32, u32)> = Vec::new(); // (line, col, len, type, modifiers)

        for func in &functions {
            // Add function name token (declaration)
            if let Ok(name_range) = find_function_name_range(func, &doc.text) {
                let len = func.name().as_inner().len() as u32;
                raw_tokens.push((
                    name_range.start.line,
                    name_range.start.character,
                    len,
                    semantic_token_types::FUNCTION,
                    0b11, // DECLARATION | DEFINITION
                ));
            }

            // Add function call tokens by walking the expression tree
            let calls = parse::ExprTree::Expression(func.body())
                .pre_order_iter()
                .filter_map(|expr| {
                    if let parse::ExprTree::Call(call) = expr {
                        get_call_span(call).ok().map(|span| (call, span))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            for (call, span) in calls {
                if let Ok((start, _end)) = span_to_positions(&span) {
                    let name = call.name();
                    let name_str = name.to_string();

                    // Determine token type based on call name
                    let (token_type, prefix_len) = match name {
                        parse::CallName::Jet(_) => {
                            // jet::xxx - add namespace token for "jet" and function for xxx
                            // First add "jet" as namespace
                            raw_tokens.push((
                                start.line,
                                start.character,
                                3, // "jet"
                                semantic_token_types::NAMESPACE,
                                0,
                            ));
                            // The function name starts after "jet::"
                            (semantic_token_types::FUNCTION, 5)
                        }
                        parse::CallName::Custom(_) => (semantic_token_types::FUNCTION, 0),
                        _ => (semantic_token_types::FUNCTION, 0), // Built-in functions
                    };

                    // Add the function name token
                    let func_name_len = if prefix_len > 0 {
                        name_str.len().saturating_sub(prefix_len)
                    } else {
                        name_str.len()
                    };

                    if func_name_len > 0 {
                        raw_tokens.push((
                            start.line,
                            start.character + prefix_len as u32,
                            func_name_len as u32,
                            token_type,
                            0,
                        ));
                    }
                }
            }
        }

        // Sort tokens by position (line, then column)
        raw_tokens.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

        // Convert to delta-encoded semantic tokens
        let mut semantic_tokens = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_char = 0u32;

        for (line, col, len, token_type, modifiers) in raw_tokens {
            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 {
                col - prev_char
            } else {
                col
            };

            semantic_tokens.push(SemanticToken {
                delta_line,
                delta_start,
                length: len,
                token_type,
                token_modifiers_bitset: modifiers,
            });

            prev_line = line;
            prev_char = col;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        // .wit files don't have symbols
        if uri.path().as_str().ends_with(".wit") {
            return Ok(None);
        }

        let documents = self.document_map.read().await;

        // Return None if document not found (e.g., file has parse errors)
        let Some(doc) = documents.get(uri) else {
            return Ok(None);
        };

        let functions = doc.functions.functions();

        let symbols: Vec<DocumentSymbol> = functions
            .iter()
            .filter_map(|func| {
                // Get the full function range
                let (start, end) = span_to_positions(func.span()).ok()?;
                let full_range = Range { start, end };

                // Get the function name range for selection
                let selection_range = find_function_name_range(func, &doc.text).ok()?;

                // Build parameters detail string
                let params_str = func
                    .params()
                    .iter()
                    .map(|p| format!("{p}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                let return_type = match func.ret() {
                    Some(ret) => format!("{ret}"),
                    None => "()".to_string(),
                };

                #[allow(deprecated)]
                Some(DocumentSymbol {
                    name: func.name().to_string(),
                    detail: Some(format!("fn({params_str}) -> {return_type}")),
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    deprecated: None,
                    range: full_range,
                    selection_range,
                    children: None,
                })
            })
            .collect();

        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let documents = self.document_map.read().await;
        let uri = &params.text_document_position_params.text_document.uri;

        // Return None if document not found (e.g., file has parse errors)
        let Some(doc) = documents.get(uri) else {
            return Ok(None);
        };

        let token_pos = params.text_document_position_params.position;

        // Get the current line up to cursor position
        let line = doc
            .text
            .lines()
            .nth(token_pos.line as usize)
            .ok_or(LspError::Internal("Line not found".into()))?;

        let line_str = line
            .get_slice(..token_pos.character as usize)
            .map(|s| s.to_string())
            .unwrap_or_default();

        // Find function call context: look for unclosed '(' and count commas
        let (func_name, active_param) = match find_function_call_context(&line_str) {
            Some(ctx) => ctx,
            None => return Ok(None),
        };

        // Try to find the function signature
        let signature_info = if func_name.starts_with("jet::") {
            // It's a jet function
            let jet_name = func_name.strip_prefix("jet::").unwrap_or(&func_name);
            match simplicityhl::simplicity::jet::Elements::from_str(jet_name) {
                Ok(element) => {
                    let template = completion::jet::jet_to_template(element);
                    Some(create_signature_info(&template))
                }
                Err(_) => None,
            }
        } else if let Some((function, function_doc)) = doc.functions.get(&func_name) {
            // It's a custom function
            let template = completion::function_to_template(function, function_doc);
            Some(create_signature_info(&template))
        } else {
            // Try builtin functions
            find_builtin_signature(&func_name)
        };

        match signature_info {
            Some(sig) => Ok(Some(SignatureHelp {
                signatures: vec![sig],
                active_signature: Some(0),
                active_parameter: Some(active_param),
            })),
            None => Ok(None),
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let documents = self.document_map.read().await;
        let uri = &params.text_document_position.text_document.uri;

        // Return None if document not found (e.g., file has parse errors)
        let Some(doc) = documents.get(uri) else {
            return Ok(None);
        };

        let pos = params.text_document_position.position;

        let line = doc
            .text
            .lines()
            .nth(pos.line as usize)
            .ok_or(LspError::Internal("Rope proccesing error".into()))?;

        let slice = line
            .get_slice(..pos.character as usize)
            .ok_or(LspError::ConversionFailed(
                "Rope to slice conversion failed".into(),
            ))?;

        let prefix = slice.as_str().ok_or(LspError::ConversionFailed(
            "RopeSlice to str conversion failed".into(),
        ))?;

        let completions = self
            .completion_provider
            .process_completions(prefix, &doc.functions.functions_and_docs())
            .map(CompletionResponse::Array);

        Ok(completions)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;

        // .wit files don't have hover info
        if uri.path().as_str().ends_with(".wit") {
            return Ok(None);
        }

        let documents = self.document_map.read().await;

        // Return None if document not found (e.g., file has parse errors)
        let Some(doc) = documents.get(uri) else {
            return Ok(None);
        };
        let functions = doc.functions.functions();

        let token_pos = params.text_document_position_params.position;

        let token_span = position_to_span(token_pos)?;
        let Ok(Some(call)) = find_related_call(&functions, token_span) else {
            return Ok(None);
        };

        let call_span = get_call_span(call)?;
        let (start, end) = span_to_positions(&call_span)?;

        let description = match call.name() {
            parse::CallName::Jet(jet) => {
                let element =
                    simplicityhl::simplicity::jet::Elements::from_str(format!("{jet}").as_str())
                        .map_err(|err| LspError::ConversionFailed(err.to_string()))?;

                let template = completion::jet::jet_to_template(element);
                format!(
                    "Jet function\n```simplicityhl\nfn {}({}) -> {}\n```\n---\n\n{}",
                    template.display_name,
                    template.args.join(", "),
                    template.return_type,
                    template.description
                )
            }
            parse::CallName::Custom(func) => {
                let (function, function_doc) =
                    doc.functions
                        .get(func.as_inner())
                        .ok_or(LspError::FunctionNotFound(format!(
                            "Function {func} is not found"
                        )))?;

                let template = completion::function_to_template(function, function_doc);
                format!(
                    "```simplicityhl\nfn {}({}) -> {}\n```\n---\n{}",
                    template.display_name,
                    template.args.join(", "),
                    template.return_type,
                    template.description
                )
            }
            other => {
                let Some(template) = completion::builtin::match_callname(other) else {
                    return Ok(None);
                };
                format!(
                    "Built-in function\n```simplicityhl\nfn {}({}) -> {}\n```\n---\n{}",
                    template.display_name,
                    template.args.join(", "),
                    template.return_type,
                    template.description
                )
            }
        };

        Ok(Some(Hover {
            contents: tower_lsp_server::lsp_types::HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: description,
            }),
            range: Some(Range { start, end }),
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let documents = self.document_map.read().await;
        let uri = &params.text_document_position_params.text_document.uri;

        // Return None if document not found (e.g., file has parse errors)
        let Some(doc) = documents.get(uri) else {
            return Ok(None);
        };
        let functions = doc.functions.functions();

        let token_position = params.text_document_position_params.position;
        let token_span = position_to_span(token_position)?;

        let Ok(Some(call)) = find_related_call(&functions, token_span) else {
            let Some(func) = functions
                .iter()
                .find(|func| span_contains(func.span(), &token_span))
            else {
                return Ok(None);
            };
            let range = find_function_name_range(func, &doc.text)?;

            if token_position <= range.end && token_position >= range.start {
                return Ok(Some(GotoDefinitionResponse::from(Location::new(
                    uri.clone(),
                    range,
                ))));
            }
            return Ok(None);
        };

        match call.name() {
            simplicityhl::parse::CallName::Custom(func) => {
                let function =
                    doc.functions
                        .get_func(func.as_inner())
                        .ok_or(LspError::FunctionNotFound(format!(
                            "Function {func} is not found"
                        )))?;

                let (start, end) = span_to_positions(function.as_ref())?;
                Ok(Some(GotoDefinitionResponse::from(Location::new(
                    uri.clone(),
                    Range::new(start, end),
                ))))
            }
            _ => Ok(None),
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let documents = self.document_map.read().await;
        let uri = &params.text_document_position.text_document.uri;

        // Return None if document not found (e.g., file has parse errors)
        let Some(doc) = documents.get(uri) else {
            return Ok(None);
        };
        let functions = doc.functions.functions();

        let token_position = params.text_document_position.position;

        let token_span = position_to_span(token_position)?;

        let call_name =
            find_related_call(&functions, token_span)?.map(simplicityhl::parse::Call::name);

        match call_name {
            Some(parse::CallName::Custom(_)) | None => {}
            Some(name) => {
                return Ok(Some(
                    find_all_references(&functions, name)?
                        .iter()
                        .map(|range| Location {
                            range: *range,
                            uri: uri.clone(),
                        })
                        .collect(),
                ));
            }
        }

        let Some(func) = functions.iter().find(|func| match call_name {
            Some(parse::CallName::Custom(name)) => func.name() == name,
            _ => span_contains(func.span(), &token_span),
        }) else {
            return Ok(None);
        };

        let range = find_function_name_range(func, &doc.text)?;

        if (token_position <= range.end && token_position >= range.start) || call_name.is_some() {
            Ok(Some(
                find_all_references(&functions, &parse::CallName::Custom(func.name().clone()))?
                    .into_iter()
                    .chain(std::iter::once(range))
                    .map(|range| Location {
                        range,
                        uri: uri.clone(),
                    })
                    .collect(),
            ))
        } else {
            Ok(None)
        }
    }
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            document_map: Arc::new(RwLock::new(HashMap::new())),
            completion_provider: CompletionProvider::new(),
        }
    }

    /// Function which executed on change of file (`did_save`, `did_open` or `did_change` methods)
    async fn on_change(&self, params: TextDocumentItem<'_>) {
        // Check if this is a witness file
        if params.uri.path().as_str().ends_with(".wit") {
            self.on_change_witness(params).await;
            return;
        }

        let (err, document) = parse_program(params.text);

        let mut documents = self.document_map.write().await;
        if let Some(doc) = document {
            documents.insert(params.uri.clone(), doc);
        } else if let Some(doc) = documents.get_mut(&params.uri) {
            doc.text = Rope::from_str(params.text);
        }

        match err {
            None => {
                self.client
                    .publish_diagnostics(params.uri.clone(), vec![], params.version)
                    .await;
            }
            Some(err) => {
                let (start, end) = match span_to_positions(err.span()) {
                    Ok(result) => result,
                    Err(err) => {
                        self.client
                            .log_message(
                                MessageType::ERROR,
                                format!("Catch error while parsing span: {err}"),
                            )
                            .await;
                        return;
                    }
                };

                self.client
                    .publish_diagnostics(
                        params.uri.clone(),
                        vec![Diagnostic::new_simple(
                            Range::new(start, end),
                            err.error().to_string(),
                        )],
                        params.version,
                    )
                    .await;
            }
        }
    }

    /// Validate witness (.wit) files
    async fn on_change_witness(&self, params: TextDocumentItem<'_>) {
        let diagnostics = validate_witness_file(params.text);
        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, params.version)
            .await;
    }
}

/// Create [`Document`] using parsed program and code.
fn create_document(program: &simplicityhl::parse::Program, text: &str) -> Document {
    let mut document = Document {
        functions: Functions::new(),
        text: Rope::from_str(text),
    };

    program
        .items()
        .iter()
        .filter_map(|item| {
            if let parse::Item::Function(func) = item {
                Some(func)
            } else {
                None
            }
        })
        .for_each(|func| {
            let start_line = u32::try_from(func.as_ref().start.line.get()).unwrap_or_default() - 1;

            document.functions.insert(
                func.name().to_string(),
                func.to_owned(),
                get_comments_from_lines(start_line, &document.text),
            );
        });

    document
}

/// Parse program using [`simplicityhl`] compiler and return [`RichError`],
/// which used in Diagnostic. Also create [`Document`] from parsed program.
fn parse_program(text: &str) -> (Option<RichError>, Option<Document>) {
    let program = match parse::Program::parse_from_str(text) {
        Ok(p) => p,
        Err(e) => return (Some(e), None),
    };

    (
        ast::Program::analyze(&program).with_file(text).err(),
        Some(create_document(&program, text)),
    )
}

/// Validate a witness (.wit) file and return diagnostics.
fn validate_witness_file(text: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Try to parse as JSON
    let json: serde_json::Value = match serde_json::from_str(text) {
        Ok(v) => v,
        Err(e) => {
            // JSON parse error - find the position
            let line = e.line().saturating_sub(1) as u32;
            let col = e.column().saturating_sub(1) as u32;
            diagnostics.push(Diagnostic::new_simple(
                Range::new(
                    tower_lsp_server::lsp_types::Position::new(line, col),
                    tower_lsp_server::lsp_types::Position::new(line, col + 1),
                ),
                format!("JSON syntax error: {}", e),
            ));
            return diagnostics;
        }
    };

    // Must be an object
    let obj = match json.as_object() {
        Some(o) => o,
        None => {
            diagnostics.push(Diagnostic::new_simple(
                Range::new(
                    tower_lsp_server::lsp_types::Position::new(0, 0),
                    tower_lsp_server::lsp_types::Position::new(0, 1),
                ),
                "Witness file must be a JSON object".to_string(),
            ));
            return diagnostics;
        }
    };

    // Validate each witness entry
    for (name, value) in obj {
        let witness_obj = match value.as_object() {
            Some(o) => o,
            None => {
                // Find approximate position for this key
                if let Some(pos) = find_key_position(text, name) {
                    diagnostics.push(Diagnostic::new_simple(
                        Range::new(pos, pos),
                        format!(
                            "Witness '{}' must be an object with 'value' and 'type' fields",
                            name
                        ),
                    ));
                }
                continue;
            }
        };

        // Check for required 'value' field
        if !witness_obj.contains_key("value") {
            if let Some(pos) = find_key_position(text, name) {
                diagnostics.push(Diagnostic::new_simple(
                    Range::new(pos, pos),
                    format!("Witness '{}' is missing required 'value' field", name),
                ));
            }
        }

        // Check for required 'type' field
        if !witness_obj.contains_key("type") {
            if let Some(pos) = find_key_position(text, name) {
                diagnostics.push(Diagnostic::new_simple(
                    Range::new(pos, pos),
                    format!("Witness '{}' is missing required 'type' field", name),
                ));
            }
        }
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_program() -> &'static str {
        "fn add(a: u32, b: u32) -> u32 { let (_, res): (bool, u32) = jet::add_32(a, b); res }
         fn main() {}"
    }

    fn invalid_program_on_ast() -> &'static str {
        "fn add(a: u32, b: u32) -> u32 {}"
    }

    fn invalid_program_on_parsing() -> &'static str {
        "fn add(a: u32 b: u32) -> u32 {}"
    }

    #[test]
    fn test_parse_program_valid() {
        let (err, doc) = parse_program(sample_program());
        assert!(err.is_none(), "Expected no parsing error");
        let doc = doc.expect("Expected Some(Document)");
        assert_eq!(doc.functions.map.len(), 2);
    }

    #[test]
    fn test_parse_program_invalid_ast() {
        let (err, doc) = parse_program(invalid_program_on_ast());
        assert!(
            err.unwrap()
                .to_string()
                .contains("Expected expression of type `u32`, found type `()`"),
            "Expected error on return type"
        );
        assert!(doc.is_some(), "Expected problem in AST build, not parse");
    }

    #[test]
    fn test_parse_program_invalid_parse() {
        let (err, doc) = parse_program(invalid_program_on_parsing());
        assert!(
            err.unwrap().to_string().contains("Grammar error"),
            "Expected `Grammar error`"
        );
        assert!(doc.is_none(), "Expected no document to return");
    }
}
