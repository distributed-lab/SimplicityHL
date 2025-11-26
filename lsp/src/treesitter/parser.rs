use tower_lsp_server::lsp_types;
use tree_sitter::{Language, Node, Point, Query, QueryCursor, StreamingIterator, Tree};

use crate::{
    error::LspError,
    treesitter::{scope::ScopeGraph, variable::get_range_from_node},
};

/// Represents a classified syntactic element extracted from the Tree-sitter AST.
///
/// Tokens produced by the tree-sitter grammar are mapped into these categories
/// so that the LSP can distinguish between language constructs.
#[derive(Debug)]
pub enum Token {
    /// A user-defined variable.
    Identifier,

    /// A built-in type alias.
    BuiltinAlias,

    /// A user-defined type alias.
    Alias,

    /// A built-in function.
    BuiltinFunction,

    /// A user-defined function.
    Function,

    /// A `jet` function.
    Jet,
}

/// Represents a classified token together with its text value and source range.
pub struct TokenInfo {
    pub token: Token,

    pub text: String,
    pub range: lsp_types::Range,
}

/// Wrapper struct for working with the AST produced by Tree-sitter.
pub struct AstContext {
    /// Tree-sitter AST.
    pub tree: Tree,
    /// Scope graph which contains scopes and variables.
    pub scopes: ScopeGraph,

    /// [`tree_sitter::Language`], so we would not call
    /// `&tree_sitter_simplicityhl::LANGUAGE.into()` every time.
    language: Language,
}

impl AstContext {
    pub fn new(source: &str) -> Option<Self> {
        let mut parser = tree_sitter::Parser::new();
        // .unwrap() fails only when Language is is cannot be loaded.
        parser
            .set_language(&tree_sitter_simplicityhl::LANGUAGE.into())
            .unwrap();

        let tree = parser.parse(source, None)?;
        let scopes = ScopeGraph::new(tree.root_node(), source);

        Some(Self {
            tree,
            scopes,
            language: tree_sitter_simplicityhl::LANGUAGE.into(),
        })
    }

    /// Returns the token located at the given [`lsp_types::Position`].
    ///
    /// Only tokens defined by [`TokenInfo`] are returned.  
    pub fn get_token_on_position(
        &self,
        source: &str,
        pos: lsp_types::Position,
    ) -> Option<TokenInfo> {
        let target_point = Point {
            row: pos.line as usize,
            column: pos.character as usize,
        };
        let root_node = self.tree.root_node();
        let node_opt = root_node.named_descendant_for_point_range(target_point, target_point);
        if let Some(node) = node_opt {
            return match_node_kind(source, &node);
        }
        None
    }

    /// Returns all definitions of a type alias with the given name.
    pub fn get_alias_definition(
        &self,
        source: &str,
        alias_name: &str,
    ) -> Result<Vec<lsp_types::Range>, LspError> {
        let query = self.create_query(&format!(
            r#"definition: ((alias_name) @def (#eq? @def "{alias_name}"))"#,
        ))?;

        self.collect_query_captures(&query, source)
    }

    /// Returns all definitions of a function with the given name.
    pub fn get_function_definition(
        &self,
        source: &str,
        function_name: &str,
    ) -> Result<Vec<lsp_types::Range>, LspError> {
        let query = self.create_query(&format!(
            r#"definition: ((function_name) @def (#eq? @def "{function_name}"))"#,
        ))?;

        self.collect_query_captures(&query, source)
    }

    /// Returns the definition of a variable visible at the given position.
    ///
    /// Variable resolution is scope-dependent, so it's need to know current position.
    pub fn get_identifier_definition(
        &self,
        identifier_name: &str,
        pos: lsp_types::Position,
    ) -> Vec<tower_lsp_server::lsp_types::Range> {
        let visible_vars = self.scopes.get_visible_variables(pos, true);

        let mut result = Vec::new();
        if let Some(definition) = visible_vars.iter().find(|&v| v.name == identifier_name) {
            result.push(definition.range);
        }
        result
    }

    /// Returns all references of a type alias, including its definition.
    pub fn get_alias_reference(
        &self,
        source: &str,
        alias_name: &str,
    ) -> Result<Vec<lsp_types::Range>, LspError> {
        let query = self.create_query(&format!(
            r#"reference: ((alias_name) @call (#eq? @call "{alias_name}"))
               definition: ((alias_name) @def (#eq? @def "{alias_name}"))"#,
        ))?;

        self.collect_query_captures(&query, source)
    }

    /// Returns all references of a function, including its definition.
    pub fn get_function_reference(
        &self,
        source: &str,
        function_name: &str,
    ) -> Result<Vec<lsp_types::Range>, LspError> {
        let query = self.create_query(&format!(
            r#"reference: ((function_name) @call (#eq? @call "{function_name}"))
               definition: ((function_name) @def (#eq? @def "{function_name}"))"#,
        ))?;

        self.collect_query_captures(&query, source)
    }

    /// Returns all references of a variable visible at the given position.
    ///
    /// Variable resolution is scope-dependent, so it's need to know current position.
    pub fn get_identifier_reference(
        &self,
        identifier_name: &str,
        pos: lsp_types::Position,
    ) -> Result<Vec<lsp_types::Range>, LspError> {
        let visible_vars = self.scopes.get_visible_variables(pos, true);
        let mut result = vec![];

        let definition = visible_vars
            .iter()
            .find(|&v| v.name == identifier_name)
            .ok_or_else(|| LspError::Internal(format!("Variable {identifier_name} not found!")))?;

        result.push(definition.range);
        result.extend_from_slice(&definition.references);

        Ok(result)
    }

    /// Collects all captures produced by a [`tree_sitter::Query`] and converts
    /// them into [`lsp_types::Range`] values.
    pub fn collect_query_captures(
        &self,
        query: &Query,
        source: &str,
    ) -> Result<Vec<lsp_types::Range>, LspError> {
        let mut result = Vec::new();
        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(query, self.tree.root_node(), source.as_bytes());

        while let Some(m) = matches.next() {
            for capture in m.captures {
                result.push(get_range_from_node(&capture.node)?);
            }
        }

        Ok(result)
    }

    /// Create [`tree_sitter::Query`] with query code.
    fn create_query(&self, query: &str) -> Result<Query, LspError> {
        Query::new(&self.language, query)
            .map_err(|err| LspError::Internal(format!("Query creation error: {err}")))
    }
}

/// Classifies a Tree-sitter node and returns a corresponding [`TokenInfo`].
///
/// Returns `None` if the node does not match any supported token category.
pub fn match_node_kind(source: &str, node: &Node) -> Option<TokenInfo> {
    let node_text = source[node.byte_range()].to_string();
    let range = get_range_from_node(node).ok()?;
    match node.kind() {
        "identifier" => Some(TokenInfo {
            token: Token::Identifier,
            text: node_text,
            range,
        }),
        "builtin_alias" | "bool_type" | "unsigned_type" => Some(TokenInfo {
            token: Token::BuiltinAlias,
            text: node_text,
            range,
        }),
        "sum_type" => Some(TokenInfo {
            token: Token::BuiltinAlias,
            text: "Either".to_string(),
            range,
        }),
        "option_type" => Some(TokenInfo {
            token: Token::BuiltinAlias,
            text: "Option".to_string(),
            range,
        }),
        "alias_name" => Some(TokenInfo {
            token: Token::Alias,
            text: node_text,
            range,
        }),
        "call_name" => Some(TokenInfo {
            token: Token::BuiltinFunction,
            text: node_text,
            range,
        }),
        "unwrap_left" | "unwrap_right" | "type_cast" | "fold" | "array_fold" | "for_while" => {
            Some(TokenInfo {
                token: Token::BuiltinFunction,
                text: node.kind().to_string(),
                range: get_range_from_node(&node.child(0)?).ok()?,
            })
        }
        "function_name" => {
            if let Some(parent) = node.parent() {
                if parent.kind() == "jet" {
                    return match_node_kind(source, &parent);
                }
            }
            Some(TokenInfo {
                token: Token::Function,
                text: node_text,
                range,
            })
        }
        "jet" => Some(TokenInfo {
            token: Token::Jet,
            text: source[node.child(2)?.byte_range()].to_string(),
            range,
        }),
        _ => None,
    }
}
