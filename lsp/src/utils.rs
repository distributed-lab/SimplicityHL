use miniscript::iter::TreeLike;

use crate::completion;
use crate::error::LspError;
use ropey::Rope;
use simplicityhl::parse::{self, CallName};
use tower_lsp_server::lsp_types::{
    self, MarkupContent, MarkupKind, ParameterInformation, ParameterLabel, SignatureInformation,
};

pub fn span_contains(a: &simplicityhl::error::Span, b: &simplicityhl::error::Span) -> bool {
    a.start <= b.start && a.end >= b.end
}

pub fn offset_to_position(offset: usize, rope: &Rope) -> Result<lsp_types::Position, LspError> {
    let line = rope.try_char_to_line(offset)?;
    let first_char_of_line = rope.try_line_to_char(line)?;
    let column = offset - first_char_of_line;
    Ok(lsp_types::Position::new(
        <u32>::try_from(line)?,
        <u32>::try_from(column)?,
    ))
}

pub fn position_to_offset(position: lsp_types::Position, rope: &Rope) -> Result<usize, LspError> {
    let line_char_offset = rope.try_line_to_char(position.line as usize)?;
    let slice = rope.slice(0..line_char_offset + position.character as usize);
    Ok(slice.len_bytes())
}

/// Convert [`simplicityhl::error::Span`] to [`tower_lsp_server::lsp_types::Position`]
///
/// Converting is required because `simplicityhl::error::Span` using their own versions of `Position`,
/// which contains non-zero column and line, so they are always starts with one.
/// `Position` required for diagnostic starts with zero
pub fn span_to_positions(
    span: &simplicityhl::error::Span,
    rope: &Rope,
) -> Result<(lsp_types::Position, lsp_types::Position), LspError> {
    Ok((
        offset_to_position(span.start, rope)?,
        offset_to_position(span.end, rope)?,
    ))
}

/// Convert [`tower_lsp_server::lsp_types::Position`] to [`simplicityhl::error::Span`]
///
/// Useful when [`tower_lsp_server::lsp_types::Position`] represents some singular point.
pub fn position_to_span(
    position: lsp_types::Position,
    rope: &Rope,
) -> Result<simplicityhl::error::Span, LspError> {
    let start_line = position_to_offset(position, rope)?;

    Ok(simplicityhl::error::Span::new(start_line, start_line))
}

/// Get document comments, using lines above given line index. Only used to
/// get documentation for custom functions.
pub fn get_comments_from_lines(line: u32, rope: &Rope) -> String {
    let mut lines = Vec::new();

    if line == 0 {
        return String::new();
    }

    for i in (0..line).rev() {
        let Some(rope_slice) = rope.get_line(i as usize) else {
            break;
        };
        let text = rope_slice.to_string();

        if text.starts_with("///") {
            let doc = text
                .strip_prefix("///")
                .unwrap_or("")
                .trim_end()
                .to_string();
            lines.push(doc);
        } else {
            break;
        }
    }

    lines.reverse();

    let mut result = String::new();
    let mut prev_line_was_text = false;

    for line in lines {
        let trimmed = line.trim();

        let is_md_block = trimmed.is_empty()
            || trimmed.starts_with('#')
            || trimmed.starts_with('-')
            || trimmed.starts_with('*')
            || trimmed.starts_with('>')
            || trimmed.starts_with("```")
            || trimmed.starts_with("    ");

        if result.is_empty() {
            result.push_str(trimmed);
        } else if prev_line_was_text && !is_md_block {
            result.push(' ');
            result.push_str(trimmed);
        } else {
            result.push('\n');
            result.push_str(trimmed);
        }

        prev_line_was_text = !trimmed.is_empty() && !is_md_block;
    }

    result
}

/// Find [`simplicityhl::parse::Call`] which contains given [`simplicityhl::error::Span`], which also have minimal Span.
pub fn find_related_call<'a>(
    functions: &'a [&'a parse::Function],
    token_span: simplicityhl::error::Span,
) -> Result<Option<&'a simplicityhl::parse::Call>, LspError> {
    let func = functions
        .iter()
        .find(|func| span_contains(func.span(), &token_span))
        .ok_or(LspError::CallNotFound(
            "Span of the call is not inside function.".into(),
        ))?;

    let call = parse::ExprTree::Expression(func.body())
        .pre_order_iter()
        .filter_map(|expr| {
            if let parse::ExprTree::Call(call) = expr {
                // Only include if call span can be obtained
                get_call_span(call).ok().map(|span| (call, span))
            } else {
                None
            }
        })
        .filter(|(_, span)| span_contains(span, &token_span))
        .map(|(call, _)| call)
        .last();

    Ok(call)
}

pub fn find_function_name_range(
    function: &parse::Function,
    text: &Rope,
) -> Result<lsp_types::Range, LspError> {
    let start_line = offset_to_position(function.span().start, text)?.line;
    let Some((line, character)) =
        text.lines()
            .enumerate()
            .skip(start_line as usize)
            .find_map(|(i, line)| {
                line.to_string()
                    .find(function.name().as_inner())
                    .map(|col| (i, col))
            })
    else {
        return Err(LspError::FunctionNotFound(format!(
            "Function with name {} not found",
            function.name()
        )));
    };

    let func_size = u32::try_from(function.name().as_inner().len()).map_err(LspError::from)?;

    let (line, character) = (
        u32::try_from(line).map_err(LspError::from)?,
        u32::try_from(character).map_err(LspError::from)?,
    );

    let (start, end) = (
        lsp_types::Position { line, character },
        lsp_types::Position {
            line,
            character: character + func_size,
        },
    );
    Ok(lsp_types::Range { start, end })
}

pub fn get_call_span(
    call: &simplicityhl::parse::Call,
) -> Result<simplicityhl::error::Span, LspError> {
    let length = call.name().to_string().len();

    Ok(simplicityhl::error::Span {
        start: call.span().start,
        end: call.span().start + length,
    })
}

pub fn find_all_references<'a>(
    text: &Rope,
    functions: &'a [&'a parse::Function],
    call_name: &CallName,
) -> Result<Vec<lsp_types::Range>, LspError> {
    functions
        .iter()
        .flat_map(|func| {
            parse::ExprTree::Expression(func.body())
                .pre_order_iter()
                .filter_map(|expr| {
                    if let parse::ExprTree::Call(call) = expr {
                        get_call_span(call).ok().map(|span| (call, span))
                    } else {
                        None
                    }
                })
                .filter(|(call, _)| call.name() == call_name)
                .map(|(_, span)| span)
                .collect::<Vec<_>>()
        })
        .map(|span| {
            let (start, end) = span_to_positions(&span, text)?;
            Ok(lsp_types::Range { start, end })
        })
        .collect::<Result<Vec<_>, LspError>>()
}

/// Find the position of a key in the JSON text
pub fn find_key_position(text: &str, key: &str) -> Option<lsp_types::Position> {
    let search = format!("\"{}\"", key);
    for (line_num, line) in text.lines().enumerate() {
        if let Some(col) = line.find(&search) {
            return Some(lsp_types::Position::new(line_num as u32, col as u32));
        }
    }
    None
}

/// Find function call context from the current line.
/// Returns (function_name, active_parameter_index) if inside a function call.
pub fn find_function_call_context(line: &str) -> Option<(String, u32)> {
    let mut paren_depth = 0;
    let mut bracket_depth = 0;
    let mut angle_depth = 0;
    let mut last_open_paren = None;
    let mut comma_count = 0;

    // Scan from the end to find the innermost unclosed function call
    for (i, ch) in line.chars().rev().enumerate() {
        let pos = line.len() - 1 - i;
        match ch {
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                } else {
                    // Found unclosed '(' - this is our function call
                    last_open_paren = Some(pos);
                    break;
                }
            }
            ']' => bracket_depth += 1,
            '[' => {
                if bracket_depth > 0 {
                    bracket_depth -= 1;
                }
            }
            '>' => angle_depth += 1,
            '<' => {
                if angle_depth > 0 {
                    angle_depth -= 1;
                }
            }
            ',' if paren_depth == 0 && bracket_depth == 0 && angle_depth == 0 => {
                comma_count += 1;
            }
            _ => {}
        }
    }

    let open_paren_pos = last_open_paren?;

    // Extract function name before the '('
    let before_paren = &line[..open_paren_pos];
    let func_name = extract_function_name(before_paren)?;

    Some((func_name, comma_count))
}

/// Extract function name from text before an opening parenthesis.
/// Handles patterns like: `func_name`, `jet::add_32`, `fold::<f, 8>`
pub fn extract_function_name(text: &str) -> Option<String> {
    let trimmed = text.trim_end();

    // Skip generic parameters if present (e.g., `fold::<f, 8>`)
    let without_generics = if trimmed.ends_with('>') {
        let mut depth = 0;
        let mut start = None;
        for (i, ch) in trimmed.chars().rev().enumerate() {
            match ch {
                '>' => depth += 1,
                '<' => {
                    depth -= 1;
                    if depth == 0 {
                        start = Some(trimmed.len() - 1 - i);
                        break;
                    }
                }
                _ => {}
            }
        }
        match start {
            Some(pos) => {
                let before = &trimmed[..pos];
                // Remove the `::` before `<` if present
                before.strip_suffix("::").unwrap_or(before)
            }
            None => trimmed,
        }
    } else {
        trimmed
    };

    // Now find the function name - it should be an identifier possibly with `::`
    let mut name_chars = Vec::new();

    for ch in without_generics.chars().rev() {
        if ch.is_alphanumeric() || ch == '_' || ch == ':' {
            name_chars.push(ch);
        } else {
            break;
        }
    }

    if name_chars.is_empty() {
        return None;
    }

    name_chars.reverse();
    let name: String = name_chars.into_iter().collect();

    // Clean up leading colons
    let cleaned = name.trim_start_matches(':');
    if cleaned.is_empty() {
        None
    } else {
        Some(cleaned.to_string())
    }
}

/// Create SignatureInformation from a FunctionTemplate.
pub fn create_signature_info(
    template: &completion::types::FunctionTemplate,
) -> SignatureInformation {
    let params: Vec<ParameterInformation> = template
        .args
        .iter()
        .map(|arg| ParameterInformation {
            label: ParameterLabel::Simple(arg.clone()),
            documentation: None,
        })
        .collect();

    let signature_label = format!(
        "fn {}({}) -> {}",
        template.display_name,
        template.args.join(", "),
        template.return_type
    );

    SignatureInformation {
        label: signature_label,
        documentation: if template.description.is_empty() {
            None
        } else {
            Some(lsp_types::Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: template.description.clone(),
            }))
        },
        parameters: Some(params),
        active_parameter: None,
    }
}

/// Find signature for builtin functions.
pub fn find_builtin_signature(name: &str) -> Option<SignatureInformation> {
    use simplicityhl::str::AliasName;
    use simplicityhl::types::AliasedType;

    let ty = AliasedType::from(AliasName::from_str_unchecked("T"));

    // Match common builtin function names
    let call_name = match name {
        "unwrap_left" => Some(CallName::UnwrapLeft(ty.clone())),
        "unwrap_right" => Some(CallName::UnwrapRight(ty.clone())),
        "unwrap" => Some(CallName::Unwrap),
        "is_none" => Some(CallName::IsNone(ty.clone())),
        "assert!" => Some(CallName::Assert),
        "panic!" => Some(CallName::Panic),
        "dbg!" => Some(CallName::Debug),
        _ => None,
    };

    let call_name = call_name?;
    let template = completion::builtin::match_callname(&call_name)?;
    Some(create_signature_info(&template))
}

#[cfg(test)]
mod tests {
    use super::*;
    use ropey::Rope;

    #[test]
    fn test_get_comments_from_lines() {
        let text = Rope::from_str("/// This is a test.\n/// It has two lines.\nfn func() {}");
        let result = get_comments_from_lines(2, &text);
        assert_eq!(result, "This is a test. It has two lines.");

        let text = Rope::from_str("/// # Title\n/// - Point one\n/// - Point two\nfn func() {}");
        let result = get_comments_from_lines(3, &text);
        assert_eq!(result, "# Title\n- Point one\n- Point two");

        let text = Rope::from_str(
            "/// This is not part of the doc \n\n/// This is part of the doc\nfn func() {}",
        );
        let result = get_comments_from_lines(3, &text);
        assert_eq!(result, "This is part of the doc");

        let text = Rope::from_str("fn func() {}");
        let result = get_comments_from_lines(0, &text);
        assert_eq!(result, "");
    }

    #[test]
    fn test_extract_function_name() {
        // Simple function name
        assert_eq!(extract_function_name("foo"), Some("foo".to_string()));
        assert_eq!(
            extract_function_name("my_func"),
            Some("my_func".to_string())
        );

        // With module prefix
        assert_eq!(
            extract_function_name("jet::add_32"),
            Some("jet::add_32".to_string())
        );

        // With generic parameters
        assert_eq!(
            extract_function_name("fold::<f, 8>"),
            Some("fold".to_string())
        );
        assert_eq!(
            extract_function_name("unwrap_left::<u8>"),
            Some("unwrap_left".to_string())
        );

        // With leading whitespace/expressions
        assert_eq!(
            extract_function_name("let x = foo"),
            Some("foo".to_string())
        );

        // Empty input
        assert_eq!(extract_function_name(""), None);
    }

    #[test]
    fn test_find_function_call_context() {
        // Simple function call
        assert_eq!(
            find_function_call_context("foo("),
            Some(("foo".to_string(), 0))
        );
        assert_eq!(
            find_function_call_context("foo(a, "),
            Some(("foo".to_string(), 1))
        );
        assert_eq!(
            find_function_call_context("foo(a, b, "),
            Some(("foo".to_string(), 2))
        );

        // Nested function calls
        assert_eq!(
            find_function_call_context("outer(inner(x), "),
            Some(("outer".to_string(), 1))
        );

        // With module prefix
        assert_eq!(
            find_function_call_context("jet::add_32(a, "),
            Some(("jet::add_32".to_string(), 1))
        );

        // No function call
        assert_eq!(find_function_call_context("let x = 5"), None);
    }
}
