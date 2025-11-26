use tower_lsp_server::lsp_types::{self, CompletionItem, CompletionItemKind};
use tree_sitter::Node;

use crate::error::LspError;

#[derive(Debug, Clone)]
/// Info about variable.
pub struct VariableInfo {
    pub name: String,
    pub ty: String,
    pub range: lsp_types::Range,
    pub references: Vec<lsp_types::Range>,
}

/// Parse assignment statement into [`VariableInfo`].
///
/// This algorithm is modified code for checking type in [`simplicityhl::pattern::Pattern::is_of_type`].
pub fn parse_assignment(text: &str, node: &Node, ty: &Node) -> Option<Vec<VariableInfo>> {
    let mut stack = vec![(*node, *ty)];
    let mut output = Vec::new();

    while let Some((pattern, ty)) = stack.pop() {
        let (pattern, ty) = (pattern.child(0)?, ty.child(0)?);

        match (pattern.kind(), ty.kind()) {
            ("variable_pattern", _) => {
                output.push(VariableInfo {
                    name: text[pattern.byte_range()].to_string(),
                    ty: text[ty.byte_range()].to_string(),
                    range: get_range_from_node(&pattern).ok()?,
                    references: vec![],
                });
            }

            ("tuple_pattern", "tuple_type") => {
                stack.extend(
                    pattern
                        .named_children(&mut pattern.walk())
                        .zip(ty.named_children(&mut ty.walk())),
                );
            }
            ("array_pattern", "array_type") => {
                stack.extend(
                    pattern
                        .named_children(&mut pattern.walk())
                        .zip(std::iter::repeat(ty.named_child(0)?)),
                );
            }
            ("ignore_pattern", _) => {}
            _ => return None,
        }
    }
    Some(output)
}

pub fn get_range_from_node(node: &Node) -> Result<lsp_types::Range, LspError> {
    Ok(lsp_types::Range {
        start: lsp_types::Position::new(
            node.start_position().row.try_into()?,
            node.start_position().column.try_into()?,
        ),
        end: lsp_types::Position::new(
            node.end_position().row.try_into()?,
            node.end_position().column.try_into()?,
        ),
    })
}

impl From<&VariableInfo> for lsp_types::CompletionItem {
    fn from(val: &VariableInfo) -> Self {
        CompletionItem {
            label: val.name.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some(val.ty.clone()),
            ..Default::default()
        }
    }
}
