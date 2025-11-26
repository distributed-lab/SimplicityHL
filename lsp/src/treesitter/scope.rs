use std::collections::HashMap;
use tower_lsp_server::lsp_types;
use tree_sitter::Node;

use crate::treesitter::variable::{self, get_range_from_node, VariableInfo};

/// Scope that contains variables within it.
#[derive(Debug, Clone)]
pub struct Scope {
    /// Id of this scope.
    pub id: usize,

    /// Id of parent scope.
    pub parent_id: Option<usize>,

    /// Range of this Scope.
    pub range: lsp_types::Range,

    /// All variables inside this scope.
    ///
    /// It's not contains variables from children scopes.
    pub variables: Vec<VariableInfo>,
}

/// Vector that stores all scopes.
#[derive(Debug, Clone)]
pub struct ScopeGraph {
    pub scopes: Vec<Scope>,
}

impl ScopeGraph {
    pub fn new(root_node: Node, source: &str) -> Self {
        let mut graph = ScopeGraph { scopes: Vec::new() };

        let root_scope = Scope {
            id: 0,
            parent_id: None,
            range: get_range_from_node(&root_node).unwrap_or_default(),
            variables: Vec::new(),
        };
        graph.scopes.push(root_scope);

        graph.index_node(root_node, 0, source);
        graph.resolve_references(root_node, 0, source);

        graph
    }

    /// Recursively indexes nodes to discover scopes and variables.
    fn index_node(&mut self, node: Node, current_scope_id: usize, source: &str) {
        let mut active_scope_id = current_scope_id;

        if Self::is_scope_creator(node.kind()) && node.kind() != "program" {
            let new_id = self.scopes.len();
            let new_scope = Scope {
                id: new_id,
                parent_id: Some(current_scope_id),
                variables: Vec::new(),
                range: get_range_from_node(&node).unwrap_or_default(),
            };
            self.scopes.push(new_scope);
            active_scope_id = new_id;
        }

        // All `typed_identifier` is defining some variable.
        if node.kind() == "typed_identifier" {
            if let (Some(id), Some(ty)) = (node.named_child(0), node.named_child(1)) {
                let name = &source[id.byte_range()];
                let ty = &source[ty.byte_range()];
                let var_info = VariableInfo {
                    name: name.to_string(),
                    ty: ty.to_string(),
                    range: get_range_from_node(&id).unwrap_or_default(),
                    references: vec![],
                };

                if let Some(scope) = self.scopes.get_mut(active_scope_id) {
                    scope.variables.push(var_info);
                }
            }
        }

        // All `assignment` also defining some variables.
        if node.kind() == "assignment" {
            if let Some(scope) = self.scopes.get_mut(active_scope_id) {
                if let (Some(pat), Some(ty)) = (node.named_child(0), node.named_child(1)) {
                    if let Some(vars) = variable::parse_assignment(source, &pat, &ty) {
                        scope.variables.extend(vars);
                    }
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            self.index_node(child, active_scope_id, source);
        }
    }

    /// Recursively collects all references to previously defined variables.
    fn resolve_references(&mut self, node: Node, current_scope_id: usize, source: &str) {
        let mut active_scope_id = current_scope_id;

        if Self::is_scope_creator(node.kind()) && node.kind() != "program" {
            if let Some(scope) = self
                .scopes
                .iter()
                .find(|s| s.range == get_range_from_node(&node).unwrap_or_default())
            {
                active_scope_id = scope.id;
            }
        }

        // All references is wrapped in `variable_expr`.
        if node.kind() == "variable_expr" {
            let name = &source[node.byte_range()];
            let range = get_range_from_node(&node).unwrap_or_default();

            self.add_reference(active_scope_id, name, range);
        }

        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            self.resolve_references(child, active_scope_id, source);
        }
    }

    /// Finds the definition of a variable and records a reference to it.
    fn add_reference(&mut self, start_scope_id: usize, name: &str, range: lsp_types::Range) {
        let mut current_id = Some(start_scope_id);

        while let Some(scope_id) = current_id {
            let scope = &mut self.scopes[scope_id];

            if let Some(var) = scope
                .variables
                .iter_mut()
                .filter(|v| (v.name == name) && (v.range.start < range.start))
                .last()
            {
                var.references.push(range);
                return;
            }
            current_id = scope.parent_id;
        }
    }

    fn is_scope_creator(kind: &str) -> bool {
        matches!(
            kind,
            "program" | "function" | "block_expression" | "match_arm"
        )
    }

    /// Retrieves all visible variables at the specified [`lsp_types::Position`].
    ///
    /// The [`is_included`] flag determines whether the variable at the exact position
    /// should be included. For example, completion uses `false` to avoid suggesting
    /// the variable currently being typed.
    pub fn get_visible_variables(
        &self,
        pos: lsp_types::Position,
        is_included: bool,
    ) -> Vec<VariableInfo> {
        let mut visible_vars = HashMap::new();

        let mut current_scope_id = self.find_deepest_scope(pos);

        while let Some(scope_id) = current_scope_id {
            let scope = &self.scopes[scope_id];

            scope.variables.iter().rev().for_each(|info| {
                if !visible_vars.contains_key(&info.name)
                    && (if is_included {
                        info.range.start
                    } else {
                        info.range.end
                    }) <= pos
                {
                    visible_vars.insert(info.name.clone(), info.clone());
                }
            });

            current_scope_id = scope.parent_id;
        }

        visible_vars.into_values().collect()
    }

    /// Finds the deepest scope that contains the given [`lsp_types::Position`].
    fn find_deepest_scope(&self, pos: lsp_types::Position) -> Option<usize> {
        self.scopes.iter().enumerate().rev().find_map(|(i, scope)| {
            if scope.range.start <= pos && scope.range.end >= pos {
                return Some(i);
            }
            None
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static SOURCE_CODE: &str = "
fn first_function(index: u32) -> u32 {
    let pair: (Asset1, Amount1) = unwrap(jet::input_amount(index));
    let (asset, amount): (Asset1, Amount1) = pair;

    let pair: u32 = 0;

    let (_, scope): (u32, u32) = {
        let ((a1, complex_type), a2): ((u32, [u8; 2]), u64) = ((0, [1,2]), 43242);
        (a1, a1)
    };

    let use_a1: u32 = a1;

    (asset, amount)
}

fn second_function() -> u32 {
    index
}
    ";

    fn return_scope_graph() -> ScopeGraph {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_simplicityhl::LANGUAGE.into())
            .unwrap();

        ScopeGraph::new(
            parser.parse(SOURCE_CODE, None).unwrap().root_node(),
            SOURCE_CODE,
        )
    }

    #[test]
    fn test_number_of_scopes() {
        let graph = return_scope_graph();

        // 6 scopes:
        // 0 - program scope
        // 1 - first_function, 2 - first_function block, 3 - nested block
        // 4 - second_function, 5 - second_function block
        assert_eq!(graph.scopes.len(), 6, "Expected exactly 6 scopes");
    }

    #[test]
    fn test_visible_variables_at_line_4_col_0() {
        let graph = return_scope_graph();
        let pos = lsp_types::Position::new(4, 0);

        let vars = graph.get_visible_variables(pos, true);

        let names: Vec<_> = vars.iter().map(|v| v.name.as_str()).collect();

        assert!(
            names.contains(&"index"),
            "index should be visible at position 4:0"
        );
        assert!(
            names.contains(&"pair"),
            "pair should be visible at position 4:0"
        );
        assert!(
            names.contains(&"asset"),
            "asset should be visible at position 4:0"
        );
        assert!(
            names.contains(&"amount"),
            "amount should be visible at position 4:0"
        );
    }

    #[test]
    fn test_pair_reference_counts() {
        let graph = return_scope_graph();

        // Position 4:0
        let pos_4_0 = lsp_types::Position::new(4, 0);
        let vars_4_0 = graph.get_visible_variables(pos_4_0, true);
        let pair_4_0 = vars_4_0
            .iter()
            .find(|v| v.name == "pair")
            .expect("pair must be visible at 4:0");

        assert_eq!(
            pair_4_0.references.len(),
            1,
            "pair should have exactly 1 reference at 4:0"
        );

        // Position 6:0
        let pos_6_0 = lsp_types::Position::new(6, 0);
        let vars_6_0 = graph.get_visible_variables(pos_6_0, true);
        let pair_6_0 = vars_6_0
            .iter()
            .find(|v| v.name == "pair")
            .expect("pair must be visible at 6:0");

        assert_eq!(
            pair_6_0.references.len(),
            0,
            "pair should have 0 references at 6:0"
        );
    }

    #[test]
    fn test_deepest_scope_detection() {
        let graph = return_scope_graph();

        // Line inside the inner block of first_function
        let pos_inner = lsp_types::Position::new(9, 8);
        let scope_id = graph.find_deepest_scope(pos_inner).unwrap();

        assert_eq!(scope_id, 3, "Expected block to have scope_id == 3");
    }

    #[test]
    fn test_no_duplicate_variables() {
        let graph = return_scope_graph();
        let pos = lsp_types::Position::new(18, 0);
        let vars = graph.get_visible_variables(pos, true);

        assert_eq!(
            vars.len(),
            0,
            "Second function shoud not contain any variables"
        );
    }

    #[test]
    fn test_references_are_after_definitions() {
        let graph = return_scope_graph();
        for scope in &graph.scopes {
            for var in &scope.variables {
                for r in &var.references {
                    assert!(
                        r.start >= var.range.start,
                        "Reference {:?} occurs before variable definition {:?}",
                        r.start,
                        var.range.start
                    );
                }
            }
        }
    }

    #[test]
    fn test_second_function_have_no_variables() {
        let graph = return_scope_graph();
        let pos = lsp_types::Position::new(4, 0);
        let vars = graph.get_visible_variables(pos, true);

        let mut set = std::collections::HashSet::new();
        for v in vars {
            assert!(set.insert(v.name.clone()), "Duplicate variable: {}", v.name);
        }
    }
}
