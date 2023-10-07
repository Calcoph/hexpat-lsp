use std::collections::HashMap;

use hexparser::{m_parser::{Expr, MatchBranch, MatchCaseItem, MatchCaseElement, Statement, FuncCall, Definition}, token::Spanned};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat};

use crate::namespace_handling::namespace_to_string;

pub enum ImCompleteCompletionItem {
    Variable(String),
    Function(String, Vec<String>),
    Struct(String),
    Enum(String),
    EnumMember(String),
    NameSpace(String),
    NamespacedItem{namespace: String, item: Box<Self>},
    BitField(String)
}
pub fn completion(
    ast: &Spanned<Vec<Spanned<Statement>>>,
    ident_offset: usize,
) -> HashMap<String, ImCompleteCompletionItem> {
    let mut map = HashMap::new();
    get_completion_of_statements(&ast.0, &mut map, ident_offset);
    map
}

pub fn get_completion_of_expressions(
    exprs: &Vec<Spanned<Expr>>,
    definition_map: &mut HashMap<String, ImCompleteCompletionItem>,
    ident_offset: usize,
) -> bool {
    for expr in exprs {
        if !get_completion_of(expr, definition_map, ident_offset) {
            return false
        }
    }
    true
}

pub fn get_completion_of_statements(
    stmnts: &Vec<Spanned<Statement>>,
    definition_map: &mut HashMap<String, ImCompleteCompletionItem>,
    ident_offset: usize,
) -> bool {
    for stmnt in stmnts {
        if !get_completion_of_statement(stmnt, definition_map, ident_offset) {
            return false
        }
    }
    true
}

pub fn get_completion_of_statement(
    stmnt: &Spanned<Statement>,
    definition_map: &mut HashMap<String, ImCompleteCompletionItem>,
    ident_offset: usize,
) -> bool {
    match &stmnt.0 {
        Statement::Using { new_name, template_parameters, old_name } => {
            definition_map.insert(
                new_name.0.clone(),
                ImCompleteCompletionItem::Variable(new_name.0.clone())
            );

            true
        }, // TODO
        Statement::Return { value } => true, // TODO
        Statement::Continue => true, // TODO
        Statement::Break => true, // TODO
        Statement::Func { name, args, body } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Function(name.0.clone(), Vec::new())
            );

            for arg in &args.0 {
                match &arg.0 {
                    hexparser::m_parser::FuncArgument::Parameter(param) => {
                        if !get_completion_of(param, definition_map, ident_offset) {
                            return false;
                        }
                    },
                    hexparser::m_parser::FuncArgument::ParameterPack(param) => {
                        definition_map.insert(
                            param.0.clone(),
                            ImCompleteCompletionItem::Variable(param.0.clone())
                        );
                    },
                }
            }

            if ident_offset < body.1.end {
                get_completion_of_statements(&body.0, definition_map, ident_offset);
            }

            true
        }, // TODO
        Statement::Struct { name, body, template_parameters } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Struct(name.0.clone())
            );
            get_completion_of_statements(&body.0, definition_map, ident_offset)
        }, // TODO
        Statement::Namespace { name, body } => {
            if !get_completion_of(name, definition_map, ident_offset) {
                return false;
            }

            let mut namespace_hm = HashMap::new();

            let ret = get_completion_of_statements(&body.0, &mut namespace_hm, ident_offset);

            definition_map.extend(
                namespace_hm.into_iter().map(|(v, item)| (v, ImCompleteCompletionItem::NamespacedItem {
                        namespace: namespace_to_string(name),
                        item: Box::new(item)
                    })
                )
            );
            ret
        }, // TODO
        Statement::Enum { name, value_type, body } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Enum(name.0.clone())
            );

            get_completion_of_expressions(&body.0, definition_map, ident_offset)
        }, // TODO
        Statement::Bitfield { name, body } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::BitField(name.0.clone())
            );

            get_completion_of_statements(&body.0, definition_map, ident_offset)
        }, // TODO
        Statement::If { test, consequent } => {
            if !get_completion_of(test, definition_map, ident_offset) {
                return false
            }

            get_completion_of_statements(&consequent.0, definition_map, ident_offset)
        }
        Statement::IfBlock { ifs, alternative } => {
            if !get_completion_of_statements(&ifs.0, definition_map, ident_offset) {
                return false;
            }

            get_completion_of_statements(&alternative.0, definition_map, ident_offset)
        }, // TODO
        Statement::BitFieldEntry { name, length: _ } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Variable(name.0.clone())
            );

            true
        }, // TODO
        Statement::ForLoop { var_init, var_test, var_change, body } => {
            if !get_completion_of_statement(var_init, definition_map, ident_offset) {
                return false;
            }

            get_completion_of_statements(&body.0, definition_map, ident_offset)
        },
        Statement::Union { name, body, template_parameters } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Struct(name.0.clone())
            );

            get_completion_of_statements(&body.0, definition_map, ident_offset)
        }, // TODO
        Statement::ArrayDefinition { value_type, array_name, size, body } => true,
        Statement::Match { parameters, branches  } => {
            for parameter in parameters {
                if !get_completion_of(parameter, definition_map, ident_offset) {
                    return false;
                }
            }

            for branch in branches {
                if !get_completion_of_match_branch(branch, definition_map, ident_offset) {
                    return false;
                }
            }

            true
        },
        Statement::TryCatch { try_block, catch_block } => {
            if !get_completion_of_statements(&try_block.0, definition_map, ident_offset) {
                return false;
            }
            get_completion_of_statements(&catch_block.0, definition_map, ident_offset)
        },
        Statement::Assignment { loperand, operator, roperand } => {
            /*
            if !get_completion_of(loperand, definition_map, ident_offset) {
                return false;
            }

            get_completion_of(roperand, definition_map, ident_offset)
            */

            true
        },
        Statement::Error => true,
        Statement::WhileLoop { condition, body } => {
            if ident_offset < stmnt.1.end {
                // Inside the while loop body
                get_completion_of_statements(&body.0, definition_map, ident_offset)
            } else {
                true
            }
        },
        Statement::Definition(Definition { value_type: _, name, body }) => {
            match &name.0 {
                Expr::Local { name } => {
                    definition_map.insert(
                        name.0.clone(),
                        ImCompleteCompletionItem::Variable(name.0.clone()),
                    );
                },
                _ => return true // TODO
            }
            get_completion_of(body, definition_map, ident_offset)
        },
        Statement::Padding { padding_body } => get_completion_of(padding_body, definition_map, ident_offset),
        Statement::Call(FuncCall { func_name, arguments }) => {
            if !get_completion_of(func_name, definition_map, ident_offset) {
                return false
            }
            for expr in &arguments.0 {
                if !get_completion_of(&expr, definition_map, ident_offset) {
                    return false
                }
            }
            true
        },
    }
}

/// return need_to_continue_search
pub fn get_completion_of(
    expr: &Spanned<Expr>,
    definition_map: &mut HashMap<String, ImCompleteCompletionItem>,
    ident_offset: usize,
) -> bool {
    match &expr.0 {
        Expr::Error => true,
        Expr::Value { .. } => true,
        // Expr::List(exprs) => exprs
        //     .iter()
        //     .for_each(|expr| get_definition(expr, definition_ass_list)),
        Expr::Local { name } => {
            if ident_offset >= name.1.start && ident_offset < name.1.end {
                false
            } else {
                true
            }
        }
        Expr::Binary { loperand, operator: _, roperand } => {
            /*
            if !get_completion_of(loperand, definition_map, ident_offset) {
                return false;
            }

            get_completion_of(roperand, definition_map, ident_offset)
            */

            true
        },
        Expr::Call(FuncCall { func_name, arguments }) => {
            if !get_completion_of(func_name, definition_map, ident_offset) {
                return false
            }
            for expr in &arguments.0 {
                if !get_completion_of(&expr, definition_map, ident_offset) {
                    return false
                }
            }
            true
        }
        Expr::Definition(Definition { value_type: _, name, body }) => {
            match &name.0 {
                Expr::Local { name } => {
                    definition_map.insert(
                        name.0.clone(),
                        ImCompleteCompletionItem::Variable(name.0.clone()),
                    );
                },
                _ => return true // TODO
            }
            get_completion_of(body, definition_map, ident_offset)
        },
        Expr::ExprList { list } => {
            for expr in list {
                if !get_completion_of(expr, definition_map, ident_offset) {
                    return false
                }
            }
            true
        }, // TODO
        Expr::UnnamedParameter { type_: _ } => true, // TODO
        Expr::Unary { operation: _, operand: _ } => true, // TODO
        Expr::Ternary { loperand: _, moperand: _, roperand: _ } => true, // TODO
        Expr::EnumEntry { name, value } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::EnumMember(name.0.clone())
            );

            true
        }, // TODO
        Expr::NamespaceAccess { previous, name } => true, // TODO
        Expr::Access { item, member } => true, // TODO
        Expr::Attribute { arguments } => true, // TODO
        Expr::AttributeArgument { name, value } => true, // TODO
        Expr::WhileLoop { condition, body } => {
            if ident_offset < expr.1.end {
                // Inside the while loop body
                get_completion_of(&body, definition_map, ident_offset)
            } else {
                true
            }
        },
        Expr::Cast { cast_operator, operand } => true, // TODO
        Expr::ArrayAccess { array: item, index: member } => true, // TODO
        Expr::Type { val } => true, // TODO
    }
}

fn get_completion_of_match_branch(branch: &MatchBranch, definition_map: &mut HashMap<String, ImCompleteCompletionItem>, ident_offset: usize) -> bool {
    let MatchBranch {
        case,
        body
    } = branch;

    for (case, _) in case.0.case.iter() {
        match case {
            MatchCaseItem::Anything => (),
            MatchCaseItem::Element(element) => {
                if !get_completion_of_match_case_element(element, definition_map, ident_offset) {
                    return false
                }
            },
            MatchCaseItem::OrList(elements) => {
                for element in elements {
                    if !get_completion_of_match_case_element(element, definition_map, ident_offset) {
                        return false
                    }
                }
            },
        };
    }

    get_completion_of_statements(&body.0, definition_map, ident_offset)
}

fn get_completion_of_match_case_element(element: &MatchCaseElement, definition_map: &mut HashMap<String, ImCompleteCompletionItem>, ident_offset: usize) -> bool {
    true // TODO
}

pub fn add_completion(item: ImCompleteCompletionItem, ret: &mut Vec<CompletionItem>, namespace: Option<String>) {
    use ImCompleteCompletionItem as ImComp;
    match item {
        ImComp::Variable(var) => {
            let label = match namespace {
                Some(namespace) => format!("{namespace}::{}",var.clone()),
                None => var.clone(),
            };

            ret.push(CompletionItem {
                label: label.clone(),
                insert_text: Some(label.clone()),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some(label),
                ..Default::default()
            });
        },
        ImComp::Function(name,args) => {
            let label = match namespace {
                Some(namespace) => format!("{namespace}::{}",name.clone()),
                None => name.clone(),
            };

            ret.push(CompletionItem {
                label: label.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(label.clone()),
                insert_text: Some(format!(
                    "{}({})",
                    label,
                    args.iter()
                        .enumerate()
                        .map(|(index, item)| { format!("${{{}:{}}}", index + 1, item) })
                        .collect::<Vec<_>>()
                        .join(",")
                )),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        },
        ImComp::Struct(name) => {
            let label = match namespace {
                Some(namespace) => format!("{namespace}::{}",name.clone()),
                None => name.clone(),
            };

            ret.push(CompletionItem {
                label: label.clone(),
                insert_text: Some(label.clone()),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some(label),
                ..Default::default()
            });
        },
        ImComp::BitField(name) => {
            let label = match namespace {
                Some(namespace) => format!("{namespace}::{}",name.clone()),
                None => name.clone(),
            };

            ret.push(CompletionItem {
                label: label.clone(),
                insert_text: Some(label.clone()),
                kind: Some(CompletionItemKind::STRUCT),
                detail: Some(label),
                ..Default::default()
            });
        },
        ImComp::NameSpace(name) => {
            let label = match namespace {
                Some(namespace) => format!("{namespace}::{}",name.clone()),
                None => name.clone(),
            };

            ret.push(CompletionItem {
                label: label.clone(),
                insert_text: Some(label.clone()),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some(label),
                ..Default::default()
            });
        },
        ImComp::Enum(name) => {
            let label = match namespace {
                Some(namespace) => format!("{namespace}::{}",name.clone()),
                None => name.clone(),
            };

            ret.push(CompletionItem {
                label: label.clone(),
                insert_text: Some(label.clone()),
                kind: Some(CompletionItemKind::ENUM),
                detail: Some(label),
                ..Default::default()
            });
        },
        ImComp::EnumMember(member) => {
            let label = match namespace {
                Some(namespace) => format!("{namespace}::{}",member.clone()),
                None => member.clone(),
            };

            ret.push(CompletionItem {
                label: label.clone(),
                insert_text: Some(label.clone()),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some(label),
                ..Default::default()
            });
        }
        ImComp::NamespacedItem { namespace: nmspc, item } => {
            let new_namespace = match namespace {
                Some(namespace) => format!("{}::{}", namespace, nmspc),
                None => nmspc,
            };

            add_completion(*item, ret, Some(new_namespace))
        },
    }
}
