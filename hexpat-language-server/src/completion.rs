use std::collections::HashMap;

use hexparser::{m_parser::Expr, token::Spanned};
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
/// return (need_to_continue_search, founded reference)
pub fn completion(
    ast: &Spanned<Expr>,
    ident_offset: usize,
) -> HashMap<String, ImCompleteCompletionItem> {
    let mut map = HashMap::new();
    get_completion_of(ast, &mut map, ident_offset);
    map
}

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
        Expr::Call { func_name, arguments } => {
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
        Expr::If { test, consequent } => {
            if !get_completion_of(test, definition_map, ident_offset) {
                return false
            }

            get_completion_of(consequent, definition_map, ident_offset)
        }
        Expr::IfBlock { ifs, alternative } => {
            if !get_completion_of(ifs, definition_map, ident_offset) {
                return false;
            }

            get_completion_of(alternative, definition_map, ident_offset)
        }, // TODO
        Expr::Definition { value_type: _, name, body } => {
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
        Expr::BitFieldEntry { name, length: _ } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Variable(name.0.clone())
            );

            true
        }, // TODO
        Expr::EnumEntry { name, value } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::EnumMember(name.0.clone())
            );

            true
        }, // TODO
        Expr::NamespaceAccess { previous, name } => true, // TODO
        Expr::Using { new_name, template_parameters, old_name } => {
            definition_map.insert(
                new_name.0.clone(),
                ImCompleteCompletionItem::Variable(new_name.0.clone())
            );

            true
        }, // TODO
        Expr::Return { value } => true, // TODO
        Expr::Continue => true, // TODO
        Expr::Break => true, // TODO
        Expr::Func { name, args, body } => {
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
                get_completion_of(body, definition_map, ident_offset);
            }

            true
        }, // TODO
        Expr::Struct { name, body, template_parameters } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Struct(name.0.clone())
            );
            get_completion_of(body, definition_map, ident_offset)
        }, // TODO
        Expr::Namespace { name, body } => {
            if !get_completion_of(name, definition_map, ident_offset) {
                return false;
            }

            let mut namespace_hm = HashMap::new();

            let ret = get_completion_of(body, &mut namespace_hm, ident_offset);

            definition_map.extend(
                namespace_hm.into_iter().map(|(v, item)| (v, ImCompleteCompletionItem::NamespacedItem {
                        namespace: namespace_to_string(name),
                        item: Box::new(item)
                    })
                )
            );
            ret
        }, // TODO
        Expr::Enum { name, value_type, body } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Enum(name.0.clone())
            );

            get_completion_of(body, definition_map, ident_offset)
        }, // TODO
        Expr::Bitfield { name, body } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::BitField(name.0.clone())
            );

            get_completion_of(body, definition_map, ident_offset)
        }, // TODO
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
        Expr::ForLoop { var_init, var_test, var_change, body } => {
            if !get_completion_of(var_init, definition_map, ident_offset) {
                return false;
            }

            get_completion_of(body, definition_map, ident_offset)
        },
        Expr::Cast { cast_operator, operand } => true, // TODO
        Expr::Union { name, body, template_parameters } => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Struct(name.0.clone())
            );

            get_completion_of(body, definition_map, ident_offset)
        }, // TODO
        Expr::ArrayAccess { array: item, index: member } => true, // TODO
        Expr::ArrayDefinition { value_type, array_name, size, body } => true,
        Expr::Type { val } => true, // TODO
    }
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
