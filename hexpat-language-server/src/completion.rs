use std::collections::HashMap;

use hexparser::{m_parser::{Expr, NamedNode}, token::Spanned};

pub enum ImCompleteCompletionItem {
    Variable(String),
    Function(String, Vec<String>),
    Struct(String),
    Enum(String),
    NameSpace(String),
    BitField(String)
}
/// return (need_to_continue_search, founded reference)
pub fn completion(
    ast: &(HashMap<String, Spanned<NamedNode>>, Spanned<Expr>),
    ident_offset: usize,
) -> HashMap<String, ImCompleteCompletionItem> {
    let mut map = HashMap::new();
    /* for (name, (v, span)) in ast.0.iter() {
        match v {
            NamedNode::Function(args) => {
                if span.end < ident_offset {
                    map.insert(
                        name.clone(),
                        ImCompleteCompletionItem::Function(
                            name.clone(),
                            args.clone().into_iter().collect(),
                        ),
                    );
                }
            },
            NamedNode::Struct => {
                if span.end < ident_offset {
                    map.insert(
                        name.clone(),
                        ImCompleteCompletionItem::Struct(
                            name.clone(),
                        ),
                    );
                }
            },
            NamedNode::Enum => {
                if span.end < ident_offset {
                    map.insert(
                        name.clone(),
                        ImCompleteCompletionItem::Enum(
                            name.clone(),
                        ),
                    );
                }
            },
            NamedNode::NameSpace => {
                if span.end < ident_offset {
                    map.insert(
                        name.clone(),
                        ImCompleteCompletionItem::NameSpace(
                            name.clone(),
                        ),
                    );
                }
            },
            NamedNode::BitField => {
                if span.end < ident_offset {
                    map.insert(
                        name.clone(),
                        ImCompleteCompletionItem::BitField(
                            name.clone(),
                        ),
                    );
                }
            },
            NamedNode::Variable => (), // TODO
        }
    }

    // collect params variable
    for (_, (v, span)) in ast.0.iter() {
        match v {
            NamedNode::Function(v) => {
                if span.end > ident_offset && span.start < ident_offset {
                    // log::debug!("this is completion from body {}", name);
                    v.iter().for_each(|item| {
                        map.insert(
                            item.clone(),
                            ImCompleteCompletionItem::Variable(item.clone()),
                        );
                    });
                }
            },
            _ => (),
        }
    }
     */ // TODO: Uncomment this
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
            match get_completion_of(loperand, definition_map, ident_offset) {
                true => get_completion_of(roperand, definition_map, ident_offset),
                false => false,
            }
        },
        Expr::Call { func_name, arguments } => {
            match get_completion_of(func_name, definition_map, ident_offset) {
                true => {}
                false => return false,
            }
            for expr in &arguments.0 {
                match get_completion_of(&expr, definition_map, ident_offset) {
                    true => continue,
                    false => return false,
                }
            }
            true
        }
        Expr::If { test, consequent } => {
            match get_completion_of(test, definition_map, ident_offset) {
                true => {}
                false => return false,
            }
            get_completion_of(consequent, definition_map, ident_offset)
        }
        Expr::IfBlock { ifs, alternative } => false, // TODO
        Expr::Definition { value_type, name, body } => {
            match &name.0 {
                Expr::Local { name } => {
                    definition_map.insert(
                        name.0.clone(),
                        ImCompleteCompletionItem::Variable(name.0.clone()),
                    );
                },
                _ => return false // TODO
            }
            get_completion_of(body, definition_map, ident_offset)
        },
        Expr::ExprList { list } => false, // TODO
        Expr::UnnamedParameter { type_ } => false, // TODO
        Expr::Unary { operation, operand } => false, // TODO
        Expr::Ternary { loperand, moperand, roperand } => false, // TODO
        Expr::BitFieldEntry { name, length } => false, // TODO
        Expr::EnumEntry { name, value } => false, // TODO
        Expr::NamespaceAccess { previous, name } => false, // TODO
        Expr::Using { new_name, old_name } => false, // TODO
        Expr::Return { value } => false, // TODO
        Expr::Continue => false, // TODO
        Expr::Break => false, // TODO
        Expr::Func { name, args, body } => false, // TODO
        Expr::Struct { name, body } => false, // TODO
        Expr::Namespace { name, body } => false, // TODO
        Expr::Enum { name, value_type, body } => false, // TODO
        Expr::Bitfield { name, body } => false, // TODO
        Expr::Access { item, member } => false, // TODO
        Expr::Attribute { arguments } => false, // TODO
        Expr::AttributeArgument { name, value } => false, // TODO
        Expr::WhileLoop { condition, body } => false, // TODO
        Expr::ForLoop { var_init, var_test, var_change, body } => false, // TODO
        Expr::Cast { cast_operator, operand } => false, // TODO
        Expr::Union { name, body } => false, // TODO
    }
}
