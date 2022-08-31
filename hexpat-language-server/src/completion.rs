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
    for (name, (v, span)) in ast.0.iter() {
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
    map
}

pub fn get_completion_of(
    expr: &Spanned<Expr>,
    definition_map: &mut HashMap<String, ImCompleteCompletionItem>,
    ident_offset: usize,
) -> bool {
    match &expr.0 {
        Expr::Error => true,
        Expr::Value(_) => true,
        // Expr::List(exprs) => exprs
        //     .iter()
        //     .for_each(|expr| get_definition(expr, definition_ass_list)),
        Expr::Local(local) => {
            if ident_offset >= local.1.start && ident_offset < local.1.end {
                false
            } else {
                true
            }
        }
        Expr::Binary(lhs, _op, rhs) => {
            match get_completion_of(lhs, definition_map, ident_offset) {
                true => get_completion_of(rhs, definition_map, ident_offset),
                false => false,
            }
        },
        Expr::Call(callee, args) => {
            match get_completion_of(callee, definition_map, ident_offset) {
                true => {}
                false => return false,
            }
            for expr in &args.0 {
                match get_completion_of(&expr, definition_map, ident_offset) {
                    true => continue,
                    false => return false,
                }
            }
            true
        }
        Expr::If(test, consequent, alternative) => {
            match get_completion_of(test, definition_map, ident_offset) {
                true => {}
                false => return false,
            }
            match get_completion_of(consequent, definition_map, ident_offset) {
                true => {}
                false => return false,
            }
            get_completion_of(alternative, definition_map, ident_offset)
        }
        Expr::Definition(_, name, lhs) => {
            definition_map.insert(
                name.0.clone(),
                ImCompleteCompletionItem::Variable(name.0.clone()),
            );
            get_completion_of(lhs, definition_map, ident_offset)
        },
        Expr::BitFieldEntry(_, _) => false, // TODO
        Expr::EnumEntry(_, _) => false, // TODO
        Expr::Ternary(_, _, _) => false, // TODO
        Expr::NamespaceAccess(_, _) => false, // TODO
        Expr::Dollar => false, // TODO
        Expr::Unary(_, _) => false, // TODO
        Expr::Using(_) => false, // TODO
        Expr::Continue => false, // TODO
        Expr::Break => false, // TODO
        Expr::ExprList(_) => false, // TODO
        Expr::Func(_, _, _) => false, // TODO
        Expr::Struct(_, _) => false, // TODO
        Expr::Namespace(_, _) => false, // TODO
        Expr::Enum(_, _, _) => false, // TODO
        Expr::Bitfield(_, _) => false, // TODO
        Expr::Return(_) => false, // TODO
        Expr::Access(_, _) => false, // TODO
    }
}
