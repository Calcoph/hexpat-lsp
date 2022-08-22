use std::collections::HashMap;

use hexparser::m_parser::{Spanned, Expr, NormalASTNode, NamedASTNode};

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
    ast: &(HashMap<String, NamedASTNode>, Vec<NormalASTNode>),
    ident_offset: usize,
) -> HashMap<String, ImCompleteCompletionItem> {
    let mut map = HashMap::new();
    for (_, v) in ast.0.iter() {
        match v {
            NamedASTNode::Func(v) => {
                if v.name.1.end < ident_offset {
                    map.insert(
                        v.name.0.clone(),
                        ImCompleteCompletionItem::Function(
                            v.name.0.clone(),
                            v.args.clone().into_iter().map(|(name, _)| name).collect(),
                        ),
                    );
                }
            },
            NamedASTNode::Expr(_) => (), // TODO: expr completion todo!()
            NamedASTNode::Struct(v) => {
                if v.name.1.end < ident_offset {
                    map.insert(
                        v.name.0.clone(),
                        ImCompleteCompletionItem::Struct(
                            v.name.0.clone(),
                        ),
                    );
                }
            },
            NamedASTNode::Enum(v) => {
                if v.name.1.end < ident_offset {
                    map.insert(
                        v.name.0.clone(),
                        ImCompleteCompletionItem::Enum(
                            v.name.0.clone(),
                        ),
                    );
                }
            },
            NamedASTNode::Namespace(v) => {
                if v.name.1.end < ident_offset {
                    map.insert(
                        v.name.0.clone(),
                        ImCompleteCompletionItem::NameSpace(
                            v.name.0.clone(),
                        ),
                    );
                }
            },
            NamedASTNode::Bitfield(v) => {
                if v.name.1.end < ident_offset {
                    map.insert(
                        v.name.0.clone(),
                        ImCompleteCompletionItem::BitField(
                            v.name.0.clone(),
                        ),
                    );
                }
            },
        }
    }

    // collect params variable
    for (_, v) in ast.0.iter() {
        match v {
            NamedASTNode::Func(v) => {
                if v.span.end > ident_offset && v.span.start < ident_offset {
                    // log::debug!("this is completion from body {}", name);
                    v.args.iter().for_each(|(item, _)| {
                        map.insert(
                            item.clone(),
                            ImCompleteCompletionItem::Variable(item.clone()),
                        );
                    });
                    get_completion_of(&v.body, &mut map, ident_offset);
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
        Expr::Then(first, second) => {
            match get_completion_of(first, definition_map, ident_offset) {
                true => get_completion_of(second, definition_map, ident_offset),
                false => false,
            }
        },
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
        Expr::BitFieldEntry(_, _, _) => false, // TODO
        Expr::EnumEntry(_, _, _) => false, // TODO
        Expr::MemberAccess(_, _) => false, // TODO
        Expr::ArrayAccess(_, _) => false, // TODO
        Expr::Ternary(_, _, _) => false, // TODO
        Expr::NamespaceAccess(_, _) => false, // TODO
        Expr::Dollar => false, // TODO
        Expr::Unary(_, _) => false, // TODO
    }
}
