use std::collections::HashMap;

use im_rc::Vector;

use hexparser::m_parser::{Spanned, Expr, NamedASTNode, NormalASTNode};

/// return (need_to_continue_search, founded reference)
pub fn get_definition(ast: &(HashMap<String, NamedASTNode>, Vec<NormalASTNode>), ident_offset: usize) -> Option<Spanned<String>> {
    let mut vector = Vector::new();
    for (_, v) in ast.0.iter() {
        match v {
            NamedASTNode::Expr(_, (name, span), _) => {
                if span.end < ident_offset {
                    vector.push_back((name.clone(), span.clone()));
                }
            },
            v => if v.getname().1.end < ident_offset {
                vector.push_back(v.getname().clone());
            }
        }
    }

    for (_, v) in ast.0.iter() {
        if let NamedASTNode::Func(v) = v {
            let args = v.args.iter().map(|arg| arg.clone()).collect::<Vector<_>>();
            match get_definition_of_expr(&v.body, args + vector.clone(), ident_offset) {
                (_, Some(value)) => {
                    return Some(value);
                }
                _ => {}
            }
        }
    }
    None
}

pub fn get_definition_of_expr(
    expr: &Spanned<Expr>,
    definition_ass_list: Vector<Spanned<String>>,
    ident_offset: usize,
) -> (bool, Option<Spanned<String>>) {
    match &expr.0 {
        Expr::Error => (true, None),
        Expr::Value(_) => (true, None),
        // Expr::List(exprs) => exprs
        //     .iter()
        //     .for_each(|expr| get_definition(expr, definition_ass_list)),
        Expr::Local(local) => {
            if ident_offset >= local.1.start && ident_offset < local.1.end {
                let index = definition_ass_list
                    .iter()
                    .position(|decl| decl.0 == local.0);
                (
                    false,
                    index.map(|i| definition_ass_list.get(i).unwrap().clone()),
                )
            } else {
                (true, None)
            }
        }
        Expr::Then(first, second) => {
            match get_definition_of_expr(first, definition_ass_list.clone(), ident_offset) {
                (true, None) => {
                    get_definition_of_expr(second, definition_ass_list.clone(), ident_offset)
                }
                (false, None) => (false, None),
                (true, Some(value)) | (false, Some(value)) => (false, Some(value)),
            }
        }
        Expr::Binary(lhs, _op, rhs) => {
            match get_definition_of_expr(lhs, definition_ass_list.clone(), ident_offset) {
                (true, None) => {
                    get_definition_of_expr(rhs, definition_ass_list.clone(), ident_offset)
                }
                (false, None) => (false, None),
                (true, Some(value)) | (false, Some(value)) => (false, Some(value)),
            }
        }
        Expr::Call(callee, args) => {
            match get_definition_of_expr(callee, definition_ass_list.clone(), ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            for expr in &args.0 {
                match get_definition_of_expr(&expr, definition_ass_list.clone(), ident_offset) {
                    (true, None) => continue,
                    (true, Some(value)) => return (false, Some(value)),
                    (false, None) => return (false, None),
                    (false, Some(value)) => return (false, Some(value)),
                }
            }
            (true, None)
        }
        Expr::If(test, consequent, alternative) => {
            match get_definition_of_expr(test, definition_ass_list.clone(), ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(consequent, definition_ass_list.clone(), ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(alternative, definition_ass_list, ident_offset) {
                (true, None) => return (true, None),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
        }
        Expr::Definition(_, (name, name_span), lhs) => {
            let new_decl = Vector::unit((name.clone(), name_span.clone()));

            match lhs {
                Some(lhs) => get_definition_of_expr(lhs, definition_ass_list.clone(), ident_offset),
                None => {
                    if ident_offset >= name_span.start && ident_offset < name_span.end {
                        let index = definition_ass_list
                            .iter()
                            .position(|decl| decl.0 == *name);
                        (
                            false,
                            index.map(|i| definition_ass_list.get(i).unwrap().clone()),
                        )
                    } else {
                        (true, None)
                    }
                },
            }
        },
    }
}
