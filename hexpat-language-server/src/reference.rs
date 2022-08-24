use std::collections::HashMap;

use im_rc::Vector;
use hexparser::m_parser::{Spanned, Expr, NamedNode};

#[derive(Debug, Clone)]
pub enum ReferenceSymbol {
    Found(Spanned<String>),
    Finding(usize),
}

use ReferenceSymbol::*;

pub fn get_reference(
    ast: &(HashMap<String, Spanned<NamedNode>>, Spanned<Expr>),
    ident_offset: usize,
    include_self: bool,
) -> Vec<Spanned<String>> {
    let vector = Vector::new();
    let mut reference_list = vec![];
    let reference_symbol = ReferenceSymbol::Finding(ident_offset);
    get_reference_of_expr(&ast.1, vector, reference_symbol, &mut reference_list, include_self);
    reference_list
}

pub fn get_reference_of_expr(
    expr: &Spanned<Expr>,
    definition_ass_list: Vector<Spanned<String>>,
    reference_symbol: ReferenceSymbol,
    reference_list: &mut Vec<Spanned<String>>,
    include_self: bool,
) {
    match &expr.0 {
        Expr::Error => {}
        Expr::Value(_) => {}
        Expr::Local((name, span)) => {
            if let Found((symbol_name, symbol_span)) = reference_symbol {
                if &symbol_name == name {
                    let index = definition_ass_list
                        .iter()
                        .position(|decl| decl.0 == symbol_name);
                    if let Some(symbol) = index.map(|i| definition_ass_list.get(i).unwrap().clone())
                    {
                        if symbol == (symbol_name, symbol_span) {
                            reference_list.push((name.clone(), span.clone()));
                        }
                    };
                }
            }
            // if ident_offset >= local.1.start && ident_offset < local.1.end {
            //     let index = definition_ass_list
            //         .iter()
            //         .position(|decl| decl.0 == local.0);
            //     (
            //         false,
            //         index.map(|i| definition_ass_list.get(i).unwrap().clone()),
            //     )
            // } else {
            //     (true, None)
            // }
        }
        Expr::Binary(lhs, _op, rhs) => {
            get_reference_of_expr(
                lhs,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                rhs,
                definition_ass_list.clone(),
                reference_symbol,
                reference_list,
                include_self,
            );
        }
        Expr::Call(callee, args) => {
            get_reference_of_expr(
                callee,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            for expr in &args.0 {
                get_reference_of_expr(
                    &expr,
                    definition_ass_list.clone(),
                    reference_symbol.clone(),
                    reference_list,
                    include_self,
                );
            }
        }
        Expr::If(_test, consequent, alternative) => {
            get_reference_of_expr(
                consequent,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                alternative,
                definition_ass_list,
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
        }
        Expr::Definition(_, (name, name_span), lhs) => {
            let next_symbol = match reference_symbol {
                Finding(ident) if ident >= name_span.start && ident < name_span.end => {
                    let spanned_name = (name.clone(), name_span.clone());
                    if include_self {
                        reference_list.push(spanned_name.clone());
                    }
                    ReferenceSymbol::Found(spanned_name)
                }
                _ => reference_symbol,
            };

            get_reference_of_expr(
                lhs,
                definition_ass_list.clone(),
                next_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::BitFieldEntry((name, name_span), _length, next_entries) => {
            let next_symbol = match reference_symbol {
                Finding(ident) if ident >= name_span.start && ident < name_span.end => {
                    let spanned_name = (name.clone(), name_span.clone());
                    if include_self {
                        reference_list.push(spanned_name.clone());
                    }
                    ReferenceSymbol::Found(spanned_name)
                }
                _ => reference_symbol,
            };

            get_reference_of_expr(
                next_entries,
                definition_ass_list.clone(),
                next_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::EnumEntry((name, name_span), _length, next_entries) => {
            let next_symbol = match reference_symbol {
                Finding(ident) if ident >= name_span.start && ident < name_span.end => {
                    let spanned_name = (name.clone(), name_span.clone());
                    if include_self {
                        reference_list.push(spanned_name.clone());
                    }
                    ReferenceSymbol::Found(spanned_name)
                }
                _ => reference_symbol,
            };

            get_reference_of_expr(
                next_entries,
                definition_ass_list.clone(),
                next_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::MemberAccess(previous_entries, (name, name_span)) => {
            let next_symbol = match reference_symbol {
                Finding(ident) if ident >= name_span.start && ident < name_span.end => {
                    let spanned_name = (name.clone(), name_span.clone());
                    if include_self {
                        reference_list.push(spanned_name.clone());
                    }
                    ReferenceSymbol::Found(spanned_name)
                }
                _ => reference_symbol,
            };

            get_reference_of_expr(
                previous_entries,
                definition_ass_list.clone(),
                next_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::ArrayAccess(name, value) => {
            get_reference_of_expr(
                name,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                value,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::Ternary(exp1, exp2, exp3) => {
            get_reference_of_expr(
                exp1,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                exp2,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                exp3,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::NamespaceAccess(previous_entries, (name, name_span)) => {
            let next_symbol = match reference_symbol {
                Finding(ident) if ident >= name_span.start && ident < name_span.end => {
                    let spanned_name = (name.clone(), name_span.clone());
                    if include_self {
                        reference_list.push(spanned_name.clone());
                    }
                    ReferenceSymbol::Found(spanned_name)
                }
                _ => reference_symbol,
            };

            get_reference_of_expr(
                previous_entries,
                definition_ass_list.clone(),
                next_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::Dollar => (),
        Expr::Unary(_, exp) => get_reference_of_expr(
            exp,
            definition_ass_list.clone(),
            reference_symbol.clone(),
            reference_list,
            include_self,
        ),
        Expr::Using(new_name) => {
            get_reference_of_expr(
                new_name,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::Continue => (),
        Expr::Break => (),
        Expr::ExprList(exps) => for exp in exps.as_ref() {
            get_reference_of_expr(
                exp,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::Func(_, _, _) => (), // TODO
        Expr::Struct(_, _) => (), // TODO
        Expr::Namespace(_, _) => (), // TODO
        Expr::Enum(_, _, _) => (), // TODO
        Expr::Bitfield(_, _) => (), // TODO
        Expr::Return(_) => (), // TODO
    }
}
