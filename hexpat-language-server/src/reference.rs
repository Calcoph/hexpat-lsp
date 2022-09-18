use std::collections::HashMap;

use im_rc::Vector;
use hexparser::{m_parser::{Expr, NamedNode}, token::Spanned};

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
    //let vector = Vector::new();
    let mut reference_list = vec![];
    let reference_symbol = ReferenceSymbol::Finding(ident_offset);
    //get_reference_of_expr(&ast.1, vector, reference_symbol, &mut reference_list, include_self); // TODO: Uncomment this
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
        Expr::Error => (),
        Expr::Value { .. } => (),
        Expr::Local { name: (name, name_span) } => {
            if let Found((symbol_name, symbol_span)) = reference_symbol {
                if &symbol_name == name {
                    let index = definition_ass_list
                        .iter()
                        .position(|decl| decl.0 == symbol_name);
                    if let Some(symbol) = index.map(|i| definition_ass_list.get(i).unwrap().clone())
                    {
                        if symbol == (symbol_name, symbol_span) {
                            reference_list.push((name.clone(), name_span.clone()));
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
        Expr::Binary { loperand, operator, roperand } => {
            get_reference_of_expr(
                loperand,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                roperand,
                definition_ass_list.clone(),
                reference_symbol,
                reference_list,
                include_self,
            );
        }
        Expr::Call { func_name, arguments } => {
            get_reference_of_expr(
                func_name,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            for expr in &arguments.0 {
                get_reference_of_expr(
                    &expr,
                    definition_ass_list.clone(),
                    reference_symbol.clone(),
                    reference_list,
                    include_self,
                );
            }
        }
        Expr::If { test, consequent } => {
            get_reference_of_expr(
                consequent,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
        }
        Expr::IfBlock { ifs, alternative } => (), // TODO
        Expr::Definition { value_type, name, body } => {
            let next_symbol = match &name.0 {
                Expr::Local { name: (name, name_span) } => {
                    match reference_symbol {
                        Finding(ident) if ident >= name_span.start && ident < name_span.end => {
                            let spanned_name = (name.clone(), name_span.clone());
                            if include_self {
                                reference_list.push(spanned_name.clone());
                            }
                            ReferenceSymbol::Found(spanned_name)
                        }
                        _ => reference_symbol,
                    }
                },
                _ => todo!()
            };

            get_reference_of_expr(
                body,
                definition_ass_list.clone(),
                next_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::BitFieldEntry { name: (name, name_span), length } => {
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
        },
        Expr::EnumEntry { name: (name, name_span), value } => {
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
        },
        Expr::Ternary { loperand, moperand, roperand } => {
            get_reference_of_expr(
                loperand,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                moperand,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                roperand,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::NamespaceAccess { previous, name: (name, name_span) } => {
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
                previous,
                definition_ass_list.clone(),
                next_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::Unary { operation, operand } => get_reference_of_expr(
            operand,
            definition_ass_list.clone(),
            reference_symbol.clone(),
            reference_list,
            include_self,
        ),
        Expr::Using { new_name, old_name } => (), // TODO
        Expr::Continue => (),
        Expr::Break => (),
        Expr::ExprList { list } => for exp in list {
            get_reference_of_expr(
                exp,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
        },
        Expr::UnnamedParameter { type_ } => (), // TODO
        Expr::Return { value } => (), // TODO
        Expr::Func { name, args, body } => (), // TODO
        Expr::Struct { name, body } => (), // TODO
        Expr::Namespace { name, body } => (), // TODO
        Expr::Enum { name, value_type, body } => (), // TODO
        Expr::Bitfield { name, body } => (), // TODO
        Expr::Access { item, member } => (), // TODO
        Expr::Attribute { arguments } => (), // TODO
        Expr::AttributeArgument { name, value } => (), // TODO
        Expr::WhileLoop { condition, body } => (), // TODO
        Expr::ForLoop { var_init, var_test, var_change, body } => (), // TODO
        Expr::Cast { cast_operator, operand } => (), // TODO
        Expr::Union { name, body } => (), // TODO
    }
}
