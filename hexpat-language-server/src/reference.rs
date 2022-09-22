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
    let mut reference_list = vec![];
    let mut reference_symbol = ReferenceSymbol::Finding(ident_offset);
    get_reference_of_expr(&ast.1, &mut reference_symbol, &mut reference_list, include_self);
    reference_list
}

pub fn get_reference_of_expr(
    expr: &Spanned<Expr>,
    reference_symbol: &mut ReferenceSymbol,
    reference_list: &mut Vec<Spanned<String>>,
    include_self: bool,
) {
    match &expr.0 {
        Expr::Error => (),
        Expr::Value { .. } => (),
        Expr::Local { name: (name, name_span) } => {
            if let Found((symbol_name, symbol_span)) = reference_symbol {
                if symbol_name == name {
                    reference_list.push((name.clone(), name_span.clone()));
                }
            }
        }
        Expr::Binary { loperand, operator: _, roperand } => {
            get_reference_of_expr(
                loperand,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                roperand,
                reference_symbol,
                reference_list,
                include_self,
            );
        }
        Expr::Call { func_name, arguments } => {
            get_reference_of_expr(
                func_name,
                reference_symbol,
                reference_list,
                include_self,
            );
            for expr in &arguments.0 {
                get_reference_of_expr(
                    &expr,
                    reference_symbol,
                    reference_list,
                    include_self,
                );
            }
        }
        Expr::If { test, consequent } => {
            get_reference_of_expr(
                test,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                consequent,
                reference_symbol,
                reference_list,
                include_self,
            );
        }
        Expr::IfBlock { ifs, alternative } => {
            get_reference_of_expr(
                ifs,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                alternative,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Expr::Definition { value_type, name, body } => {
            match &name.0 {
                Expr::Local { name: (name, name_span) } => {
                    match reference_symbol {
                        Finding(ident) if *ident >= name_span.start && *ident < name_span.end => {
                            let spanned_name = (name.clone(), name_span.clone());
                            if include_self {
                                reference_list.push(spanned_name.clone());
                            }
                            *reference_symbol = ReferenceSymbol::Found(spanned_name)
                        }
                        _ => (),
                    }
                },
                _ => todo!()
            };

            get_reference_of_expr(
                body,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Expr::BitFieldEntry { name: (name, name_span), length } => {
            match reference_symbol {
                Finding(ident) if *ident >= name_span.start && *ident < name_span.end => {
                    let spanned_name = (name.clone(), name_span.clone());
                    if include_self {
                        reference_list.push(spanned_name.clone());
                    }
                    *reference_symbol = ReferenceSymbol::Found(spanned_name)
                }
                _ => (),
            };
        },
        Expr::EnumEntry { name: (name, name_span), value } => {
            let next_symbol = match reference_symbol {
                Finding(ident) if *ident >= name_span.start && *ident < name_span.end => {
                    let spanned_name = (name.clone(), name_span.clone());
                    if include_self {
                        reference_list.push(spanned_name.clone());
                    }
                    *reference_symbol = ReferenceSymbol::Found(spanned_name)
                }
                _ => (),
            };
        },
        Expr::Ternary { loperand, moperand, roperand } => {
            get_reference_of_expr(
                loperand,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                moperand,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                roperand,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Expr::NamespaceAccess { previous, name: (name, name_span) } => {
            match reference_symbol {
                Finding(ident) if *ident >= name_span.start && *ident < name_span.end => {
                    let spanned_name = (name.clone(), name_span.clone());
                    if include_self {
                        reference_list.push(spanned_name.clone());
                    }
                    *reference_symbol = ReferenceSymbol::Found(spanned_name)
                }
                _ => (),
            };

            get_reference_of_expr(
                previous,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Expr::Unary { operation, operand } => get_reference_of_expr(
            operand,
            reference_symbol,
            reference_list,
            include_self,
        ),
        Expr::Using { new_name, old_name } => (), // TODO
        Expr::Continue => (),
        Expr::Break => (),
        Expr::ExprList { list } => for exp in list {
            get_reference_of_expr(
                exp,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Expr::UnnamedParameter { type_: _ } => (),
        Expr::Return { value } => get_reference_of_expr(
            value,
            reference_symbol,
            reference_list,
            include_self,
        ),
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
