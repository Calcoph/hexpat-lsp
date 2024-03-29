use std::collections::HashMap;

use im_rc::Vector;
use hexparser::{m_parser::{Expr, HexTypeDef, HexType, Definition, FuncCall, Statement}, token::Spanned};

#[derive(Debug, Clone)]
pub enum ReferenceSymbol {
    Found(Spanned<String>),
    Finding(usize),
}

use ReferenceSymbol::*;

pub fn get_reference(
    ast: &Spanned<Vec<Spanned<Statement>>>,
    ident_offset: usize,
    include_self: bool,
) -> Vec<Spanned<String>> {
    let mut reference_list = vec![];
    let mut reference_symbol = ReferenceSymbol::Finding(ident_offset);
    get_reference_of_statements(&ast.0, &mut reference_symbol, &mut reference_list, include_self);
    reference_list
}

pub fn get_reference_of_expressions(
    exprs: &Vec<Spanned<Expr>>,
    reference_symbol: &mut ReferenceSymbol,
    reference_list: &mut Vec<Spanned<String>>,
    include_self: bool,
) {
    for exp in exprs {
        get_reference_of_expr(
            exp,
            reference_symbol,
            reference_list,
            include_self,
        );
    }
}

pub fn get_reference_of_statements(
    stmnts: &Vec<Spanned<Statement>>,
    reference_symbol: &mut ReferenceSymbol,
    reference_list: &mut Vec<Spanned<String>>,
    include_self: bool,
) {
    for stmnt in stmnts {
        get_reference_of_statement(
            stmnt,
            reference_symbol,
            reference_list,
            include_self,
        );
    }
}

pub fn get_reference_of_statement(
    stmnt: &Spanned<Statement>,
    reference_symbol: &mut ReferenceSymbol,
    reference_list: &mut Vec<Spanned<String>>,
    include_self: bool,
) {
    match &stmnt.0 {
        Statement::If { test, consequent } => {
            get_reference_of_expr(
                test,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_statements(
                &consequent.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        }
        Statement::IfBlock { ifs, alternative } => {
            get_reference_of_statements(
                &ifs.0,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_statements(
                &alternative.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Statement::BitFieldEntry { name: (name, name_span), length } => {
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
        Statement::Using { new_name, template_parameters, old_name } => (), // TODO
        Statement::Continue => (),
        Statement::Break => (),
        Statement::ForLoop { var_init, var_test, var_change, body } => {
            get_reference_of_statement(
                var_init,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                var_test,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_statement(
                var_change,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_statements(
                &body.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Statement::Union { name, body, template_parameters } => { // TODO: template_parameters
            get_reference_of_statements(
                &body.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Statement::ArrayDefinition { value_type, array_name, size: index, body } => {}, // TODO
        Statement::Match { parameters, branches  } => (), // TODO
        Statement::TryCatch { try_block, catch_block } => (), // TODO
        Statement::Return { value } => get_reference_of_expr(
            value,
            reference_symbol,
            reference_list,
            include_self,
        ),
        Statement::Func { name: (name, name_span), args, body } => {
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
            // TODO: args
            get_reference_of_statements(
                &body.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Statement::Struct { name: (name, name_span), body, template_parameters } => { // TODO: template_parameters
            match reference_symbol {
                Finding(ident) if *ident >= name_span.start && *ident < name_span.end => {
                    let spanned_name = (name.clone(), name_span.clone());
                    if include_self {
                        reference_list.push(spanned_name.clone());
                    }
                    dbg!(name);
                    *reference_symbol = ReferenceSymbol::Found(spanned_name)
                }
                _ => {dbg!(name);},
            }
            get_reference_of_statements(
                &body.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Statement::Namespace { name, body } => {
            get_reference_of_expr(
                name,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_statements(
                &body.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Statement::Enum { name: (name, name_span), value_type, body } => {
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
            get_reference_of_expressions(
                &body.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        }
        Statement::Bitfield { name: (name, name_span), body } => {
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
            get_reference_of_statements(
                &body.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        }
        Statement::Assignment { loperand, operator, roperand } => {
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
        },
        Statement::Error => (),
        Statement::WhileLoop { condition, body } => {
            get_reference_of_expr(
                condition,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_statements(
                &body.0,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Statement::Definition(Definition { value_type, name, body }) => {
            get_reference_of_type_def(
                value_type,
                reference_symbol,
                reference_list,
                include_self,
            );
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
        Statement::Padding { padding_body } => get_reference_of_expr(padding_body, reference_symbol, reference_list, include_self),
        Statement::Call(FuncCall { func_name, arguments }) => {
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
        },
    }
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
        Expr::Call(FuncCall { func_name, arguments }) => {
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
        Expr::Definition(Definition { value_type, name, body }) => {
            get_reference_of_type_def(
                value_type,
                reference_symbol,
                reference_list,
                include_self,
            );
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
        Expr::Unary { operation: _, operand } => get_reference_of_expr(
            operand,
            reference_symbol,
            reference_list,
            include_self,
        ),
        Expr::ExprList { list } => get_reference_of_expressions(list, reference_symbol, reference_list, include_self),
        Expr::UnnamedParameter { type_: _ } => (),
        Expr::Access { item, member } => {
            get_reference_of_expr(
                item,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                member,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Expr::Attribute { arguments } => {
            for arg in arguments.0.iter() {
                get_reference_of_expr(
                    arg,
                    reference_symbol,
                    reference_list,
                    include_self,
                );
            }
        },
        Expr::AttributeArgument { name, value } => {
            get_reference_of_expr(
                name,
                reference_symbol,
                reference_list,
                include_self,
            );

            for arg in value {
                get_reference_of_expr(
                    arg,
                    reference_symbol,
                    reference_list,
                    include_self,
                );
            }
        },
        Expr::WhileLoop { condition, body } => {
            get_reference_of_expr(
                condition,
                reference_symbol,
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                body,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Expr::Cast { cast_operator, operand } => {
            get_reference_of_expr(
                operand,
                reference_symbol,
                reference_list,
                include_self,
            );
        },
        Expr::ArrayAccess { array: item, index: member } => {}, // TODO
        Expr::Type { val } => (), // TODO
    }
}

pub fn get_reference_of_type_def(
    type_def: &Spanned<HexTypeDef>,
    reference_symbol: &mut ReferenceSymbol,
    reference_list: &mut Vec<Spanned<String>>,
    include_self: bool,
) {
    let HexTypeDef {
        endianness: _,
        name,
    } = &type_def.0;
    get_reference_of_type(name, reference_symbol, reference_list, include_self)
}

pub fn get_reference_of_type(
    type_: &Spanned<HexType>,
    reference_symbol: &mut ReferenceSymbol,
    reference_list: &mut Vec<Spanned<String>>,
    include_self: bool,
) {
    if let Found((symbol_name, symbol_span)) = reference_symbol {
        match &type_.0 {
            HexType::Custom(name) => if symbol_name == name {
                reference_list.push((name.clone(), type_.1.clone()));
            } else {
                dbg!(symbol_name);
                dbg!(name);
            },
            HexType::Path(p) => {
                let name = p.last().unwrap().clone(); // TODO: Don't ignore the rest of the path
                if symbol_name == &name {
                    reference_list.push((name.clone(), type_.1.clone()));
                }
            },
            HexType::V(_) => (),
            HexType::Null => (),
            HexType::Parameted(r#type, _) => get_reference_of_type(&(r#type.as_ref().clone(), type_.1.clone()), reference_symbol, reference_list, include_self),
        }
    }
}
