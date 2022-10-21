use std::collections::HashMap;

use im_rc::Vector;
use hexparser::{m_parser::{Expr, HexTypeDef, HexType, FuncArgument}, token::Spanned};

pub fn get_definition(ast: &Spanned<Expr>, ident_offset: usize) -> Option<Spanned<String>> {
    let mut definition_ass_list = Vector::new();
    let (_, name) = get_definition_of_expr(&ast, &mut definition_ass_list, ident_offset, false);
    name
}

/// return (need_to_continue_search, found reference)
fn get_definition_of_expr(
    expr: &Spanned<Expr>,
    definition_ass_list: &mut Vector<Spanned<String>>,
    ident_offset: usize,
    is_defining: bool
) -> (bool, Option<Spanned<String>>) {
    if ident_offset < expr.1.start {
        return (true, None)
    };
    match &expr.0 {
        Expr::Error => (true, None),
        Expr::Value { .. } => (true, None),
        Expr::Local { name } => {
            if is_defining {
                definition_ass_list.push_back(name.clone())
            }
            if ident_offset >= name.1.start && ident_offset <= name.1.end {
                let index = definition_ass_list
                    .iter()
                    .position(|decl| decl.0 == name.0);
                (
                    false,
                    index.map(|i| definition_ass_list.get(i).unwrap().clone()),
                )
            } else {
                (true, None)
            }
        }
        Expr::Binary { loperand, operator: _, roperand } => {
            match get_definition_of_expr(loperand, definition_ass_list, ident_offset, false) {
                (true, None) => {
                    get_definition_of_expr(roperand, definition_ass_list, ident_offset, false)
                }
                (false, None) => (false, None),
                (true, Some(value)) | (false, Some(value)) => (false, Some(value)),
            }
        }
        Expr::Call { func_name, arguments } => {
            match get_definition_of_expr(func_name, definition_ass_list, ident_offset, false) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            for expr in &arguments.0 {
                match get_definition_of_expr(&expr, definition_ass_list, ident_offset, false) {
                    (true, None) => continue,
                    (true, Some(value)) => return (false, Some(value)),
                    (false, None) => return (false, None),
                    (false, Some(value)) => return (false, Some(value)),
                }
            }
            (true, None)
        }
        Expr::If { test, consequent } => {
            match get_definition_of_expr(test, definition_ass_list, ident_offset, false) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(consequent, &mut definition_ass_list.clone(), ident_offset, false)
        }
        Expr::IfBlock { ifs, alternative } => {
            match get_definition_of_expr(ifs, definition_ass_list, ident_offset, false) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(alternative, &mut definition_ass_list.clone(), ident_offset, false)
        },
        Expr::Definition { value_type, name, body } => {
            match get_definition_of_type_def(value_type, definition_ass_list, ident_offset) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(name, definition_ass_list, ident_offset, true) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(body, definition_ass_list, ident_offset, false)
        },
        Expr::ExprList { list } => {
            for expr in list.iter() {
                match get_definition_of_expr(&expr, definition_ass_list, ident_offset, false) {
                    (true, None) => continue,
                    (true, Some(value)) => return (false, Some(value)),
                    (false, None) => return (false, None),
                    (false, Some(value)) => return (false, Some(value)),
                }
            }
            (true, None)
        },
        Expr::UnnamedParameter { type_ } => get_definition_of_type(type_, definition_ass_list, ident_offset),
        Expr::Unary { operation: _, operand } => get_definition_of_expr(operand, definition_ass_list, ident_offset, false),
        Expr::Ternary { loperand, moperand, roperand } => {
            match get_definition_of_expr(loperand, definition_ass_list, ident_offset, false) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(moperand, definition_ass_list, ident_offset, false) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(roperand, definition_ass_list, ident_offset, false)
        },
        Expr::BitFieldEntry { name, length: _ } => (true, None), // TODO: Add name to the definition list
        Expr::EnumEntry { name, value } => get_definition_of_expr(value, definition_ass_list, ident_offset, false), // TODO: Add name to the definition list
        Expr::NamespaceAccess { previous, name } => {
            if is_defining {
                definition_ass_list.push_back(name.clone()) // TODO: Don't ignore previous
            }

            if ident_offset >= name.1.start && ident_offset <= name.1.end {
                let index = definition_ass_list
                    .iter()
                    .position(|decl| decl.0 == name.0);
                (
                    false,
                    index.map(|i| definition_ass_list.get(i).unwrap().clone()),
                )
            } else {
                get_definition_of_expr(previous, definition_ass_list, ident_offset, false)
            }
        },
        Expr::Using { new_name, old_name } => {
            definition_ass_list.push_back(new_name.clone());
            get_definition_of_type_def(old_name, definition_ass_list, ident_offset)
        },
        Expr::Return { value } => get_definition_of_expr(value, definition_ass_list, ident_offset, false),
        Expr::Continue => (true, None),
        Expr::Break => (true, None),
        Expr::Func { name, args, body } => {
            definition_ass_list.push_back(name.clone());
            let mut new_scope = definition_ass_list.clone();
            for arg in &args.0 {
                match arg.0 {
                    FuncArgument::Parameter(ref arg) => match get_definition_of_expr(arg, &mut new_scope, ident_offset, true) {
                        (true, None) => continue,
                        (true, Some(value)) => return (false, Some(value)),
                        (false, None) => return (false, None),
                        (false, Some(value)) => return (false, Some(value)),
                    },
                    FuncArgument::ParameterPack(_) => (),
                }
            }
            get_definition_of_expr(body, &mut new_scope, ident_offset, false)
        },
        Expr::Struct { name, body } => {
            definition_ass_list.push_back(name.clone());
            get_definition_of_expr(body, &mut definition_ass_list.clone(), ident_offset, false)
        },
        Expr::Namespace { name, body } => {
            match get_definition_of_expr(name, definition_ass_list, ident_offset, true) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            };
            // TODO: retain the scope but with a prefix of the namespace name
            get_definition_of_expr(body, &mut definition_ass_list.clone(), ident_offset, false)
        },
        Expr::Enum { name, value_type, body } => {
            definition_ass_list.push_back(name.clone());
            match get_definition_of_type_def(value_type, definition_ass_list, ident_offset) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(body, definition_ass_list, ident_offset, false)
        },
        Expr::Bitfield { name, body } => {
            definition_ass_list.push_back(name.clone());
            get_definition_of_expr(body, definition_ass_list, ident_offset, false)
        },
        Expr::Access { item, member } => {
            match get_definition_of_expr(item, definition_ass_list, ident_offset, is_defining) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(member, definition_ass_list, ident_offset, is_defining)
        },
        Expr::Attribute { arguments } => {
            for expr in arguments.0.iter() {
                match get_definition_of_expr(&expr, definition_ass_list, ident_offset, false) {
                    (true, None) => continue,
                    (true, Some(value)) => return (false, Some(value)),
                    (false, None) => return (false, None),
                    (false, Some(value)) => return (false, Some(value)),
                }
            }
            (true, None)
        },
        Expr::AttributeArgument { name, value } => (true, None),  // TODO: Check if anything defined can be in arguments
        Expr::WhileLoop { condition, body } => {
            match get_definition_of_expr(condition, definition_ass_list, ident_offset, false) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(body, &mut definition_ass_list.clone(), ident_offset, false)
        },
        Expr::ForLoop { var_init, var_test, var_change, body } => {
            let mut new_scope = definition_ass_list.clone();
            match get_definition_of_expr(var_init, &mut new_scope, ident_offset, false) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(var_test, &mut new_scope, ident_offset, false) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(var_change, &mut new_scope, ident_offset, false) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(body, &mut new_scope, ident_offset, false)
        },
        Expr::Cast { cast_operator: _, operand } => get_definition_of_expr(operand, definition_ass_list, ident_offset, false),
        Expr::Union { name, body } => {
            definition_ass_list.push_back(name.clone());
            get_definition_of_expr(body, definition_ass_list, ident_offset, false)
        },
    }
}

fn get_definition_of_type_def(
    type_: &Spanned<HexTypeDef>,
    definition_ass_list: &Vector<Spanned<String>>,
    ident_offset: usize
) -> (bool, Option<Spanned<String>>) {
    let HexTypeDef {
        endianness: _,
        name,
    } = &type_.0;
    get_definition_of_type(name, definition_ass_list, ident_offset)
}

fn get_definition_of_type(
    type_: &Spanned<HexType>,
    definition_ass_list: &Vector<Spanned<String>>,
    ident_offset: usize
) -> (bool, Option<Spanned<String>>) {
    if ident_offset >= type_.1.start && ident_offset <= type_.1.end {
        match &type_.0 {
            HexType::Custom(name) => {
                let index = definition_ass_list
                    .iter()
                    .position(|decl| decl.0 == name.clone());
                (
                    false,
                    index.map(|i| definition_ass_list.get(i).unwrap().clone()),
                )
            },
            HexType::Path(p) => {
                let name = p.last().unwrap().clone(); // TODO: Don't ignore the rest of the path
                let index = definition_ass_list
                    .iter()
                    .position(|decl| decl.0 == name);
                (
                    false,
                    index.map(|i| definition_ass_list.get(i).unwrap().clone()),
                )
            },
            HexType::V(_) => (true, None),
            HexType::Null => (true, None),
        }
    } else {
        (true, None)
    }
}
