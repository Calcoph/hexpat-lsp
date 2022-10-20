use std::collections::HashMap;

use im_rc::Vector;

use hexparser::{m_parser::{Expr, NamedNode, HexTypeDef, HexType}, token::Spanned};

pub fn get_definition(ast: &(HashMap<String, Spanned<NamedNode>>, Spanned<Expr>), ident_offset: usize) -> Option<Spanned<String>> {
    let (_, name) = get_definition_of_expr(&ast.1, ident_offset);
    match name {
        Some(name) => match ast.0.get(&name) {
            Some((_, span)) => {
                Some((name, span.clone()))
            },
            None => {
                // If it is not a global name, it must be a function argument (or namespaces, but that's not done yet) // TODO: Do namespaces
                let iter = ast.0.iter().filter(|(_, (node, _))| match node {
                    NamedNode::Function(_) => true,
                    _ => false
                });
                for (_, (node, span)) in iter {
                    if ident_offset >= span.start {
                        if let NamedNode::Function(args) = node {
                            for arg in args {
                                if arg.0 == name {
                                    return Some(arg.clone())
                                }
                            }
                        } else {
                            unreachable!()
                        }
                    }
                };

                None
            },
        },
        None => None,
    }
    //let mut vector = Vector::new();
    /* for (name, (v, span)) in ast.0.iter() {
        if span.end < ident_offset {
            vector.push_back(name.clone());
        }
    }

    for (_, (v, span)) in ast.0.iter() {
        if let NamedNode::Function(args) = v {
            let args = args.iter().map(|arg| arg.clone()).collect::<Vector<_>>();
        }
    }
    None
    */
}

/// return (need_to_continue_search, found reference)
pub fn get_definition_of_expr(
    expr: &Spanned<Expr>,
    //definition_ass_list: Vector<Spanned<String>>,
    ident_offset: usize,
) -> (bool, Option<String>) {
    if ident_offset > expr.1.end {
        return (false, None)
    };
    match &expr.0 {
        Expr::Error => (true, None),
        Expr::Value { .. } => (true, None),
        Expr::Local { name } => {
            if ident_offset >= name.1.start && ident_offset < name.1.end {
                (
                    false,
                    Some(name.0.clone()),
                )
            } else {
                (true, None)
            }
        }
        Expr::Binary { loperand, operator: _, roperand } => {
            match get_definition_of_expr(loperand, ident_offset) {
                (true, None) => {
                    get_definition_of_expr(roperand, ident_offset)
                }
                (false, None) => (false, None),
                (true, Some(value)) | (false, Some(value)) => (false, Some(value)),
            }
        }
        Expr::Call { func_name, arguments } => {
            match get_definition_of_expr(func_name, ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            for expr in &arguments.0 {
                match get_definition_of_expr(&expr, ident_offset) {
                    (true, None) => continue,
                    (true, Some(value)) => return (false, Some(value)),
                    (false, None) => return (false, None),
                    (false, Some(value)) => return (false, Some(value)),
                }
            }
            (true, None)
        }
        Expr::If { test, consequent } => {
            match get_definition_of_expr(test, ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(consequent, ident_offset) {
                (true, None) => return (true, None),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
        }
        Expr::IfBlock { ifs, alternative } => {
            match get_definition_of_expr(ifs, ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(alternative, ident_offset)
        },
        Expr::Definition { value_type, name, body } => {
            match get_definition_of_type(value_type, ident_offset) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(name, ident_offset) {
                (true, None) => (),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            get_definition_of_expr(body, ident_offset)
        },
        Expr::ExprList { list } => (false, None), // TODO
        Expr::UnnamedParameter { type_ } => (false, None), // TODO
        Expr::Unary { operation, operand } => (false, None), // TODO
        Expr::Ternary { loperand, moperand, roperand } => (false, None), // TODO
        Expr::BitFieldEntry { name, length } => (false, None), // TODO
        Expr::EnumEntry { name, value } => (false, None), // TODO
        Expr::NamespaceAccess { previous, name } => (false, None), // TODO
        Expr::Using { new_name, old_name } => (false, None), // TODO
        Expr::Return { value } => (false, None), // TODO
        Expr::Continue => (false, None), // TODO
        Expr::Break => (false, None), // TODO
        Expr::Func { name, args, body } => (false, None), // TODO
        Expr::Struct { name, body } => (false, None), // TODO
        Expr::Namespace { name, body } => (false, None), // TODO
        Expr::Enum { name, value_type, body } => (false, None), // TODO
        Expr::Bitfield { name, body } => (false, None), // TODO
        Expr::Access { item, member } => (false, None), // TODO
        Expr::Attribute { arguments } => (false, None), // TODO
        Expr::AttributeArgument { name, value } => (false, None), // TODO
        Expr::WhileLoop { condition, body } => (false, None), // TODO
        Expr::ForLoop { var_init, var_test, var_change, body } => (false, None), // TODO
        Expr::Cast { cast_operator, operand } => (false, None), // TODO
        Expr::Union { name, body } => (false, None),
    }
}

fn get_definition_of_type(
    type_: &Spanned<HexTypeDef>,
    ident_offset: usize
) -> (bool, Option<String>) {
    let HexTypeDef {
        endianness: _,
        name,
    } = &type_.0;
    if ident_offset >= name.1.start && ident_offset < name.1.end {
        match &name.0 {
            HexType::Custom(c) => (false, Some(c.clone())),
            HexType::Path(p) => (false, Some(p.last().unwrap().clone())), // TODO: Don't ignore the rest of the path
            HexType::V(_) => (true, None),
            HexType::Null => (true, None),
        }
    } else {
        (true, None)
    }
}
