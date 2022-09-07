use std::collections::HashMap;

use im_rc::Vector;

use hexparser::{m_parser::{Expr, NamedNode}, token::Spanned};

/// return (need_to_continue_search, founded reference)
pub fn get_definition(ast: &(HashMap<String, Spanned<NamedNode>>, Spanned<Expr>), ident_offset: usize) -> Option<Spanned<String>> {
    let mut vector = Vector::new();
    for (name, (v, span)) in ast.0.iter() {
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
}

pub fn get_definition_of_expr(
    expr: &Spanned<Expr>,
    definition_ass_list: Vector<Spanned<String>>,
    ident_offset: usize,
) -> (bool, Option<Spanned<String>>) {
    match &expr.0 {
        Expr::Error => (true, None),
        Expr::Value { .. } => (true, None),
        // Expr::List(exprs) => exprs
        //     .iter()
        //     .for_each(|expr| get_definition(expr, definition_ass_list)),
        Expr::Local { name } => {
            if ident_offset >= name.1.start && ident_offset < name.1.end {
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
        Expr::Binary { loperand, operator, roperand } => {
            match get_definition_of_expr(loperand, definition_ass_list.clone(), ident_offset) {
                (true, None) => {
                    get_definition_of_expr(roperand, definition_ass_list.clone(), ident_offset)
                }
                (false, None) => (false, None),
                (true, Some(value)) | (false, Some(value)) => (false, Some(value)),
            }
        }
        Expr::Call { func_name, arguments } => {
            match get_definition_of_expr(func_name, definition_ass_list.clone(), ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            for expr in &arguments.0 {
                match get_definition_of_expr(&expr, definition_ass_list.clone(), ident_offset) {
                    (true, None) => continue,
                    (true, Some(value)) => return (false, Some(value)),
                    (false, None) => return (false, None),
                    (false, Some(value)) => return (false, Some(value)),
                }
            }
            (true, None)
        }
        Expr::If { test, consequent, alternative } => {
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
        Expr::Definition { value_type, name, body } => {
            get_definition_of_expr(body, definition_ass_list.clone(), ident_offset)
        },
        Expr::Dollar => (false, None), // TODO
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
        Expr::Union { name, body } => (false, None), // TODO
    }
}
