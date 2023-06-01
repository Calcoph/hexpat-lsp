use hexparser::{Expr, token::Spanned};

pub(crate) fn namespace_to_string(namespace: &Box<Spanned<Expr>>) -> String {
    match &namespace.0 {
        Expr::Local { name } => name.0.clone(),
        Expr::NamespaceAccess { previous, name } => namespace_to_string_rec(previous, name.0.clone()),
        _ => panic!("non-namespace was passed to namespace\n{namespace:?}")
    }
}

fn namespace_to_string_rec(namespace: &Box<Spanned<Expr>>, accumulated_namespace: String) -> String {
    match &namespace.0 {
        Expr::Local { name } => format!("{}::{}", name.0, accumulated_namespace),
        Expr::NamespaceAccess { previous, name } => format!("{}::{}", namespace_to_string_rec(previous, name.0.clone()), accumulated_namespace),
        _ => panic!("non-namespace was passed to namespace\n{namespace:?}")
    }
}
