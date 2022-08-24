use std::collections::HashMap;

use parserlib::LEGEND_TYPE;
use tower_lsp::lsp_types::SemanticTokenType;

use hexparser::{ImCompleteSemanticToken, m_parser::{Spanned, Expr, NamedNode}};

pub fn semantic_token_from_ast(ast: &(HashMap<String, Spanned<NamedNode>>, Spanned<Expr>)) -> Vec<ImCompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    semantic_token_from_expr(&ast.1, &mut semantic_tokens);

    semantic_tokens
}

pub fn semantic_token_from_expr(
    expr: &Spanned<Expr>,
    semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
) {
    match &expr.0 {
        Expr::Error => {}
        Expr::Value(_) => {}
        Expr::Local((_, span)) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: span.start,
                length: span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
        }
        Expr::Binary(lhs, _op, rhs) => {
            semantic_token_from_expr(lhs, semantic_tokens);
            semantic_token_from_expr(rhs, semantic_tokens);
        }
        Expr::Call(expr, params) => {
            semantic_token_from_expr(expr, semantic_tokens);
            params.0.iter().for_each(|p| {
                semantic_token_from_expr(p, semantic_tokens);
            });
        }
        Expr::If(test, consequent, alternative) => {
            semantic_token_from_expr(test, semantic_tokens);
            semantic_token_from_expr(consequent, semantic_tokens);
            semantic_token_from_expr(alternative, semantic_tokens);
        }
        Expr::Definition(_, (_, name_span), rhs) => { // TODO: The type
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
            semantic_token_from_expr(rhs, semantic_tokens)
        },
        Expr::BitFieldEntry(_, _, _) => (),// TODO
        Expr::EnumEntry(_, _, _) => (), // TODO
        Expr::MemberAccess(_, _) => (), // TODO
        Expr::ArrayAccess(_, _) => (), // TODO
        Expr::Ternary(_, _, _) => (), // TODO
        Expr::NamespaceAccess(_, _) => (), // TODO
        Expr::Dollar => (), // TODO
        Expr::Unary(_, _) => (), // TODO
        Expr::Using(_) => (), // TODO
        Expr::Continue => (), // TODO
        Expr::Break => (), // TODO
        Expr::ExprList(_) => (), // TODO
        Expr::Func(_, _) => (), // TODO
        Expr::Struct(_, _) => (), // TODO
        Expr::Namespace(_, _) => (), // TODO
        Expr::Enum(_, _) => (), // TODO
        Expr::Bitfield(_, _) => (), // TODO
    }
}
