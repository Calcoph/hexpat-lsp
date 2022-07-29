use std::collections::HashMap;

use tower_lsp::lsp_types::SemanticTokenType;

use crate::chumsky::{ImCompleteSemanticToken, m_parser::{func::Func, Spanned, Expr, NormalASTNode, NamedASTNode}};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::new("directive"),
];

pub fn semantic_token_from_ast(ast: &(HashMap<String, NamedASTNode>, Vec<NormalASTNode>)) -> Vec<ImCompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    ast.0.iter().for_each(|(_name, node)| {
        match node {
            NamedASTNode::Func(f) => {
                f.args.iter().for_each(|(_, span)| {
                    semantic_tokens.push(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::PARAMETER)
                            .unwrap(),
                    });
                });

                let (_, span) = &f.name;
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::FUNCTION)
                        .unwrap(),
                });
                semantic_token_from_expr(&f.body, &mut semantic_tokens);
            },
            NamedASTNode::Struct(_) => todo!(),
            NamedASTNode::Enum(_) => todo!(),
            NamedASTNode::Namespace(_) => todo!(),
            NamedASTNode::Bitfield(_) => todo!(),
        }
    });

    semantic_tokens
}

pub fn semantic_token_from_expr(
    expr: &Spanned<Expr>,
    semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
) {
    match &expr.0 {
        Expr::Error => {}
        Expr::Value(_) => {}
        Expr::Local((_name, span)) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: span.start,
                length: span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
        }
        Expr::Then(first, rest) => {
            semantic_token_from_expr(first, semantic_tokens);
            semantic_token_from_expr(rest, semantic_tokens);
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
        Expr::Definition(_, _, _, _, _) => todo!(),
    }
}
