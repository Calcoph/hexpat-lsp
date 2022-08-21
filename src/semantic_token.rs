use std::collections::HashMap;

use parserlib::LEGEND_TYPE;
use tower_lsp::lsp_types::SemanticTokenType;

use hexparser::{ImCompleteSemanticToken, m_parser::{Spanned, Expr, NormalASTNode, NamedASTNode, Declaration}};

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
            NamedASTNode::Struct(v) => {
                let (_, span) = &v.name;
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::STRUCT)
                        .unwrap(),
                });
                semantic_token_from_expr(&v.body, &mut semantic_tokens);
            },
            NamedASTNode::Enum(v) => {
                let (_, span) = &v.name;
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::ENUM)
                        .unwrap(),
                });
                semantic_token_from_expr(&v.body, &mut semantic_tokens);
            },
            NamedASTNode::Namespace(v) => {
                let (_, span) = &v.name;
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::NAMESPACE)
                        .unwrap(),
                });
                semantic_token_from_expr(&v.body, &mut semantic_tokens);
            },
            NamedASTNode::Bitfield(v) => {
                let (_, span) = &v.name;
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::new("bitfield"))
                        .unwrap(),
                });
                semantic_token_from_expr(&v.body, &mut semantic_tokens);
            },
            NamedASTNode::Expr(Declaration { type_: _, name: (_, span), body: lhs }) => {
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::VARIABLE)
                        .unwrap(),
                });
                semantic_token_from_expr(lhs, &mut semantic_tokens);
            },
        }
    });

    /* ast.1.iter().for_each(|node| match node {
        NormalASTNode::Expr((node, span)) => match node {
            Expr::Error => todo!(),
            Expr::Value(_) => todo!(),
            Expr::Local(_) => todo!(),
            Expr::Then(_, _) => todo!(),
            Expr::Binary(_, _, _) => todo!(),
            Expr::Call(_, _) => todo!(),
            Expr::If(_, _, _) => todo!(),
            Expr::Definition(_, _, _, _, _) => todo!(),
        },
    }); */

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
    }
}
