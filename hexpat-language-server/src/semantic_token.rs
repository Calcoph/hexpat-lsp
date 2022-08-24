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
        Expr::Definition((_, type_span), (_, name_span), rhs) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: type_span.start,
                length: type_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::TYPE)
                    .unwrap(),
            });
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
        Expr::BitFieldEntry((_, name_span), e1, e2) => {
            semantic_token_from_expr(e1, semantic_tokens);
            semantic_token_from_expr(e2, semantic_tokens)
        },// TODO
        Expr::EnumEntry((_, name_span), e1, e2) => {
            semantic_token_from_expr(e1, semantic_tokens);
            semantic_token_from_expr(e2, semantic_tokens)
        },// TODO
        Expr::MemberAccess(e, (_, name_span)) => {
            semantic_token_from_expr(e, semantic_tokens);
        }, // TODO
        Expr::ArrayAccess(e1, e2) => {
            semantic_token_from_expr(e1, semantic_tokens);
            semantic_token_from_expr(e2, semantic_tokens)
        },
        Expr::Ternary(e1, e2, e3) => {
            semantic_token_from_expr(e1, semantic_tokens);
            semantic_token_from_expr(e2, semantic_tokens);
            semantic_token_from_expr(e3, semantic_tokens)
        },
        Expr::NamespaceAccess(e, (_, name_span)) => {
            semantic_token_from_expr(e, semantic_tokens)
        }, // TODO
        Expr::Dollar => semantic_tokens.push(ImCompleteSemanticToken {
            start: expr.1.start,
            length: expr.1.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::KEYWORD)
                .unwrap(),
        }), // TODO: Make dollars different from keywords
        Expr::Unary(_, e) => {
            semantic_token_from_expr(e, semantic_tokens)
        },
        Expr::Using(e) => {
            semantic_token_from_expr(e, semantic_tokens)
        },
        Expr::Continue => semantic_tokens.push(ImCompleteSemanticToken {
            start: expr.1.start,
            length: expr.1.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::KEYWORD)
                .unwrap(),
        }),
        Expr::Break => semantic_tokens.push(ImCompleteSemanticToken {
            start: expr.1.start,
            length: expr.1.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::KEYWORD)
                .unwrap(),
        }),
        Expr::ExprList(exps) => {
            for expr in exps.as_ref() {
                semantic_token_from_expr(expr, semantic_tokens)
            }
        },
        Expr::Func((_, name_span), args, e) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::FUNCTION)
                    .unwrap(),
            });
            semantic_token_from_expr(e, semantic_tokens)
        }, // TODO
        Expr::Struct((_, name_span), e) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::STRUCT)
                    .unwrap(),
            });
            semantic_token_from_expr(e, semantic_tokens)
        },
        Expr::Namespace((_, name_span), e) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::NAMESPACE)
                    .unwrap(),
            });
            semantic_token_from_expr(e, semantic_tokens)
        },
        Expr::Enum((_, name_span), e, type_) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::ENUM)
                    .unwrap(),
            });
            semantic_token_from_expr(e, semantic_tokens)
        }, // TODO
        Expr::Bitfield((_, name_span), e) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::new("bitfield"))
                    .unwrap(),
            });
            semantic_token_from_expr(e, semantic_tokens)
        },
    }
}
