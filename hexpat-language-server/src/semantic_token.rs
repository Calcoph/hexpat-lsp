use std::collections::HashMap;

use parserlib::LEGEND_TYPE;
use tower_lsp::lsp_types::SemanticTokenType;

use hexparser::{ImCompleteSemanticToken, m_parser::{Expr, NamedNode}, token::Spanned};

pub fn semantic_token_from_ast(ast: &(HashMap<String, Spanned<NamedNode>>, Spanned<Expr>)) -> Vec<ImCompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    semantic_token_from_expr(&ast.1, &mut semantic_tokens); // TODO: Uncomment this

    semantic_tokens
}

pub fn semantic_token_from_expr(
    expr: &Spanned<Expr>,
    semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
) {
    match &expr.0 {
        Expr::Error => {}
        Expr::Value { .. } => {}
        Expr::Local { name: (_, name_span) } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
        }
        Expr::Binary { loperand, operator, roperand } => {
            semantic_token_from_expr(loperand, semantic_tokens);
            semantic_token_from_expr(roperand, semantic_tokens);
        }
        Expr::Call { func_name, arguments } => {
            semantic_token_from_expr(func_name, semantic_tokens);
            arguments.0.iter().for_each(|p| {
                semantic_token_from_expr(p, semantic_tokens);
            });
        }
        Expr::If { test, consequent, alternative } => {
            semantic_token_from_expr(test, semantic_tokens);
            semantic_token_from_expr(consequent, semantic_tokens);
            semantic_token_from_expr(alternative, semantic_tokens);
        }
        Expr::Definition { value_type: (_, type_span), name, body } => {
            match &name.0 {
                Expr::Local { name: (_, name_span) } => {
                    semantic_tokens.push(ImCompleteSemanticToken {
                        start: name_span.start,
                        length: name_span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::VARIABLE)
                            .unwrap(),
                    });     
                },
                _ => () // TODO
            }
            semantic_tokens.push(ImCompleteSemanticToken {
                start: type_span.start,
                length: type_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::TYPE)
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens)
        },
        Expr::BitFieldEntry { name, length } => {
            semantic_token_from_expr(length, semantic_tokens);
        },// TODO
        Expr::EnumEntry { name: (_, name_span), value } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::ENUM_MEMBER)
                    .unwrap(),
            });
            semantic_token_from_expr(value, semantic_tokens);
        },
        Expr::Ternary { loperand, moperand, roperand } => {
            semantic_token_from_expr(loperand, semantic_tokens);
            semantic_token_from_expr(moperand, semantic_tokens);
            semantic_token_from_expr(roperand, semantic_tokens)
        },
        Expr::NamespaceAccess { previous, name } => {
            semantic_token_from_expr(previous, semantic_tokens)
        }, // TODO
        Expr::Dollar => semantic_tokens.push(ImCompleteSemanticToken {
            start: expr.1.start,
            length: expr.1.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::new("dollar"))
                .unwrap(),
        }),
        Expr::Unary { operation, operand } => {
            semantic_token_from_expr(operand, semantic_tokens)
        },
        Expr::Using { new_name, old_name } => {
            //TODO
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
        Expr::ExprList { list } => {
            for expr in list {
                semantic_token_from_expr(expr, semantic_tokens)
            }
        },
        Expr::Func { name: (_, name_span), args, body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::FUNCTION)
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens)
        }, // TODO
        Expr::Struct { name: (_, name_span), body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::STRUCT)
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens)
        },
        Expr::Namespace { name, body } => {
            /* semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::NAMESPACE)
                    .unwrap(),
            }); */ // TODO
            semantic_token_from_expr(body, semantic_tokens)
        },
        Expr::Enum { name: (_, name_span), value_type, body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::ENUM)
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens)
        }, // TODO
        Expr::Bitfield { name: (_, name_span), body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::new("bitfield"))
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens)
        },
        Expr::UnnamedParameter { type_ } => (), // TODO
        Expr::Return { value } => (), // TODO
        Expr::Access { item, member } => (), // TODO
        Expr::Attribute { arguments } => (), // TODO
        Expr::AttributeArgument { name, value } => (), // TODO
        Expr::WhileLoop { condition, body } => (), // TODO
        Expr::ForLoop { var_init, var_test, var_change, body } => (), // TODO
        Expr::Cast { cast_operator, operand } => (), // TODO
        Expr::Union { name, body } => (), // TODO
    }
}
