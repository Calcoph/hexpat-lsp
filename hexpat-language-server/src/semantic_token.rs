use std::collections::HashMap;

use parserlib::LEGEND_TYPE;
use tower_lsp::lsp_types::SemanticTokenType;

use hexparser::{ImCompleteSemanticToken, m_parser::Expr, token::Spanned};

pub fn semantic_token_from_ast(ast: &Spanned<Expr>) -> Vec<ImCompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    semantic_token_from_expr(ast, &mut semantic_tokens);

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
        Expr::Binary { loperand, operator: _, roperand } => {
            semantic_token_from_expr(loperand, semantic_tokens);
            semantic_token_from_expr(roperand, semantic_tokens);
        }
        Expr::Call { func_name, arguments } => {
            semantic_token_from_expr(func_name, semantic_tokens);
            arguments.0.iter().for_each(|p| {
                semantic_token_from_expr(p, semantic_tokens);
            });
        }
        Expr::If { test, consequent } => {
            semantic_token_from_expr(test, semantic_tokens);
            semantic_token_from_expr(consequent, semantic_tokens);
        }
        Expr::IfBlock { ifs, alternative } => {
            semantic_token_from_expr(ifs, semantic_tokens);
            semantic_token_from_expr(alternative, semantic_tokens);
        },
        Expr::Definition { value_type: (_, type_span), name, body } => {
            semantic_token_from_expr(name, semantic_tokens);
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
        Expr::BitFieldEntry { name: (_, name_span), length } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
            semantic_token_from_expr(length, semantic_tokens);
        },
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
        Expr::NamespaceAccess { previous, name: (_, name_span) } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
            semantic_token_from_expr(previous, semantic_tokens)
        },
        Expr::Unary { operation: _, operand } => {
            semantic_token_from_expr(operand, semantic_tokens)
        },
        Expr::Using { new_name: (_, name_span), old_name: (_, old_name_span) } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::TYPE)
                    .unwrap(),
            });

            semantic_tokens.push(ImCompleteSemanticToken {
                start: old_name_span.start,
                length: old_name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::TYPE)
                    .unwrap(),
            });
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
            for arg in &args.0 {
                match &arg.0 {
                    hexparser::m_parser::FuncArgument::Parameter(arg) => semantic_token_from_expr(arg, semantic_tokens),
                    hexparser::m_parser::FuncArgument::ParameterPack((_, type_span)) => semantic_tokens.push(ImCompleteSemanticToken {
                        start: type_span.start,
                        length: type_span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::TYPE)
                            .unwrap(),
                    }),
                }
            }
            semantic_token_from_expr(body, semantic_tokens)
        },
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
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name.1.start,
                length: name.1.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::NAMESPACE)
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens)
        },
        Expr::Enum { name: (_, name_span), value_type: (_, type_span), body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::ENUM)
                    .unwrap(),
            });
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
        Expr::UnnamedParameter { type_: (_, span) } => semantic_tokens.push(ImCompleteSemanticToken {
            start: span.start,
            length: span.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::TYPE)
                .unwrap(),
        }),
        Expr::Return { value } => semantic_tokens.push(ImCompleteSemanticToken {
            start: value.1.start,
            length: value.1.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::KEYWORD)
                .unwrap(),
        }),
        Expr::Access { item, member } => {
            semantic_token_from_expr(item, semantic_tokens);
            semantic_token_from_expr(member, semantic_tokens);
        },
        Expr::Attribute { arguments } => for arg in &arguments.0 {
            semantic_token_from_expr(arg, semantic_tokens);
        },
        Expr::AttributeArgument { name: (_, name_span), value } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::PROPERTY)
                    .unwrap(),
            });
            semantic_token_from_expr(value, semantic_tokens);
        },
        Expr::WhileLoop { condition, body } => {
            semantic_token_from_expr(condition, semantic_tokens);
            semantic_token_from_expr(body, semantic_tokens);
        },
        Expr::ForLoop { var_init, var_test, var_change, body } => {
            semantic_token_from_expr(var_init, semantic_tokens);
            semantic_token_from_expr(var_test, semantic_tokens);
            semantic_token_from_expr(var_change, semantic_tokens);
            semantic_token_from_expr(body, semantic_tokens);
        },
        Expr::Cast { cast_operator: (_, type_span), operand } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: type_span.start,
                length: type_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::TYPE)
                    .unwrap(),
            });
            semantic_token_from_expr(operand, semantic_tokens);  
        },
        Expr::Union { name: (_, name_span), body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::new("union"))
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens);
        },
    }
}
