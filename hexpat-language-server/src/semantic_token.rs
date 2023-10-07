use parserlib::LEGEND_TYPE;
use tower_lsp::lsp_types::SemanticTokenType;

use hexparser::{ImCompleteSemanticToken, m_parser::{Expr, MatchBranch, MatchCaseItem, MatchCaseElement, FuncCall, Definition, Statement}, token::Spanned};

pub fn semantic_token_from_ast(ast: &Spanned<Vec<Spanned<Statement>>>) -> Vec<ImCompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    semantic_token_from_statements(&ast.0, &mut semantic_tokens);

    semantic_tokens
}

pub fn semantic_token_from_exprs(
    exprs: &Vec<Spanned<Expr>>,
    semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
) {
    for expr in exprs {
        semantic_token_from_expr(expr, semantic_tokens)
    }
}

pub fn semantic_token_from_statements(
    stmnts: &Vec<Spanned<Statement>>,
    semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
) {
    for stmnt in stmnts {
        semantic_token_from_statement(stmnt, semantic_tokens)
    }
}

pub fn semantic_token_from_statement(
    stmnt: &Spanned<Statement>,
    semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
) {
    match &stmnt.0 {
        Statement::If { test, consequent } => {
            semantic_token_from_expr(test, semantic_tokens);
            semantic_token_from_statements(&consequent.0, semantic_tokens);
        }
        Statement::IfBlock { ifs, alternative } => {
            semantic_token_from_statements(&ifs.0, semantic_tokens);
            semantic_token_from_statements(&alternative.0, semantic_tokens);
        },
        Statement::BitFieldEntry { name: (_, name_span), length } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::VARIABLE.as_str())
                    .unwrap(),
            });
            semantic_token_from_expr(length, semantic_tokens);
        },
        Statement::Using { new_name: (_, name_span), template_parameters, old_name: (_, old_name_span) } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                    .unwrap(),
            });

            semantic_tokens.push(ImCompleteSemanticToken {
                start: old_name_span.start,
                length: old_name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                    .unwrap(),
            });
            semantic_token_from_exprs(template_parameters, semantic_tokens)
        },
        Statement::Continue => semantic_tokens.push(ImCompleteSemanticToken {
            start: stmnt.1.start,
            length: stmnt.1.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item.as_str() == SemanticTokenType::KEYWORD.as_str())
                .unwrap(),
        }),
        Statement::Break => semantic_tokens.push(ImCompleteSemanticToken {
            start: stmnt.1.start,
            length: stmnt.1.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item.as_str() == SemanticTokenType::KEYWORD.as_str())
                .unwrap(),
        }),
        Statement::Func { name: (_, name_span), args, body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::FUNCTION.as_str())
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
                            .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                            .unwrap(),
                    }),
                }
            }
            semantic_token_from_statements(&body.0, semantic_tokens)
        },
        Statement::Struct { name: (_, name_span), body, template_parameters } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::STRUCT.as_str())
                    .unwrap(),
            });
            semantic_token_from_statements(&body.0, semantic_tokens);
            semantic_token_from_exprs(template_parameters, semantic_tokens)
        },
        Statement::Namespace { name, body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name.1.start,
                length: name.1.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::NAMESPACE.as_str())
                    .unwrap(),
            });
            semantic_token_from_statements(&body.0, semantic_tokens)
        },
        Statement::Enum { name: (_, name_span), value_type: (_, type_span), body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::ENUM.as_str())
                    .unwrap(),
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: type_span.start,
                length: type_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                    .unwrap(),
            });
            semantic_token_from_exprs(&body.0, semantic_tokens)
        },
        Statement::Bitfield { name: (_, name_span), body } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::new("bitfield").as_str())
                    .unwrap(),
            });
            semantic_token_from_statements(&body.0, semantic_tokens)
        },
        Statement::Return { value } => {
            semantic_token_from_expr(value, semantic_tokens);
        },
        Statement::ForLoop { var_init, var_test, var_change, body } => {
            semantic_token_from_statement(var_init, semantic_tokens);
            semantic_token_from_expr(var_test, semantic_tokens);
            semantic_token_from_statement(var_change, semantic_tokens);
            semantic_token_from_statements(&body.0, semantic_tokens);
        },
        Statement::Union { name: (_, name_span), body, template_parameters } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::new("union").as_str())
                    .unwrap(),
            });
            semantic_token_from_statements(&body.0, semantic_tokens);
            semantic_token_from_exprs(template_parameters, semantic_tokens)
        },
        Statement::ArrayDefinition { value_type: (_, type_span), array_name, size, body } => {
            semantic_token_from_expr(array_name, semantic_tokens);
            semantic_token_from_expr(size, semantic_tokens);
            semantic_tokens.push(ImCompleteSemanticToken {
                start: type_span.start,
                length: type_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens)
        },
        Statement::Match { parameters, branches } => {
            for parameter in parameters {
                semantic_token_from_expr(parameter, semantic_tokens)
            }

            for branch in branches {
                let MatchBranch {
                    case,
                    body,
                } = branch;
                semantic_token_from_statements(&body.0, semantic_tokens);
                for (item, item_span) in case.0.case.iter() {
                    match item {
                        MatchCaseItem::Anything => {
                            semantic_tokens.push(ImCompleteSemanticToken {
                                start: item_span.start,
                                length: item_span.len(),
                                token_type: LEGEND_TYPE
                                    .iter()
                                    .position(|item| item.as_str() == SemanticTokenType::VARIABLE.as_str())
                                    .unwrap(),
                            });
                        },
                        MatchCaseItem::Element(element) => semantic_token_from_match_element(element, semantic_tokens),
                        MatchCaseItem::OrList(elements) => {
                            for element in elements {
                                semantic_token_from_match_element(element, semantic_tokens)
                            }
                        },
                    }
                }
            }
        },
        Statement::TryCatch { try_block, catch_block } => {
            semantic_token_from_statements(&try_block.0, semantic_tokens);
            semantic_token_from_statements(&catch_block.0, semantic_tokens)
        },
        Statement::Assignment { loperand, operator, roperand } => {
            semantic_token_from_expr(loperand, semantic_tokens);
            semantic_token_from_expr(roperand, semantic_tokens);
        },
        Statement::Error => {},
        Statement::WhileLoop { condition, body } => {
            semantic_token_from_expr(condition, semantic_tokens);
            semantic_token_from_statements(&body.0, semantic_tokens);
        },
        Statement::Definition(Definition { value_type: (_, type_span), name, body }) => {
            semantic_token_from_expr(name, semantic_tokens);
            semantic_tokens.push(ImCompleteSemanticToken {
                start: type_span.start,
                length: type_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens)
        },
        Statement::Padding { padding_body } => semantic_token_from_expr(padding_body, semantic_tokens),
        Statement::Call(FuncCall { func_name, arguments }) => {
            semantic_token_from_expr(func_name, semantic_tokens);
            arguments.0.iter().for_each(|p| {
                semantic_token_from_expr(p, semantic_tokens);
            });
        }
    }
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
                .position(|item| item.as_str() == SemanticTokenType::VARIABLE.as_str())
                .unwrap(),
            });
        }
        Expr::Binary { loperand, operator: _, roperand } => {
            semantic_token_from_expr(loperand, semantic_tokens);
            semantic_token_from_expr(roperand, semantic_tokens);
        }
        Expr::Call(FuncCall { func_name, arguments }) => {
            semantic_token_from_expr(func_name, semantic_tokens);
            arguments.0.iter().for_each(|p| {
                semantic_token_from_expr(p, semantic_tokens);
            });
        }
        Expr::Definition(Definition { value_type: (_, type_span), name, body }) => {
            semantic_token_from_expr(name, semantic_tokens);
            semantic_tokens.push(ImCompleteSemanticToken {
                start: type_span.start,
                length: type_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                    .unwrap(),
            });
            semantic_token_from_expr(body, semantic_tokens)
        },
        Expr::EnumEntry { name: (_, name_span), value } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::ENUM_MEMBER.as_str())
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
                    .position(|item| item.as_str() == SemanticTokenType::VARIABLE.as_str())
                    .unwrap(),
            });
            semantic_token_from_expr(previous, semantic_tokens)
        },
        Expr::Unary { operation: _, operand } => {
            semantic_token_from_expr(operand, semantic_tokens)
        },
        Expr::ExprList { list } => semantic_token_from_exprs(list, semantic_tokens),
        Expr::UnnamedParameter { type_: (_, span) } => semantic_tokens.push(ImCompleteSemanticToken {
            start: span.start,
            length: span.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                .unwrap(),
        }),
        Expr::Access { item, member } => {
            semantic_token_from_expr(item, semantic_tokens);
            semantic_token_from_expr(member, semantic_tokens);
        },
        Expr::Attribute { arguments } => for arg in &arguments.0 {
            semantic_token_from_expr(arg, semantic_tokens);
        },
        Expr::AttributeArgument { name, value } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name.1.start,
                length: name.1.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::PROPERTY.as_str())
                    .unwrap(),
            });
            for arg in value {
                semantic_token_from_expr(arg, semantic_tokens)
            }
        },
        Expr::WhileLoop { condition, body } => {
            semantic_token_from_expr(condition, semantic_tokens);
            semantic_token_from_expr(body, semantic_tokens);
        },
        Expr::Cast { cast_operator: (_, type_span), operand } => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: type_span.start,
                length: type_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                    .unwrap(),
            });
            semantic_token_from_expr(operand, semantic_tokens);
        },
        Expr::ArrayAccess { array, index } => {
            semantic_token_from_expr(array, semantic_tokens);
            semantic_token_from_expr(index, semantic_tokens);
        },
        Expr::Type { val } => {
            let type_span = &expr.1;
            semantic_tokens.push(ImCompleteSemanticToken {
                start: type_span.start,
                length: type_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item.as_str() == SemanticTokenType::TYPE.as_str())
                    .unwrap(),
            });
        },
    }
}

fn semantic_token_from_match_element(element: &MatchCaseElement, semantic_tokens: &mut Vec<ImCompleteSemanticToken>) {
    match element {
        MatchCaseElement::Range(exp1, exp2) => {
            semantic_token_from_expr(exp1, semantic_tokens);
            semantic_token_from_expr(exp2, semantic_tokens);
        },
        MatchCaseElement::Expr(exp) => semantic_token_from_expr(exp, semantic_tokens),
    }
}
