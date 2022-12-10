use nom::{
    branch::alt as choice,
    bytes::complete::{
        tag as just
    },
    combinator::{
        map,
        peek,
        not,
        opt
    },
    sequence::{pair as then, terminated, delimited, preceded, separated_pair},
    multi::{
       many0,
       many1,
       separated_list1
    }
};
use nom_supreme::ParserExt;

use crate::{token::{Spanned, Tokens, Token, Keyword, ValueType}, combinators::{map_with_span, spanned}, m_parser::{ident, parse_type, mathematical_expression, statement_body, FuncArgument, ident_local, Assignment, BinaryOp, member_access, function_call, assignment_expr, HexTypeDef}, Expr, Value, recovery_err::{TokResult, expression_recovery, non_opt}};

fn function_arguments<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Vec<Spanned<FuncArgument>>>> {
    map_with_span(
        choice((
            then(
                separated_list1( // TODO: maybe also parse parameter packs here, with the sole purpose of giving better error messages
                    just(Token::Separator(',')),
                    func_arg
                ),
                opt(preceded(
                    just(Token::Separator(',')),
                    map_with_span(
                        separated_pair(
                            just(Token::V(ValueType::Auto)), // TODO: Maybe don't ignore this
                            then(
                                just(Token::Separator('.')),
                                then(
                                    just(Token::Separator('.')),
                                    just(Token::Separator('.')),
                                )
                            ),
                            ident
                        ),
                        |(_, name), span| {
                            (FuncArgument::ParameterPack(name), span)
                        }
                    )
                ))
            ),
            map(
                opt(map_with_span(
                    separated_pair(
                        just(Token::V(ValueType::Auto)), // TODO: Maybe don't ignore this
                        then(
                            just(Token::Separator('.')),
                            then(
                                just(Token::Separator('.')),
                                just(Token::Separator('.')),
                            )
                        ),
                        ident
                    ),
                    |(_, name), span| {
                        (FuncArgument::ParameterPack(name), span)
                    }
                )),
                |a| (vec![], a)
            ),
        )),
        |(mut args, arg_pack), span| {
            match arg_pack {
                Some(arg_pack) => args.push(arg_pack),
                None => (),
            };

            (args, span)
        }
    )(input)
}

pub(crate) fn function_definition<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Fn)),
            then(
                ident.context("Expected function name"),
                then(
                    delimited(
                        just(Token::Separator('(')).context("Missing )"),
                        function_arguments,
                        just(Token::Separator(')')).context("Expected ) or valid function arguments")
                    ),
                    delimited(
                        just(Token::Separator('{')).context("Missing {"),
                        spanned(many0(function_statement)),
                        just(Token::Separator('}')).context("Expected } or valid function expression")
                    )
                )
            )
        ),
        |(name, (args, (list, body_span))), span| (
            Expr::Func {
                name,
                args,
                body: Box::new((Expr::ExprList { list }, body_span))
            },
            span
        )
    ))(input)
}

pub(crate) fn function_variable_decl<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(choice((
        map_with_span(
            then(
                then(
                    parse_type,
                    ident_local.context("Expected variable name")
                ),
                delimited(
                    then(
                        just(Token::Separator('[')),
                        not(just(Token::Separator('['))),
                    ),
                    opt(choice((
                        map_with_span(
                            preceded(
                                just(Token::K(Keyword::While)),
                                non_opt(delimited(
                                    just(Token::Separator('(')).context("Missing ("),
                                    mathematical_expression.context("Expected boolean expression"),
                                    just(Token::Separator(')')).context("Missing )")
                                ))
                            ),
                            |condition, span| (
                                Expr::WhileLoop {
                                    condition: Box::new(condition),
                                    body: Box::new((Expr::Value { val: Value::Null }, span.clone()))
                                },
                                span
                            )
                        ),
                        mathematical_expression
                    ))),
                    just(Token::Separator(']')).context("Missing ]")
                )
            ),
            |((value_type, name), body), span| {
                let body = Box::new(match body {
                    Some(expr) => expr,
                    None => (Expr::Value { val: Value::Null }, span.clone())
                });
                (
                    Expr::Definition {
                        value_type,
                        name: Box::new(name),
                        body
                    },
                    span
                )
            }
        ),
        map_with_span(
            then(
                parse_type,
                then(
                    separated_list1(
                        just(Token::Separator(',')),
                        ident_local
                    ).context("Expected variable name"),
                    opt(preceded(
                        just(Token::Op("=")),
                        non_opt(mathematical_expression.context("Expected mathematical expression"))
                    ))
                )
            ),
            |(value_type, (mut names, body)), span| {
                let body = Box::new(match body {
                    Some(body) => body,
                    None => (Expr::Value { val: Value::Null }, span.clone()),
                });
                let names_span = names.get(0).unwrap().1.start..names.get(names.len()-1).unwrap().1.end;
                match names.len() {
                    1 => (
                        Expr::Definition {
                            value_type,
                            name: Box::new((names.pop().unwrap().0, names_span)),
                            body
                        },
                        span
                    ),
                    _ => (
                        Expr::Definition {
                            value_type,
                            name: Box::new((Expr::ExprList { list: names }, names_span)),
                            body
                        },
                        span
                    )
                }
            }
        )
    )))(input)
}

pub(crate) fn function_while_loop<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::While)),
            then(
                delimited(
                    just(Token::Separator('(')).context("Missing ("),
                    mathematical_expression.context("Expected boolean expression"),
                    just(Token::Separator(')')).context("Missing )"),
                ),
                statement_body.context("Expected expression")
            )
        ),
        |(condition, body), span| (
            Expr::WhileLoop {
                condition: Box::new(condition),
                body: Box::new(body)
            },
            span
        )
    ))(input)
}

pub(crate) fn function_for_loop<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::For)),
            then(
                delimited(
                    just(Token::Separator('(')).context("Missing ("),
                    separated_pair(
                        function_statement_semicolonless.context("Expected variable declaration"),
                        just(Token::Separator(',')).context("Missing ,"),
                        separated_pair(
                            mathematical_expression.context("Expected boolean expression"),
                            just(Token::Separator(',')).context("Missing ,"),
                            function_statement_semicolonless.context("Expected expression")
                        )
                    ),
                    just(Token::Separator(')')).context("Missing )"),
                ),
                statement_body.context("Expected expression")
            )
        ),
        |((var_init,
            (var_test, var_change)),
            body
         ),
          span|
        (
            Expr::ForLoop {
                var_init: Box::new(var_init),
                var_test: Box::new(var_test),
                var_change: Box::new(var_change),
                body: Box::new(body)
            },
            span
        )
    ))(input)
}

pub(crate) fn func_arg<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<FuncArgument>> {
    map_with_span(
        then(
            then(
                parse_type,
                preceded(
                    not(just(Token::Separator('.'))),
                    opt(ident_local)
                )
            ),
            opt(
                preceded(
                    just(Token::Op("=")),
                    mathematical_expression
                )
            )
        ),
        |((value_type, name), body), span| {
            let body = match body {
                Some(body) => Box::new(body),
                None => Box::new((Expr::Value { val: Value::Null }, span.clone())),
            };
            let expr = match name {
                Some(name) => Expr::Definition { value_type, name: Box::new(name), body },
                None => {
                    let (HexTypeDef{ endianness: _, name: value_type }, _) = value_type;
                    Expr::UnnamedParameter { type_: value_type }
                },
            };

            (FuncArgument::Parameter(Box::new((expr, span.clone()))), span)
        }
    )(input)
}

pub(crate) fn function_statements<'a, 'b: 'a>() -> (impl FnMut(Tokens<'a,'b>) -> TokResult<'a, 'b, Spanned<Expr>>, impl FnMut(Tokens<'a,'b>) -> TokResult<'a, 'b, Spanned<Expr>>) {
    let semicolon_expr = choice((
        assignment_expr,
        function_controlflow_statement,
        function_assignment,
        function_call,
        function_variable_decl,
    ));
    let no_semicolon_expr = choice((
        function_conditional,
        function_while_loop,
        function_for_loop,
    ));

    (semicolon_expr, no_semicolon_expr)
}

pub(crate) fn function_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    let (semicolon_expr, no_semicolon_expr) = function_statements();

    expression_recovery(choice((
        terminated(
            semicolon_expr,
            many1(just(Token::Separator(';'))).context("Missing ;")
        ),
        no_semicolon_expr,
    )))(input)
}

pub(crate) fn function_statement_semicolonless<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    let (semicolon_expr, no_semicolon_expr) = function_statements();
    expression_recovery(choice((
        semicolon_expr,
        no_semicolon_expr,
    )))(input)
}

fn function_assignment<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            peek(then(
                ident,
                choice((
                    just(Token::Separator('.')),
                    just(Token::Separator('[')),
                ))
            )),
            separated_pair(
                member_access,
                just(Token::Op("=")).context("Missing ="),
                mathematical_expression.context("Expected expression")
            )
        ),
        |(loperand, roperand), span| (
            Expr::Binary {
                loperand: Box::new(loperand),
                operator: BinaryOp::Assign(Assignment::Just),
                roperand: Box::new(roperand)
            },
            span
        )
    ))(input)
}

pub(crate) fn function_controlflow_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(choice(( // TODO: Probably this can do a more personalized recovery, instead of expression_recovery
        map_with_span(
            preceded(
                just(Token::K(Keyword::Return)),
                choice((
                    map(peek(just(Token::Separator(';'))), |_| None),
                    map(mathematical_expression, |a| Some(a))
                )).context("Expected ;")
            ),
            |value, span| {
                let value = match value {
                    Some(val) => val,
                    None => (Expr::Value { val: Value::Null }, span.clone()),
                };
                let expr = Expr::Return { value: Box::new(value) };

                (expr, span)
            }
        ),
        map_with_span(
            just(Token::K(Keyword::Break)),
            |_, span| (Expr::Break, span)
        ),
        map_with_span(
            just(Token::K(Keyword::Continue)),
            |_, span| (Expr::Continue, span)
        ),
    )))(input)
}

pub(crate) fn function_conditional<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::If)),
            then(
                delimited(
                    just(Token::Separator('(')).context("Missing ("),
                    mathematical_expression.context("Expected boolean expression"),
                    just(Token::Separator(')')).context("Missing )")
                ),
                then(
                    statement_body.context("Expected expression"),
                    opt(preceded(
                        just(Token::K(Keyword::Else)),
                        non_opt(statement_body).context("Expected expression")
                    ))
                )
            )
        ),
        |(test_,
            (consequent, alternative)
         ), span| {
            let alternative = Box::new(match alternative {
                Some(alt) => alt,
                None => (Expr::Value { val: Value::Null }, span.clone()),
            });
            (
                Expr::IfBlock { ifs: Box::new((Expr::If { test: Box::new(test_), consequent: Box::new(consequent) }, span.clone())), // TODO: Fix this mess (specially the span)
                    alternative
                },
                span
            )
        }
    ))(input)
}
