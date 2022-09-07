use nom::{
    IResult,
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
        many0, many1, separated_list0, separated_list1
    }
};

use crate::{token::{Spanned, Tokens, Token, Keyword, ValueType}, combinators::map_with_span, m_parser::{ident, parse_type, mathematical_expression, statement_body, FuncArgument, ident_local, Assignment, BinaryOp, member_access, function_call, assignment_expr}, Expr, Value};


pub fn function_definition<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        preceded(
            just(Token::K(Keyword::Fn)),
            then(
                ident,
                then(
                    delimited(
                        just(Token::Separator('(')),
                        then(
                            ident,
                            then(
                                separated_list0( // TODO: maybe also parse parameter packs here, with the sole purpose of giving better error messages
                                    just(Token::Separator(',')),
                                    func_arg
                                ),
                                opt(preceded(
                                    just(Token::Separator(',')),
                                    choice((
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
                                        ),
                                        func_arg
                                    ))
                                ))
                            )
                        ),
                        just(Token::Separator(')'))
                    ),
                    delimited(
                        just(Token::Separator('{')),
                        many0(function_statement),
                        just(Token::Separator('}'))
                    )
                )
            )
        ),
        |a| a // TODO
    )(input);
    todo!()
}

pub fn function_variable_decl<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map_with_span(
            then(
                then(
                    parse_type,
                    ident_local
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
                                delimited(
                                    just(Token::Separator('(')),
                                    mathematical_expression,
                                    just(Token::Separator(')'))
                                )
                            ),
                            |condition, span| (
                                Expr::WhileLoop {
                                    condition: Box::new(condition),
                                    body: Box::new((Expr::Value { val: Value::Null }, span))
                                },
                                span
                            )
                        ),
                        mathematical_expression
                    ))),
                    just(Token::Separator(']'))
                )
            ),
            |((value_type, name), body), span| {
                let body = Box::new(match body {
                    Some(expr) => expr,
                    None => (Expr::Value { val: Value::Null }, span)
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
                    ),
                    opt(preceded(
                        just(Token::Op("=")),
                        mathematical_expression
                    ))
                )
            ),
            |(value_type, (names, body)), span| {
                let body = Box::new(match body {
                    Some(body) => body,
                    None => (Expr::Value { val: Value::Null }, span),
                });
                let names_span = names.get(0).unwrap().1.start..names.get(names.len()-1).unwrap().1.end;

                (
                    Expr::Definition {
                        value_type,
                        name: Box::new((Expr::ExprList { list: names }, names_span)),
                        body
                    },
                    span
                )
            }
        )
    ))(input)
}

pub fn function_while_loop<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::While)),
            then(
                delimited(
                    just(Token::Separator('(')),
                    mathematical_expression,
                    just(Token::Separator(')')),
                ),
                statement_body
            )
        ),
        |(condition, body), span| (
            Expr::WhileLoop {
                condition: Box::new(condition),
                body: Box::new(body)
            },
            span
        )
    )(input)
}

pub fn function_for_loop<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::For)),
            then(
                delimited(
                    just(Token::Separator('(')),
                    separated_pair(
                        function_statement,
                        just(Token::Separator(',')),
                        separated_pair(
                            mathematical_expression,
                            just(Token::Separator(',')),
                            function_statement
                        )
                    ),
                    just(Token::Separator(')')),
                ),
                statement_body
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
    )(input)
}

pub fn func_arg<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<FuncArgument>> {
    map_with_span(
        then(
            then(
                parse_type,
                opt(ident_local)
            ),
            opt(
                preceded(
                    just(Token::Op("=")),
                    mathematical_expression
                )
            )
        ),
        |((value_type, name), body), span| {
            let name = match name {
                Some(name) => Box::new(name),
                None => Box::new((Expr::Value { val: Value::Null }, span)),
            };
            let body = match body {
                Some(body) => Box::new(body),
                None => Box::new((Expr::Value { val: Value::Null }, span)),
            };
            let expr = Expr::Definition { value_type, name, body };

            (FuncArgument::Parameter(Box::new((expr, span))), span)
        }
    )(input)
}

pub fn function_statement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    terminated(
        choice((
            assignment_expr,
            function_controlflow_statement,
            function_conditional,
            function_while_loop,
            function_for_loop,
            function_assignment,
            function_call,
            function_variable_decl,
        )),
        many1(just(Token::Separator(';')))
    )(input)
}

fn function_assignment<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
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
                just(Token::Op("=")),
                mathematical_expression
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
    )(input)
}

pub fn function_controlflow_statement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map_with_span(
            preceded(
                just(Token::K(Keyword::Return)),
                choice((
                    map(peek(just(Token::Separator(';'))), |_| None),
                    map(mathematical_expression, |a| Some(a))
                ))
            ),
            |value, span| {
                let value = match value {
                    Some(val) => val,
                    None => (Expr::Value { val: Value::Null }, span),
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
    ))(input)
}

pub fn function_conditional<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::If)),
            then(
                delimited(
                    just(Token::Separator('(')),
                    mathematical_expression,
                    just(Token::Separator(')'))
                ),
                then(
                    statement_body,
                    opt(preceded(
                        just(Token::K(Keyword::Else)),
                        statement_body
                    ))
                )
            )
        ),
        |(test_,
            (consequent, alternative)
         ), span| {
            let alternative = Box::new(match alternative {
                Some(alt) => alt,
                None => (Expr::Value { val: Value::Null }, span),
            });
            (
                Expr::If {
                    test_: Box::new(test_),
                    consequent: Box::new(consequent),
                    alternative
                },
                span
            )
        }
    )(input)
}
