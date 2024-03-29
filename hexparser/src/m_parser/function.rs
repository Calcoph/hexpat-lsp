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
    sequence::{pair as then, terminated, delimited, preceded, separated_pair, tuple},
    multi::{
       many0,
       many1,
       separated_list1
    }
};
use nom_supreme::ParserExt;

use crate::{token::{Spanned, Tokens, Token, Keyword, ValueType}, combinators::{map_with_span, spanned}, m_parser::{ident, parse_type, mathematical_expression, FuncArgument, ident_local, AssignmentOp, BinaryOp, member_access, assignment_expr, HexTypeDef}, Expr, Value, recovery_err::{TokResult, expression_recovery, non_opt, statement_recovery}};

use super::{code_block, member_variable, Statement, Definition, function_call_statement};

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
                            tuple((
                                just(Token::Separator('.')),
                                just(Token::Separator('.')),
                                just(Token::Separator('.')),
                            )),
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

pub(crate) fn function_definition<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
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
        |(name, (args, body)), span| (
            Statement::Func {
                name,
                args,
                body
            },
            span
        )
    ))(input)
}

pub(crate) fn function_variable_decl<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(choice((
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
                    Statement::Definition(Definition {
                        value_type,
                        name: Box::new(name),
                        body
                    }),
                    span
                )
            }
        ),
        map_with_span(
            then(
                member_variable,
                opt(preceded(
                    just(Token::Op("=")),
                    non_opt(mathematical_expression.context("Expected mathematical expression"))
                ))
            ),
            |(expr, body), span| {
                match expr.0 {
                    Statement::ArrayDefinition { value_type, array_name, size, body: body_ } => match body {
                        Some(_) => (Statement::ArrayDefinition { value_type, array_name, size, body: body_ }, expr.1), // TODO: This should throw an error of some kind
                        None => (Statement::ArrayDefinition { value_type, array_name, size, body: body_ }, expr.1)
                    },
                    Statement::Definition(Definition { value_type, name, body: body_ }) => match body {
                        Some(body) => {
                            let a = body_.0;
                            if let Expr::Value{ val: Value::Null } = a {
                            } else {
                                // TODO: This should throw an error of some kind
                            };
                            (Statement::Definition(Definition { value_type, name, body: Box::new(body) }), span)
                        },
                        None => (Statement::Definition(Definition { value_type, name, body: body_ }), expr.1)
                    }
                    _ => unreachable!()
                }
            }
        )
    )))(input)
}

pub(crate) fn function_while_loop<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::While)),
            then(
                delimited(
                    just(Token::Separator('(')).context("Missing ("),
                    mathematical_expression.context("Expected boolean expression"),
                    just(Token::Separator(')')).context("Missing )"),
                ),
                code_block::function_statement::statement_body.context("Expected expression")
            )
        ),
        |(condition, body), span| (
            Statement::WhileLoop {
                condition: Box::new(condition),
                body
            },
            span
        )
    ))(input)
}

pub(crate) fn function_for_loop<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
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
                code_block::function_statement::statement_body.context("Expected expression")
            )
        ),
        |((var_init,
            (var_test, var_change)),
            body
         ),
          span|
        (
            Statement::ForLoop {
                var_init: Box::new(var_init),
                var_test: Box::new(var_test),
                var_change: Box::new(var_change),
                body
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
                Some(name) => Expr::Definition(Definition { value_type, name: Box::new(name), body }),
                None => {
                    let (HexTypeDef{ endianness: _, name: value_type }, _) = value_type;
                    Expr::UnnamedParameter { type_: value_type }
                },
            };

            (FuncArgument::Parameter(Box::new((expr, span.clone()))), span)
        }
    )(input)
}

pub(crate) fn function_statements<'a, 'b: 'a>() -> (
    impl FnMut(Tokens<'a,'b>) -> TokResult<'a, 'b, Spanned<Statement>>,
    impl FnMut(Tokens<'a,'b>) -> TokResult<'a, 'b, Spanned<Statement>>
) {
    let semicolon_expr = choice((
        assignment_expr,
        function_controlflow_statement,
        function_assignment,
        function_call_statement,
        preceded( // TODO: Remove preceded so const is taken into account
            opt(just(Token::K(Keyword::Const))),
            function_variable_decl
        )
    ));
    let no_semicolon_expr = choice((
        code_block::function_statement::conditional,
        code_block::function_statement::match_statement,
        code_block::function_statement::try_catch,
        function_while_loop,
        function_for_loop,
    ));

    (semicolon_expr, no_semicolon_expr)
}

pub(crate) fn function_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    let (semicolon_expr, no_semicolon_expr) = function_statements();

    statement_recovery(choice((
        terminated(
            semicolon_expr,
            many1(just(Token::Separator(';'))).context("Missing ;")
        ),
        no_semicolon_expr,
    )))(input)
}

pub(crate) fn function_statement_semicolonless<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    let (semicolon_expr, no_semicolon_expr) = function_statements();
    statement_recovery(choice((
        semicolon_expr,
        no_semicolon_expr,
    )))(input)
}

fn function_assignment<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
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
            Statement::Assignment {
                loperand: Box::new(loperand),
                operator: AssignmentOp::Just,
                roperand: Box::new(roperand)
            },
            span
        )
    ))(input)
}

pub(crate) fn function_controlflow_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(choice(( // TODO: Probably this can do a more personalized recovery, instead of expression_recovery
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
                let expr = Statement::Return { value: Box::new(value) };

                (expr, span)
            }
        ),
        map_with_span(
            just(Token::K(Keyword::Break)),
            |_, span| (Statement::Break, span)
        ),
        map_with_span(
            just(Token::K(Keyword::Continue)),
            |_, span| (Statement::Continue, span)
        ),
    )))(input)
}
