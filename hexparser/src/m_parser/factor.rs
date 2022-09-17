use nom::{
    branch::alt as choice,
    bytes::complete::{
        tag as just,
    },
    combinator::{
        map,
        peek
    },
    sequence::{pair as then, delimited, preceded}
};
use nom_supreme::ParserExt;

use crate::{token::{Spanned, Tokens, Token, Keyword, BuiltFunc}, combinators::{ignore, map_with_span, to}, m_parser::{numeric, operations::mathematical_expression, namespace_resolution, ident, value_type_any, member_access, function_call}, Expr, recovery_err::TokResult};

pub(crate) fn factor<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    choice((
        numeric,
        unary,
        delimited(
            just(Token::Separator('(')),
            mathematical_expression.context("Expected mathematical expression"),
            just(Token::Separator(')')).context("Missing )")
        ),
        function_call,
        preceded(
            peek(then(
                ident,
                just(Token::Op("::"))
            )),
            namespace_resolution
        ),
        member_access,
        special_variables,
        map_with_span(
            just(Token::Op("$")),
            |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
        ),
        builtin_func
    ))(input)
}

fn unary<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    preceded(
        peek(choice((
            just(Token::Op("+")),
            just(Token::Op("-")),
            just(Token::Op("~")),
            just(Token::Op("!"))
        ))),
        mathematical_expression
    )(input)
}

fn builtin_func<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_with_span(
        then(
            choice((
                to(ignore(just(Token::B(BuiltFunc::AddressOf))), String::from("addressof")),
                to(ignore(just(Token::B(BuiltFunc::SizeOf))), String::from("sizeof")),
            )),
            delimited(
                just(Token::Separator('(')),
                choice((
                    map(
                        then(
                            peek(choice((
                                ident,
                                map_with_span(
                                    just(Token::K(Keyword::Parent)),
                                    |_, span| (String::from("parent"), span)
                                ),
                                map_with_span(
                                    just(Token::K(Keyword::This)),
                                    |_, span| (String::from("this"), span)
                                )
                            ))),
                            member_access
                        ),
                        |(_, expr)| expr // TODO
                    ),
                    map(
                        value_type_any,
                        |(type_, span)| (
                            Expr::UnnamedParameter { type_: (type_, span.clone()) },
                            span
                        )
                    ),
                    map_with_span(
                        just(Token::Op("$")),
                        |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
                    ),
                )),
                just(Token::Separator(')'))
            )
        ),
        |((name, name_span), (argument, arg_span)), span| {
            let func_name = Box::new((Expr::Local { name: (name, name_span.clone()) }, name_span));
            (Expr::Call { func_name, arguments: (vec![(argument, arg_span.clone())], arg_span) }, span)
        }
    )(input)
}

fn special_variables<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_with_span(
        then(
            peek(choice((
                to(just(Token::K(Keyword::Parent)), String::from("parent")),
                to(just(Token::K(Keyword::This)), String::from("this"))
            ))),
            member_access
        ),
        |((name, name_span), member_access), span| {
            let item = Box::new((Expr::Local { name: (name, name_span.clone()) }, name_span));
            (
                Expr::Access {
                    item,
                    member: Box::new(member_access)
                },
                span
            )
        }
    )(input)
}
