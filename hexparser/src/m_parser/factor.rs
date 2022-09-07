use nom::{
    IResult,
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

use crate::{token::{Spanned, Tokens, Token, Keyword, BuiltFunc}, combinators::{ignore, map_with_span, to}, m_parser::{numeric, operations::mathematical_expression, namespace_resolution, old_member_access, ident, value_type_any, member_access, function_call}, Expr};

pub fn factor<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        numeric,
        unary,
        delimited(
            just(Token::Separator('(')),
            mathematical_expression,
            just(Token::Separator(')'))
        ),
        function_call,
        member_access, // TODO: make a member_access variant that starts with a namespace_resolution
        namespace_resolution,
        special_variables,
        map_with_span(
            just(Token::Op("$")),
            |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
        ),
        builtin_func
    ))(input)
}

fn unary<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
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

fn builtin_func<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
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
                            choice((
                                ident,
                                map_with_span(
                                    just(Token::K(Keyword::Parent)),
                                    |_, span| (String::from("parent"), span)
                                ),
                                map_with_span(
                                    just(Token::K(Keyword::This)),
                                    |_, span| (String::from("this"), span)
                                )
                            )),
                            old_member_access
                        ),
                        |(_, expr)| expr // TODO
                    ),
                    map(
                        value_type_any,
                        |(type_, span)| (
                            Expr::UnnamedParameter { type_: (type_, span) },
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
            let func_name = Box::new((Expr::Local { name: (name, name_span) }, name_span));
            (Expr::Call { func_name, arguments: (vec![(argument, arg_span)], arg_span) }, span)
        }
    )(input)
}

fn special_variables<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        then(
            choice((
                to(just(Token::K(Keyword::Parent)), String::from("parent")),
                to(just(Token::K(Keyword::This)), String::from("this"))
            )),
            old_member_access
        ),
        |((name, name_span), member_access), span| {
            let item = Box::new((Expr::Local { name: (name, name_span) }, name_span));
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
