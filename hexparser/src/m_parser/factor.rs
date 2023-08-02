use nom::{
    branch::alt as choice,
    bytes::complete::{
        tag as just,
    },
    combinator::{
        map,
        peek, opt
    },
    sequence::{pair as then, delimited, preceded, tuple}, Parser
};
use nom_supreme::ParserExt;

use crate::{token::{Spanned, Tokens, Token, Keyword, BuiltFunc}, combinators::{ignore, map_with_span, to}, m_parser::{numeric, operations::mathematical_expression, namespace_resolution, ident, value_type_any, member_access, function_call, parse_type}, Expr, recovery_err::{TokResult, TokError}};

use super::{custom_type, custom_type_parameters, HexTypeDef, Endianness};

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

fn builtin_func_inner<'a, 'b>(input: Tokens<'a, 'b>, post_ident: impl Parser<Tokens<'a,'b>,Spanned<Expr>,TokError<'a,'b>>, optional_parser: impl Parser<Tokens<'a,'b>,Spanned<Expr>,TokError<'a,'b>>) -> TokResult<'a, 'b, Spanned<Expr>> {
    delimited(
        just(Token::Separator('(')),
        choice((
            preceded(
                peek(ident),
                post_ident
            ),
            preceded(
                peek(choice((
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
            map_with_span(
                just(Token::Op("$")),
                |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
            ),
            optional_parser
        )),
        just(Token::Separator(')'))
    )(input)
}

fn builtin_func<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    // TODO: Make sure that it does the same as the original

    map_with_span(
        choice((
            then(
                to(ignore(just(Token::B(BuiltFunc::AddressOf))), String::from("addressof")),
                |input2| builtin_func_inner(
                    input2,
                    member_access,
                    // This parser will never return Ok() since there are not 'Q' separators
                    // TODO: find a better way to do this without "impossible" parser
                    to(just(Token::Separator('Q')), Expr::Value { val: crate::Value::Null })
                )
            ),
            then(
                to(ignore(just(Token::B(BuiltFunc::SizeOf))), String::from("sizeof")),
                |input2| builtin_func_inner(
                    input2,
                    map(
                        tuple((
                            namespace_resolution,
                            choice((
                                custom_type_parameters,
                                member_access
                            ))
                        )),
                        |((a, a_span), c)| (Expr::Value { val: crate::Value::Null }, a_span) 
                    ),
                    map(
                        value_type_any,
                        |type_name| (
                            Expr::Type { val: HexTypeDef { endianness: Endianness::Unkown, name: type_name.clone() } },
                            type_name.1
                        )
                    )
                )
            ),
            then(
                to(ignore(just(Token::B(BuiltFunc::TypeNameOf))), String::from("typenameof")),
                |input2| builtin_func_inner(
                    input2,
                    map(
                        tuple((
                            namespace_resolution,
                            choice((
                                custom_type_parameters,
                                member_access
                            ))
                        )),
                        |((a, a_span), c)| (Expr::Value { val: crate::Value::Null }, a_span) 
                    ),
                    map(
                        value_type_any,
                        |type_name| (
                            Expr::Type { val: HexTypeDef { endianness: Endianness::Unkown, name: type_name.clone() } },
                            type_name.1
                        )
                    )
                )
            )
        )),
        |((name, name_span), (argument, arg_span)), span| {
            let func_name = Box::new((Expr::Local { name: (name, name_span.clone()) }, name_span));
            (Expr::Call { func_name, arguments: (vec![(argument, arg_span.clone())], arg_span) }, span)
        }
    )(input)
}
