use nom::{
    multi::{many0, separated_list1, many1},
    bytes::complete::{
        tag as just, take,
    },
    branch::alt as choice,
    sequence::{pair as then, terminated, delimited, preceded, separated_pair, tuple}, combinator::{map, opt}, Parser,
};
use nom_supreme::ParserExt;

use crate::{m_parser::{function::function_statement, operations::mathematical_expression, member}, combinators::{map_with_span, spanned}, token::{Token, Tokens, Spanned, Keyword}, Expr, recovery_err::{TokResult, expression_recovery, non_opt, TokError}, Value};

use super::{case_parameters, parameters};


pub(crate) mod function_statement;
pub(crate) mod member;
pub(crate) mod bitfield;

fn conditional<'a, 'b, P>(input: Tokens<'a, 'b>, member_parser: P) -> TokResult<'a, 'b, Spanned<Expr>>
where
    P: Parser<Tokens<'a, 'b>, Spanned<Expr>, TokError<'a,'b>> + Copy
{
    expression_recovery(map_with_span(
        then(
            map_with_span(separated_list1(
                just(Token::K(Keyword::Else)),
                map_with_span(preceded(
                    just(Token::K(Keyword::If)),
                    tuple((
                        delimited(
                            just(Token::Separator('(')).context("Missing ("),
                            mathematical_expression,
                            just(Token::Separator(')')).context("Missing )")
                        ),
                        choice((
                            map(
                                delimited(
                                    just(Token::Separator('{')),
                                    spanned(many0(member_parser)),
                                    just(Token::Separator('}')).context("Missing }")
                                ),
                                |(list, span)| (Expr::ExprList { list }, span)
                            ),
                            member_parser.context("Invalid expression"),
                        ))
                    ))
                ), |a, span| (a, span))
            ), |a, span| (a, span)),
            opt(preceded(
                just(Token::K(Keyword::Else)),
                choice((
                    map(
                        delimited(
                            just(Token::Separator('{')),
                            spanned(many0(member_parser)),
                            non_opt(just(Token::Separator('}'))).context("Missing }")
                        ),
                        |(list, span)| (Expr::ExprList { list }, span)
                    ),
                    member_parser.context("Invalid expression")
                ))
            ))
        ),
        |(ifs, alternative), span| {
            let mut v = vec![];
            for ((test, consequent), span) in ifs.0 {
                v.push((Expr::If {
                    test: Box::new(test),
                    consequent: Box::new(consequent),
                }, span))
            }
            let alternative = Box::new(match alternative {
                Some(alt) => alt,
                None => (Expr::Value { val: Value::Null }, span.clone()),
            });
            (
                Expr::IfBlock {
                    ifs: Box::new((Expr::ExprList { list: v }, ifs.1)),
                    alternative
                },
                span
            )
        }
    ))(input)
}

fn statement_body<'a, 'b, P>(input: Tokens<'a, 'b>, member_parser: P) -> TokResult<'a, 'b, Spanned<Expr>>
where
    P: Parser<Tokens<'a, 'b>, Spanned<Expr>, TokError<'a,'b>> + Copy
{
    choice((
        map_with_span(
            delimited(
                just(Token::Separator('{')),
                many0(member_parser),
                just(Token::Separator('}'))
            ),
            |list, span| (Expr::ExprList { list }, span)
        ),
        member_parser
    ))(input)
}

fn match_statement<'a, 'b, P>(input: Tokens<'a, 'b>, member_parser: P) -> TokResult<'a, 'b, Spanned<Expr>>
where
    P: Parser<Tokens<'a, 'b>, Spanned<Expr>, TokError<'a,'b>> + Copy
{
    expression_recovery(map_with_span(
        tuple((
            just(Token::K(Keyword::Match)),
            parameters,
            delimited(
                just(Token::Separator('{')),
                many1(separated_pair(
                    case_parameters,
                    just(Token::Op(":")),
                    member_parser
                )),
                just(Token::Separator('}'))
            )
        )),
        |(a, b, c), span| (Expr::Match, span) // TODO
    ))(input)
}

fn try_catch<'a, 'b, P>(input: Tokens<'a, 'b>, member_parser: P) -> TokResult<'a, 'b, Spanned<Expr>>
where
    P: Parser<Tokens<'a, 'b>, Spanned<Expr>, TokError<'a,'b>> + Copy
{
    expression_recovery(map_with_span(
        tuple((
            just(Token::K(Keyword::Try)),
            delimited(
                just(Token::Separator('{')),
                many0(member_parser),
                just(Token::Separator('}'))
            ),
            opt(then(
                just(Token::K(Keyword::Catch)),
                delimited(
                    just(Token::Separator('{')),
                    many0(member_parser),
                    just(Token::Separator('}'))
                )
            ))
        )),
        |a, span| (Expr::TryCatch, span) // TODO
    ))(input)
}