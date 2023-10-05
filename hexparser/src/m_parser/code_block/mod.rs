use nom::{
    multi::{many0, separated_list1, many1},
    bytes::complete::{
        tag as just, take,
    },
    branch::alt as choice,
    sequence::{pair as then, terminated, delimited, preceded, separated_pair, tuple}, combinator::{map, opt}, Parser,
};
use nom_supreme::ParserExt;

use crate::{m_parser::{function::function_statement, operations::mathematical_expression, member}, combinators::{map_with_span, spanned}, token::{Token, Tokens, Spanned, Keyword}, Expr, recovery_err::{TokResult, expression_recovery, non_opt, TokError, statement_recovery}, Value};

use super::{case_parameters, parameters, MatchBranch, Statement};


pub(crate) mod function_statement;
pub(crate) mod member;
pub(crate) mod bitfield;

fn conditional<'a, 'b, P>(input: Tokens<'a, 'b>, member_parser: P) -> TokResult<'a, 'b, Spanned<Statement>>
where
    P: Parser<Tokens<'a, 'b>, Spanned<Statement>, TokError<'a,'b>> + Copy
{
    statement_recovery(map_with_span(
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
                            delimited(
                                just(Token::Separator('{')),
                                spanned(many0(member_parser)),
                                just(Token::Separator('}')).context("Missing }")
                            ),
                            spanned(map(
                                member_parser.context("Invalid expression"),
                                |a| vec![a]
                            )),
                        ))
                    ))
                ), |a, span| (a, span))
            ), |a, span| (a, span)),
            opt(preceded(
                just(Token::K(Keyword::Else)),
                choice((
                    delimited(
                        just(Token::Separator('{')),
                        spanned(many0(member_parser)),
                        non_opt(just(Token::Separator('}'))).context("Missing }")
                    ),
                    spanned(map(
                        member_parser.context("Invalid expression"),
                        |a| vec![a]
                    ))
                ))
            ))
        ),
        |(ifs, alternative), span| {
            let mut v = vec![];
            for ((test, consequent), span) in ifs.0 {
                v.push((Statement::If {
                    test: Box::new(test),
                    consequent,
                }, span))
            }
            let alternative = match alternative {
                Some(alt) => alt,
                None => (vec![], span.clone()),
            };
            (
                Statement::IfBlock {
                    ifs: Box::new((Expr::StatementList { list: v }, ifs.1)),
                    alternative
                },
                span
            )
        }
    ))(input)
}

fn statement_body<'a, 'b, P>(input: Tokens<'a, 'b>, member_parser: P) -> TokResult<'a, 'b, Spanned<Vec<Spanned<Statement>>>>
where
    P: Parser<Tokens<'a, 'b>, Spanned<Statement>, TokError<'a,'b>> + Copy
{
    choice((
        delimited(
            just(Token::Separator('{')),
            spanned(many0(member_parser)),
            just(Token::Separator('}'))
        ),
        spanned(map(
            member_parser,
            |a| vec![a]
        ))
    ))(input)
}

fn match_statement<'a, 'b, P>(input: Tokens<'a, 'b>, member_parser: P) -> TokResult<'a, 'b, Spanned<Statement>>
where
    P: Parser<Tokens<'a, 'b>, Spanned<Vec<Spanned<Statement>>>, TokError<'a,'b>> + Copy
{
    statement_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Match)),
            tuple((
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
            ))
        ),
        |(parameters, branches), span| {
            let branches = branches.into_iter().map(|(case, body)| {
                MatchBranch {
                    case,
                    body
                }
            }).collect();
            (Statement::Match {
                parameters,
                branches
            }, span)
        } // TODO
    ))(input)
}

fn try_catch<'a, 'b, P>(input: Tokens<'a, 'b>, member_parser: P) -> TokResult<'a, 'b, Spanned<Statement>>
where
    P: Parser<Tokens<'a, 'b>, Spanned<Statement>, TokError<'a,'b>> + Copy
{
    statement_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Try)),
            then(
                spanned(delimited(
                    just(Token::Separator('{')),
                    many0(member_parser),
                    just(Token::Separator('}'))
                )),
                opt(spanned(preceded(
                    just(Token::K(Keyword::Catch)),
                    delimited(
                        just(Token::Separator('{')),
                        many0(member_parser),
                        just(Token::Separator('}'))
                    )
                )))
            )
        ),
        |((try_block, try_span), catch_block), span| {
            let catch_block = catch_block.map(|(catch_block, catch_span)| {
                Box::new((Expr::StatementList { list: catch_block }, catch_span))
            });

            (Statement::TryCatch {
                try_block: Box::new((Expr::StatementList { list: try_block }, try_span)),
                catch_block,
            }, span) // TODO
        }
    ))(input)
}
