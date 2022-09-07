use nom::{
    IResult,
    branch::alt as choice,
    bytes::complete::{
        tag as just
    },
    combinator::{
        map,
        peek,
    },
    sequence::{
        pair as then, preceded,
    },
    multi::{
        many0, fold_many0,
    }
};

use crate::{token::{Spanned, Tokens, Token, Keyword}, combinators::{ignore, to, map_with_span}, m_parser::{value_type_any, parse_type, factor::factor, string_literal}, Expr};

fn cast_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map_with_span(
            preceded(
                peek(choice((
                    ignore(just(Token::K(Keyword::BigEndian))),
                    ignore(just(Token::K(Keyword::LittleEndian))),
                    ignore(value_type_any)
                ))),
                then(
                    parse_type,
                    preceded(
                        peek(just(Token::Separator('('))),
                        factor
                    )
                )
            ),
            |(casted_type, expr), span| (
                Expr::Cast {
                    cast_operator: casted_type,
                    operand: Box::new(expr)
                },
                span
            )
        ),
        factor
    ))(input)
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Add,
    Sub,
    LNot,
    BNot,
}

fn unary_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map(
            then(
                choice((
                    to(just(Token::Op("+")), UnaryOp::Add),
                    to(just(Token::Op("-")), UnaryOp::Sub),
                    to(just(Token::Op("!")), UnaryOp::LNot),
                    to(just(Token::Op("~")), UnaryOp::BNot)
                )),
                cast_expression
            ),
            |((operation, op_span), operand)| {
                let expr = Expr::Unary {
                    operation,
                    operand: Box::new(operand)
                };

                (expr, op_span.start..operand.1.end)
            }
        ),
        string_literal,
        cast_expression
    ))(input)
}

fn multiplicative_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            unary_expression,
            fold_many0(
                preceded(
                    choice((
                        just(Token::Op("*")),
                        just(Token::Op("/")),
                        just(Token::Op("%"))
                    )),
                    unary_expression
                ),
                || 0,
                |a, b| todo!()
            )
        ),
        |(a, b)| a // TODO
    )(input)
}

fn additive_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            multiplicative_expression,
            many0(then(
                choice((
                    just(Token::Op("+")),
                    just(Token::Op("-"))
                )),
                multiplicative_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn shift_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            additive_expression,
            many0(then(
                choice((
                    just(Token::Op("<<")),
                    just(Token::Op(">>"))
                )),
                additive_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn binary_and_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            shift_expression,
            many0(then(
                just(Token::Op("&")),
                shift_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn binary_xor_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            binary_and_expression,
            many0(then(
                just(Token::Op("^")),
                binary_and_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn binary_or_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            binary_xor_expression,
            many0(then(
                just(Token::Op("|")),
                binary_xor_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn relation_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            binary_or_expression,
            many0(then(
                choice((
                    just(Token::Op(">")),
                    just(Token::Op("<")),
                    just(Token::Op(">=")),
                    just(Token::Op("<="))
                )),
                binary_or_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn equality_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            relation_expression,
            many0(then(
                choice((
                    just(Token::Op("==")),
                    just(Token::Op("!="))
                )),
                relation_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn boolean_and<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            equality_expression,
            many0(then(
                just(Token::Op("&&")),
                equality_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn boolean_xor<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            boolean_and,
            many0(then(
                just(Token::Op("^^")),
                boolean_and
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn boolean_or<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            boolean_xor,
            many0(then(
                just(Token::Op("||")),
                boolean_xor
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn ternary_conditional<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            boolean_or,
            many0(then(
                just(Token::Op("?")),
                then(
                    boolean_or,
                    then(
                        just(Token::Op(":")),
                        boolean_or
                    )
                )
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

pub fn mathematical_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    ternary_conditional(input)
}
