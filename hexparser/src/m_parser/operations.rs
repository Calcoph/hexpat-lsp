use nom::{
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
    multi::fold_many0
};

use crate::{token::{Spanned, Tokens, Token, Keyword}, combinators::{ignore, to, map_with_span, fold_many0_once}, m_parser::{value_type_any, parse_type, factor::factor, string_literal, BinaryOp}, Expr, recovery_err::TokResult};

fn cast_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

fn unary_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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
                let span = op_span.start..operand.1.end;
                let expr = Expr::Unary {
                    operation,
                    operand: Box::new(operand)
                };

                (expr, span)
            }
        ),
        string_literal,
        cast_expression
    ))(input)
}

fn binary_fold((loperand, l_span): Spanned<Expr>, ((operator, _), (roperand, r_span)): (Spanned<BinaryOp>, Spanned<Expr>)) -> Spanned<Expr>{
    (
        Expr::Binary {
            loperand: Box::new((loperand, l_span.clone())),
            operator,
            roperand: Box::new((roperand, r_span.clone()))
        },
        l_span.start..r_span.end
    )
}

fn multiplicative_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = unary_expression(input)?;
    fold_many0_once(
        then(
            choice((
                to(just(Token::Op("*")), BinaryOp::Mul),
                to(just(Token::Op("/")), BinaryOp::Div),
                to(just(Token::Op("%")), BinaryOp::Mod)
            )),
            unary_expression
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn additive_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = multiplicative_expression(input)?;
    fold_many0_once(
        then(
            choice((
                to(just(Token::Op("+")), BinaryOp::Add),
                to(just(Token::Op("-")), BinaryOp::Sub)
            )),
            multiplicative_expression
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn shift_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = additive_expression(input)?;
    fold_many0_once(
        then(
            choice((
                to(just(Token::Op("<<")), BinaryOp::LShift),
                to(just(Token::Op(">>")), BinaryOp::RShift)
            )),
            additive_expression
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn binary_and_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = shift_expression(input)?;
    fold_many0_once(
        then(
            to(just(Token::Op("&")), BinaryOp::BAnd),
            shift_expression
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn binary_xor_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = binary_and_expression(input)?;
    fold_many0_once(
        then(
            to(just(Token::Op("^")), BinaryOp::BXor),
            binary_and_expression
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn binary_or_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = binary_xor_expression(input)?;
    fold_many0_once(
        then(
            to(just(Token::Op("|")), BinaryOp::BOr),
            binary_xor_expression
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn relation_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = binary_or_expression(input)?;
    fold_many0_once(
        then(
            choice((
                to(just(Token::Op(">")), BinaryOp::Greater),
                to(just(Token::Op("<")), BinaryOp::Less),
                to(just(Token::Op(">=")), BinaryOp::GreaterEqual),
                to(just(Token::Op("<=")), BinaryOp::LessEqual)
            )),
            binary_or_expression
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn equality_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = relation_expression(input)?;
    fold_many0_once(
        then(
            choice((
                to(just(Token::Op("==")), BinaryOp::Eq),
                to(just(Token::Op("!=")), BinaryOp::NotEq)
            )),
            relation_expression
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn boolean_and<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = equality_expression(input)?;
    fold_many0_once(
        then(
            to(just(Token::Op("&&")), BinaryOp::LAnd),
            equality_expression
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn boolean_xor<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = boolean_and(input)?;
    fold_many0_once(
        then(
            to(just(Token::Op("^^")), BinaryOp::LXor),
            boolean_and
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn boolean_or<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = boolean_xor(input)?;
    fold_many0_once(
        then(
            to(just(Token::Op("||")), BinaryOp::LOr),
            boolean_xor
        ),
        || first_expr,
        binary_fold
    )(input)
}

fn ternary_conditional<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_expr) = boolean_or(input)?;
    fold_many0_once(
        preceded(
            just(Token::Op("?")),
            then(
                boolean_or,
                preceded(
                    just(Token::Op(":")),
                    boolean_or
                )
            )
        ),
        || first_expr,
        |(loperand, l_span), (moperand, (roperand, r_span))| (
            Expr::Ternary {
                loperand: Box::new((loperand, l_span.clone())),
                moperand: Box::new(moperand),
                roperand: Box::new((roperand, r_span.clone()))
            },
            l_span.start..r_span.end
        )
    )(input)
}

pub fn mathematical_expression<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    ternary_conditional(input)
}
