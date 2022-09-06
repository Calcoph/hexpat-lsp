use std::ops::Range;

use nom::{error::ParseError, Parser};

use crate::{token::Spanned, recovery_err::{IResult, ToRange}};

pub fn spanned<I, O, E: ParseError<I>, F>(
    mut parser: F
) -> impl FnMut(I) -> IResult<I, Spanned<O>, E>
where
  F: Parser<I, O, E>,
  I: ToRange
{
    move |i: I| {
        let full_span = i.span();
        let (remaining, o) = parser.parse(i)?;
        let span = full_span.start..remaining.span().start;

        Ok((remaining, (o, span)))
    }
}

pub fn map_with_span<I, O1, O2, E: ParseError<I>, F, G>(
    mut parser: F,
    mut mapper: G
) -> impl FnMut(I) -> IResult<I, O2, E>
where
  F: Parser<I, O1, E>,
  I: ToRange,
  G: FnMut(O1, Range<usize>) -> O2,
{
    move |i: I| {
        let full_span = i.span();
        let (remaining, o) = parser.parse(i)?;
        let span = full_span.start..remaining.span().start;

        Ok((remaining, mapper(o, span)))
    }
}

pub fn ignore<I, O, E: ParseError<I>, F>(
    mut parser: F
) -> impl FnMut(I) -> IResult<I, (), E>
where
  F: Parser<I, O, E>,
{
    move |i: I| {
        let (remaining, _) = parser.parse(i)?;

        Ok((remaining, ()))
    }
}

pub fn to<I, O1, O2, E: ParseError<I>, F>(
    mut parser: F,
    value: O2
) -> impl FnMut(I) -> IResult<I, Spanned<O2>, E>
where
  F: Parser<I, O1, E>,
  I: ToRange,
  O2: Clone
{
    move |i: I| {
        let full_span = i.span();
        let (remaining, _) = parser.parse(i)?;
        let span = full_span.start..remaining.span().start;

        Ok((remaining, (value.clone(), span)))
    }
}
