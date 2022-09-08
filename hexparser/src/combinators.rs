use std::ops::Range;

use nom::{error::{ParseError, ErrorKind}, Parser, InputLength};
use nom::Err;
use crate::{token::Spanned, recovery_err::{StrResult, ToRange}};

pub fn spanned<I, O, E: ParseError<I>, F>(
    mut parser: F
) -> impl FnMut(I) -> StrResult<I, Spanned<O>, E>
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
) -> impl FnMut(I) -> StrResult<I, O2, E>
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
) -> impl FnMut(I) -> StrResult<I, (), E>
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
) -> impl FnMut(I) -> StrResult<I, Spanned<O2>, E>
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

pub fn fold_many0_once<I, O, E, F, G, H, R>(
    mut f: F,
    init: H,
    mut g: G,
  ) -> impl FnOnce(I) -> StrResult<I, R, E>
  where
    I: Clone + InputLength,
    F: Parser<I, O, E>,
    G: FnMut(R, O) -> R,
    H: FnOnce() -> R,
    E: ParseError<I>,
  {
    move |i: I| {
      let mut res = init();
      let mut input = i;
  
      loop {
        let i_ = input.clone();
        let len = input.input_len();
        match f.parse(i_) {
          Ok((i, o)) => {
            // infinite loop check: the parser must always consume
            if i.input_len() == len {
              return Err(Err::Error(E::from_error_kind(input, ErrorKind::Many0)));
            }
  
            res = g(res, o);
            input = i;
          }
          Err(Err::Error(_)) => {
            return Ok((input, res));
          }
          Err(e) => {
            return Err(e);
          }
        }
      }
    }
  }
