use std::{ops::Range, cell::RefCell};

use nom::Compare;
use nom_locate::LocatedSpan;

use crate::{m_lexer::Token, recovery_err::{ParseState, RecoveredError}};

pub type Spanned<T> = (T, Range<usize>);

pub type TokSpan<'a> = LocatedSpan<Token, (ParseState<'a>, usize)>;

pub trait FromStrSpan<'a> {
    fn from_strspan(token: Token, state: ParseState<'a>, span: Range<usize>) -> TokSpan<'a>;
}

impl<'a> FromStrSpan<'a> for TokSpan<'a> {
    #[inline]
    fn from_strspan(token: Token, state: ParseState<'a>, span: Range<usize>) -> TokSpan<'a> {
        unsafe{TokSpan::new_from_raw_offset(span.start, 0, token, (state, span.end-span.start))}
    }
}