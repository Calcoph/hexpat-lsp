use std::{ops::Range, cell::RefCell, iter::{Enumerate, Copied}, slice::Iter};

use nom::{Compare, CompareResult, InputLength, InputIter};
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

#[derive(Debug, Clone, Copy)]
pub struct Tokens<'a> {
    tokens: &'a [TokSpan<'a>]
}

// impl AsBytes

impl<'a> Compare<Tokens<'a>> for Tokens<'a> {
    fn compare(&self, t: Tokens) -> CompareResult {
        let pos = self.tokens.iter().zip(t.tokens.iter()).position(|(a, b)| a.fragment() != b.fragment());

        match pos {
            Some(_) => CompareResult::Error,
            None => {
                if self.tokens.len() >= t.tokens.len() {
                    CompareResult::Ok
                } else {
                    CompareResult::Incomplete
                }
            }
        }
    }

    fn compare_no_case(&self, t: Tokens) -> CompareResult {
        todo!()
    }
}

// impl ExtendInto

// impl FindSubstring

// impl FindToken

/* impl<'a> InputIter for Tokens<'a> {
    type Item = TokSpan<'a>;

    type Iter = Enumerate<Self::IterElem>;

    type IterElem = Copied<Iter<'a, Self::Item>>;

    fn iter_indices(&self) -> Self::Iter {
        todo!()
    }

    fn iter_elements(&self) -> Self::IterElem {
        todo!()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
  where
    P: Fn(Self::Item) -> bool {
        todo!()
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        todo!()
    }
} */

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tokens.input_len()
    }
}

// impl InputTake

// impl InputTakeAtPosition

// impl Offset

// impl ParseTo

// impl Slice
