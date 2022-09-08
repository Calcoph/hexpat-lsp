use std::{cell::RefCell, ops::Range, error::Error};

use nom_locate::LocatedSpan;
//use nom::error::{ParseError, ErrorKind, FromExternalError};
use nom_supreme::error::{ErrorTree, GenericErrorTree};

use crate::token::{TokSpan, Tokens};

pub type StrResult<I, O, E=ErrorTree<I>> = Result<(I, O), nom::Err<E>>;
pub type TokError<'a> = GenericErrorTree<Tokens<'a>, &'a [TokSpan<'a>], TokSpan<'a>, Box<dyn Error + 'a>>;
pub type TokResult<'a, I, O, E=TokError<'a>> = Result<(I, O), nom::Err<E>>;

/// Carried around in the `LocatedSpan::extra` field in
/// between `nom` parsers.
#[derive(Clone, Copy, Debug)]
pub struct ParseState<'a>(pub &'a RefCell<Vec<RecoveredError>>);

unsafe impl<'a> Sync for ParseState<'a> {

}

impl<'a> ParseState<'a> {
    /// Pushes an error onto the errors stack from within a `nom`
    /// parser combinator while still allowing parsing to continue.
    pub fn report_error(&self, error: RecoveredError) {
        self.0.borrow_mut().push(error);
    }
}

/// Error containing a text span and an error message to display.
#[derive(Debug)]
pub struct RecoveredError(pub Range<usize>, pub String);

pub type StrSpan<'a> = LocatedSpan<&'a str, ParseState<'a>>;

pub trait ToRange {
    fn span(&self) -> Range<usize>;
}

impl<'a> ToRange for StrSpan<'a> {
    fn span(&self) -> Range<usize> {
        let start = self.get_utf8_column_first_line()-1;
        start..start+self.fragment().chars().count()
    }
}
