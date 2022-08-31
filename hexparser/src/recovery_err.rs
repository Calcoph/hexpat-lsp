use std::{cell::RefCell, ops::Range};

use nom::Offset;
use nom_locate::LocatedSpan;
//use nom::error::{ParseError, ErrorKind, FromExternalError};
use nom_supreme::error::ErrorTree;

pub type IResult<I, O, E=ErrorTree<I>> = Result<(I, O), nom::Err<E>>;

/// Carried around in the `LocatedSpan::extra` field in
/// between `nom` parsers.
#[derive(Clone, Debug)]
pub struct ParseState<'a>(pub &'a RefCell<Vec<RecoveredError>>);

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
