use std::{cell::RefCell, ops::Range, error::Error};

use nom::{Parser, InputTake};
use nom_locate::LocatedSpan;
//use nom::error::{ParseError, ErrorKind, FromExternalError};
use nom_supreme::error::{ErrorTree, GenericErrorTree};

use crate::{token::{TokSpan, Tokens, Spanned}, Expr};

pub type StrResult<I, O, E=ErrorTree<I>> = Result<(I, O), nom::Err<E>>;
pub type TokError<'a> = GenericErrorTree<Tokens<'a>, &'a [TokSpan<'a>], &'static str, Box<dyn Error + 'a>>;
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
    fn consumed_span(&self, next_start: usize) -> Range<usize>;
}

impl<'a> ToRange for StrSpan<'a> {
    fn span(&self) -> Range<usize> {
        let start = self.get_column_first_line()-1;
        start..start+self.fragment().chars().count()
    }

    fn consumed_span(&self, next_start: usize) -> Range<usize> {
        unimplemented!()
    }
}

pub fn expression_recovery<'a, F>(mut func: F) -> impl FnMut(Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>>
where
    F: Parser<Tokens<'a>, Spanned<Expr>, TokError<'a>>
{
    move |input: Tokens<'a>| -> TokResult<Tokens<'a>, Spanned<Expr>> {
        match func.parse(input) {
            Ok(r) => Ok(r),
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                recover_from_error(e)
            },
            Err(e) => Err(e)
        }
    }
}

fn recover_from_error<'a>(e: TokError<'a>) -> TokResult<'a, Tokens<'a>, Spanned<Expr>> {
    match e {
        GenericErrorTree::Stack { base: _, contexts } => {
            let (input, context) = contexts[contexts.len()-1];
            match context {
                nom_supreme::error::StackContext::Context(error_msg) => dbg!(error_msg),
                _ => unreachable!()
            };
            let (rest, span) = match input.tokens.len() {
                0 => {
                    (input, input.span())
                },
                _ => {
                    let (rest, input) = input.take_split(1);
                    (rest, input.span())
                }
            };

            match context {
                nom_supreme::error::StackContext::Context(error_msg) => rest.state.report_error(RecoveredError(span.clone(), error_msg.to_string())),
                _ => unreachable!()
            };

            Ok((rest, (Expr::Error, span)))
        },
        GenericErrorTree::Base { location, kind } => Err(nom::Err::Error(TokError::Base { location, kind })),
        GenericErrorTree::Alt(v) => {
            let mut v2 = vec![];
            for e in v {
                match recover_from_error(e) {
                    Ok(a) => return Ok(a),
                    Err(res) => match res {
                        nom::Err::Error(res) => v2.push(res),
                        nom::Err::Failure(res) => v2.push(res),
                        _ => unreachable!(),
                    },
                };
            };
            Err(nom::Err::Error(TokError::Alt(v2)))
        },
    }
}

pub fn non_opt<'a, F, I, O>(mut func: F) -> impl FnMut(I) -> TokResult<'a, I, O>
where
    F: Parser<I, O, TokError<'a>>
{
    move |input: I| -> TokResult<I, O> {
        match func.parse(input) {
            Ok(r) => Ok(r),
            Err(nom::Err::Error(e)) => Err(nom::Err::Failure(e)),
            Err(e) => Err(e)
        }
    }
}
