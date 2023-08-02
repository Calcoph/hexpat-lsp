use std::{cell::RefCell, ops::Range, error::Error};

use nom::{Parser, InputTake, combinator::peek, bytes::complete::tag as just, branch::alt as choice};
use nom_locate::LocatedSpan;
//use nom::error::{ParseError, ErrorKind, FromExternalError};
use nom_supreme::error::{ErrorTree, GenericErrorTree};

use crate::{token::{TokSpan, Tokens, Spanned, Token}, Expr};

pub type StrResult<I, O, E=ErrorTree<I>> = Result<(I, O), nom::Err<E>>;
pub type TokError<'a, 'b> = GenericErrorTree<Tokens<'a, 'b>, &'a [TokSpan<'a, 'b>], &'static str, Box<dyn Error + 'a>>;
pub type TokResult<'a, 'b, O, I=Tokens<'a, 'b>, E=TokError<'a, 'b>> = Result<(I, O), nom::Err<E>>;

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

pub type StrSpan<'a, 'b> = LocatedSpan<&'a str, ParseState<'b>>;

pub trait ToRange {
    fn span(&self) -> Range<usize>;
    fn consumed_span(&self, next_start: usize) -> Range<usize>;
}

impl<'a, 'b> ToRange for StrSpan<'a, 'b> {
    fn span(&self) -> Range<usize> {
        let start = self.get_column_first_line()-1;
        start..start+self.fragment().chars().count()
    }

    #[allow(unused_variables)]
    fn consumed_span(&self, next_start: usize) -> Range<usize> {
        unimplemented!()
    }
}

pub fn expression_recovery<'a, 'b: 'a, F>(mut func: F) -> impl FnMut(Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>>
where
    F: Parser<Tokens<'a, 'b>, Spanned<Expr>, TokError<'a, 'b>>
{
    move |input: Tokens<'a, 'b>| -> TokResult<'a, 'b, Spanned<Expr>> {
        match func.parse(input) {
            Ok(r) => Ok(r),
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                recover_from_error(e)
            },
            Err(e) => Err(e)
        }
    }
}

fn recover_from_error<'a, 'b>(e: TokError<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    match e {
        GenericErrorTree::Stack { base: _, contexts } => {
            let (input, context) = contexts[contexts.len()-1];
            let (rest, span) = match input.tokens.len() {
                0 => {
                    (input, input.span())
                },
                _ => {
                    // Consumes tokens until finding a ';' or '}'
                    let (mut rest, n_token) = input.take_split(1);
                    let mut next_token = n_token;
                    let mut continue_loop = |input| -> TokResult<'a, 'b, Tokens> {
                        peek(choice((
                            just(Token::Separator(';')),
                            just(Token::Separator('}'))
                        )))(input)
                    }(next_token).is_err() && rest.tokens.len() > 0;
                    let mut bracket_count = 0;
                    continue_loop = false; // TODO: delete this (or not)
                    while continue_loop { // TODO: First see where to split and then do it, instead of splitting by 1 at a time
                        let (r, n_token) = rest.take_split(1);
                        rest = r;
                        next_token = n_token;

                        let is_open_bracket = |input| -> TokResult<'a, 'b, Tokens> {peek(just(Token::Separator('{')))(input)};
                        let is_semicolon = |input| -> TokResult<'a, 'b, Tokens> {peek(just(Token::Separator(';')))(input)};
                        let is_close_bracket = |input| -> TokResult<'a, 'b, Tokens> {peek(just(Token::Separator('{')))(input)};

                        if is_open_bracket(next_token).is_ok() {
                            bracket_count += 1;
                        } else if is_semicolon(next_token).is_ok() {
                            if bracket_count <= 0 {
                                continue_loop = false
                            }
                        } else if is_close_bracket(next_token).is_ok() {
                            if bracket_count <= 0 {
                                continue_loop = false
                            }
                            bracket_count -= 1;
                        }

                        continue_loop = continue_loop && rest.tokens.len() > 0; 
                    }
                    (rest, n_token.span())
                }
            };

            match context {
                nom_supreme::error::StackContext::Context(error_msg) => rest.state.report_error(RecoveredError(span.clone(), error_msg.to_string())),
                nom_supreme::error::StackContext::Kind(error_kind) => {
                    let error = error_kind.description().to_string();
                    eprintln!("Unreachable code entered: {:?} | {}", span.clone(), error);
                    rest.state.report_error(RecoveredError(span.clone(), error))
                },
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

pub fn non_opt<'a, 'b: 'a, F, I, O>(mut func: F) -> impl FnMut(I) -> TokResult<'a, 'b, O, I>
where
    F: Parser<I, O, TokError<'a, 'b>>,
{
    move |input: I| -> TokResult<O, I> {
        match func.parse(input) {
            Ok(r) => Ok(r),
            Err(nom::Err::Error(e)) => Err(nom::Err::Failure(e)),
            Err(e) => Err(e)
        }
    }
}
