//use nom::error::{ParseError, ErrorKind, FromExternalError};
use nom_supreme::error::ErrorTree;

pub type IResult<I, O, E = ErrorTree<I>> = Result<(I, O), nom::Err<E>>;

/* struct HexErr<I> {
    input: I,
    code: ErrorKind
}

impl<I> ParseError<I> for HexErr<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        HexErr { input, code: kind }
    }

    fn append(input: I, kind: ErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: I, _: char) -> Self {
        Self::from_error_kind(input, ErrorKind::Char)
    }

    fn or(self, other: Self) -> Self {
        other
      }
}

impl<I> FromExternalError<I> for HexErr<I> {
} */