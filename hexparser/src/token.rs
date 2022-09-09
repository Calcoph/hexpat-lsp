use std::{ops::Range, iter::{Enumerate, Copied}, slice::Iter, fmt::{self, Display}};

use nom::{Compare, CompareResult, InputLength, InputIter, InputTake, Needed};
use nom_locate::LocatedSpan;

use crate::recovery_err::{ParseState, ToRange};

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum Token<'a> {
    K(Keyword),
    Num(&'a str),
    Char(char),
    Str(&'a str),
    Op(&'a str),
    V(ValueType),
    B(BuiltFunc),
    Ident(&'a str),
    Separator(char),
    Bool(bool),
    Pre(PreProc<'a>),
    Comment(&'a str),
    Err
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PreProc<'a> {
    Include(&'a str),
    Define(&'a str),
    Pragma(&'a str)
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bool(x) => write!(f, "{}", x),
            Token::Num(n) => write!(f, "{}", n),
            Token::Str(s) => write!(f, "{}", s),
            Token::Char(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::Separator(c) => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::K(k) => match k {
                Keyword::Struct => write!(f, "struct"),
                Keyword::Bitfield => write!(f, "bitfield"),
                Keyword::Union => write!(f, "union"),
                Keyword::Enum => write!(f, "enum"),
                Keyword::Namespace => write!(f, "namespace"),
                Keyword::Fn => write!(f, "fn"),
                Keyword::If => write!(f, "if"),
                Keyword::Else => write!(f, "else"),
                Keyword::While => write!(f, "while"),
                Keyword::For => write!(f, "for"),
                Keyword::Break => write!(f, "break"),
                Keyword::Continue => write!(f, "continue"),
                Keyword::In => write!(f, "in"),
                Keyword::Out => write!(f, "out"),
                Keyword::Return => write!(f, "return"),
                Keyword::Using => write!(f, "using"),
                Keyword::Parent => write!(f, "parent"),
                Keyword::This => write!(f, "this"),
                Keyword::LittleEndian => write!(f, "le"),
                Keyword::BigEndian => write!(f, "be"),
            },
            Token::B(b) => match b {
                BuiltFunc::AddressOf => write!(f, "addressof"),
                BuiltFunc::SizeOf => write!(f, "sizeof"),
            }
            Token::V(_) => write!(f, "V"), // TODO
            Token::Pre(_) => write!(f, "PreProc"),
            Token::Comment(s) => write!(f, "{}", s),
            Token::Err => todo!(), // TODO
            //TokenType::PreprocStart(s) => write!(f, "{}", s),
            //TokenType::PreprocStr(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Keyword {
    Struct,
    Union,
    Using,
    Enum,
    Bitfield,
    LittleEndian,
    BigEndian,
    Fn,
    If,
    Else,
    Parent,
    This,
    While,
    For,
    Return,
    Namespace,
    In,
    Out,
    Break,
    Continue
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ValueType {
    CustomType,
    Padding,
    Auto,
    U8,
    U16,
    U24,
    U32,
    U48,
    U64,
    U96,
    U128,
    S8,
    S16,
    S24,
    S32,
    S48,
    S64,
    S96,
    S128,
    Float,
    Double,
    Boolean,
    Character,
    Character16,
    String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BuiltFunc {
    AddressOf,
    SizeOf,
}

impl ToString for BuiltFunc {
    fn to_string(&self) -> String {
        match self {
            BuiltFunc::AddressOf => "addressof".to_string(),
            BuiltFunc::SizeOf => "sizeof".to_string(),
        }
    }
}


pub type Spanned<T> = (T, Range<usize>);

pub type TokSpan<'a> = LocatedSpan<Token<'a>, (ParseState<'a>, usize)>;

pub trait FromStrSpan<'a> {
    fn from_strspan(token: Token<'a>, state: ParseState<'a>, span: Range<usize>) -> TokSpan<'a>;
}

impl<'a> FromStrSpan<'a> for TokSpan<'a> {
    #[inline]
    fn from_strspan(token: Token<'a>, state: ParseState<'a>, span: Range<usize>) -> TokSpan<'a> {
        unsafe{TokSpan::new_from_raw_offset(span.start, 0, token, (state, span.end-span.start))}
    }
}

impl<'a> ToRange for TokSpan<'a> {
    fn span(&self) -> Range<usize> {
        let start = self.location_offset();
        start..start+self.extra.1
    }
}

impl<'a> ToRange for Tokens<'a> {
    fn span(&self) -> Range<usize> {
        let start = self.offset;
        let end = match self.tokens.len() {
            0 => start+1,
            _ => {
                let end = self.tokens[self.tokens.len()-1];
                end.location_offset()+end.extra.1
            }
        };
        start..end
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Tokens<'a> {
    pub tokens: &'a [TokSpan<'a>],
    offset: usize,
    pub state: ParseState<'a>
}

impl<'a> Tokens<'a> {
    pub fn new(tokens: &'a [TokSpan<'a>], state: ParseState<'a>) -> Tokens<'a> {
        Tokens { tokens, offset: 0, state }
    }
}

impl<'a> Display for Tokens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.tokens)
    }
}

// impl AsBytes

/* impl<'a> Compare<Tokens<'a>> for Tokens<'a> {
    fn compare(&self, t: Tokens) -> CompareResult {
        let pos = self.tokens.iter()
            .zip(t.tokens.iter())
            .position(|(a, b)| a.fragment() != b.fragment());

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
} */

impl<'a> Compare<Token<'a>> for Tokens<'a> {
    fn compare(&self, t: Token) -> CompareResult {
        if self.tokens.len() == 0 || *self.tokens[0].fragment() != t {
            CompareResult::Error
        } else {
            CompareResult::Ok
        }
    }

    fn compare_no_case(&self, t: Token) -> CompareResult {
        self.compare(t)
    }
}

// impl ExtendInto

// impl FindSubstring

// impl FindToken

impl<'a> InputIter for Tokens<'a> {
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
        self.tokens.iter().position(|b| predicate(*b))
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.tokens.len() >= count {
            Ok(count)
        } else {
        Err(Needed::new(count - self.tokens.len()))
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tokens.input_len()
    }
}

impl<'a> InputLength for Token<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> InputTake for Tokens<'a> {
    fn take(&self, count: usize) -> Self {
        Tokens::new(&self.tokens[0..count], self.state)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tokens.split_at(count);
        let suf_offset = match suffix.len() {
            0 => match prefix.len() {
                0 => self.offset,
                _ => prefix[0].span().end+1
            },
            _ => suffix[0].span().start
        };
        (Tokens{tokens: suffix, offset: suf_offset, state: self.state}, Tokens{tokens: prefix, offset: self.offset, state: self.state})
    }
}

/* impl InputTake for Token {
    fn take(&self, count: usize) -> Self {
        if count > 1 {
            panic!("Token cannot be subdivided")
        } else {
            self.clone()
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        panic!("Token cannot be subdivided")
    }
} */

// impl InputTakeAtPosition

// impl Offset

// impl ParseTo

// impl Slice
