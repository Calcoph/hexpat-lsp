use std::{fmt, ops::Range, intrinsics::unreachable};

use nom::{
    character::{complete::{
        one_of,
        alpha1,
        alphanumeric1,
        space0,
        newline, hex_digit1, oct_digit1, digit1
    }, is_newline},
    branch::alt as choice,
    bytes::complete::{
        is_not,
        tag as just,
        take_until,
        tag_no_case as just_no_case, take, take_till, is_a
    },
    combinator::{
        recognize,
        eof,
        map, map_res
    },
    sequence::{pair as then, delimited},
    multi::{
        many0_count,
        many_till as many_until,
        many1
    }, Err
};
use nom_supreme::error::GenericErrorTree;

use crate::recovery_err::IResult;

use super::Span;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    K(Keyword),
    Num(String),
    Char(String),
    Str(String),
    Op(String),
    V(ValueType),
    B(BuiltFunc),
    Ident(String),
    Separator(char),
    Bool(bool),
    Pre(PreProc),
    Comment(String),
    PreprocStr(String),
    Err
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PreProc {
    Include(String),
    Define(String),
    Pragma(String)
}

impl fmt::Display for Token {
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
            Token::Comment(s) => write!(f, "{}", s), // TODO
            //Token::PreprocStart(s) => write!(f, "{}", s),
            //Token::PreprocStr(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueType {
    Boolean,
    String,
    CustomType,
    Padding,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BuiltFunc {
    AddressOf,
    SizeOf,
}

pub enum LexResult {
    Ok(Token),
    Err(String)
}

impl ToString for BuiltFunc {
    fn to_string(&self) -> String {
        match self {
            BuiltFunc::AddressOf => "addressof".to_string(),
            BuiltFunc::SizeOf => "sizeof".to_string(),
        }
    }
}

/// Error containing a text span and an error message to display.
#[derive(Debug)]
struct Error(Range<usize>, String);

/* fn expect<F, E, T, O>(parser: F, error_msg: E) -> impl Fn(&str) -> IResult<Option<T>>
where
    F: Fn(&str) -> IResult<T>,
    E: ToString,
{
    move |input| match parser(input) {
        Ok((remaining, out)) => Ok((remaining, Some(out))),
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            let err = Error(e.input.to_range(), error_msg.to_string());
            e.input.extra.report_error(err); // Push error onto stack.
            Ok((input, None)) // Parsing failed, but keep going.
        }
        Err(err) => Err(err),
    }
} */

fn hex_num(input: &str) -> IResult<&str, Token> {
    map(
        then(
            just_no_case("0x"),
            |input: &str| match hex_digit1(input) {
                Ok((p, s)) => Ok((p, Token::Num(s.to_string()))),
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let input = match e {
                        GenericErrorTree::Base { location, kind } => match kind {
                            nom_supreme::error::BaseErrorKind::Expected(ex) => match ex {
                                nom_supreme::error::Expectation::HexDigit => {
                                    input // TODO: Error message
                                },
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        },
                        GenericErrorTree::Stack { base, contexts } => todo!(),
                        GenericErrorTree::Alt(_) => unreachable!(),
                    };
                    Ok((input, Token::Err))
                },
                Err(_) => unreachable!()
            }
        ),
        |(_, t)| t
    )(input)
}

fn oct_num(input: &str) -> IResult<&str, Token> {
    map(
        then(
            just_no_case("0o"),
            |input: &str| match oct_digit1(input) {
                Ok((p, s)) => Ok((p, Token::Num(s.to_string()))),
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let input = match e {
                        GenericErrorTree::Base { location, kind } => match kind {
                            nom_supreme::error::BaseErrorKind::Expected(ex) => match ex {
                                nom_supreme::error::Expectation::OctDigit => {
                                    input // TODO: Error message
                                },
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        },
                        GenericErrorTree::Stack { base, contexts } => todo!(),
                        GenericErrorTree::Alt(_) => unreachable!(),
                    };
                    Ok((input, Token::Err))
                },
                Err(_) => unreachable!()
            }
        ),
        |(_, t)| t
    )(input)
}

fn bin_num(input: &str) -> IResult<&str, Token> {
    map(
        then(
            just_no_case("0b"),
            |input: &str| match is_a("01")(input) {
                Ok((p, s)) => Ok((p, Token::Num(s.to_string()))),
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let input = match e {
                        GenericErrorTree::Base { location, kind } => match kind {
                            nom_supreme::error::BaseErrorKind::Kind(k) => match k {
                                nom::error::ErrorKind::Many1 => {
                                    input // TODO: Error message
                                },
                                _ => unreachable!()
                            },
                            _ => unreachable!(),
                        },
                        GenericErrorTree::Stack { base, contexts } => todo!(),
                        GenericErrorTree::Alt(_) => unreachable!(),
                    };
                    Ok((input, Token::Err))
                },
                Err(_) => unreachable!()
            }
        ),
        |(_, t)| t
    )(input)
}

fn dec_num(input: &str) -> IResult<&str, Token> {
    match digit1(input) {
        Ok((p, s)) => Ok((p, Token::Num(s.to_string()))),
        Err(e) => Err(e),
    }
}

fn lexer(input: &str) -> IResult<&str, Vec<Token>> {
    // Integer parser
    // TODO: Floats
    let num = choice((
        hex_num,
        map(
            then(
                just_no_case("0o"),
                many1(oct_digit1)
            ),
            |(_, s)| Token::Num(String::from_iter(s.into_iter()))
        ),
        map(
            then(
                just_no_case("0b"),
                many1(one_of("01"))
            ),
            |(_, s)| Token::Num(String::from_iter(s.into_iter()))
        ),
        map(
            many1(one_of("0123456789abcdefABCDEF")),
            |s| Token::Num(String::from_iter(s.into_iter()))
        )
    ));

    // A parser for chars
    let char_ = map_res(
        then(
            then(
                just("\'"),
                take(1 as u8)
            ),
            just("\'")
        ),
        |((_, c), _)| match c {
            "'" => Err(Err::Error("Empty char")),
            _ => Ok(Token::Char(c.to_string()))
        }
    );

    // A parser for strings
    let str_ = map(
        delimited(
            just("\""),
            choice((
                map(
                    then(
                        just("\\"),
                        just("\\")
                    ),
                    |(_,_)| "\\\\".to_string()
                ),
                map(
                    then(
                        just("\\"),
                        just("\"")
                    ),
                    |(_,_)| "\\\"".to_string()
                ),
                map_res(
                    take(1 as u8),
                    |c| match c {
                        "\"" => Err(Err::Error("End of String")),
                        _ => Ok(c.to_string())
                    }
                )
            )),
            just("\"")
        ),
        Token::Str
    );

    // A parser for operators
    let op = map(
        choice((
            map(just("::"), |_| "::".to_string()),
            map(
                then(
                    one_of("=!><"),
                    just("=")
                ),
                |(c, _)| c.to_string() + "="
            ),
            map(just("&&"), |_| "&&".to_string()),
            map(just("||"), |_| "||".to_string()),
            map(just("^^"), |_| "^^".to_string()),
            map(just("<<"), |_| "<<".to_string()),
            map(just(">>"), |_| "<<".to_string()),
            map(one_of("=:+-*/%@><!|&^~?$"), |c| c.to_string())
        )),
        Token::Op
    );

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = map(
        one_of("()[]{};,."),
        Token::Separator
    );

    // A parser for preproccessor directives start
    let preproc = map(
        map(
            then(
                then(
                    just("#"),
                    choice((
                        just("include"),
                        just("pragma"),
                        just("define"),
                    ))
                ),
                take_until("\n"),
            ),
            |((_, command), arg)| {
                match command {
                    "include" => PreProc::Include(arg.to_string()),
                    "pragma" => PreProc::Pragma(arg.to_string()),
                    "define" => PreProc::Define(arg.to_string()),
                    _ => unreachable!()
                }
            }
        ),
        Token::Pre
    );
    /*
    let preproc_str = just('<')
        .ignore_then(filter(|c| *c != '>' && *c != '\n').repeated())
        .then_ignore(just('>'))
        .collect::<String>()
        .map(Token::PreprocStr);*/

    // A parser for identifiers and keywords
    let ident = map(
        recognize(
            then(
                choice((alpha1, just("_"))),
                many0_count(choice((alphanumeric1, just("_"))))
            )
        ),
        |s| match s {
            "struct" => Token::K(Keyword::Struct),
            "bitfield" => Token::K(Keyword::Bitfield),
            "le" => Token::K(Keyword::LittleEndian),
            "be" => Token::K(Keyword::BigEndian),
            "union" => Token::K(Keyword::Union),
            "enum" => Token::K(Keyword::Enum),
            "namespace" => Token::K(Keyword::Namespace),
            "fn" => Token::K(Keyword::Fn),
            "if" => Token::K(Keyword::If),
            "else" => Token::K(Keyword::Else),
            "while" => Token::K(Keyword::While),
            "for" => Token::K(Keyword::For),
            "break" => Token::K(Keyword::Break),
            "continue" => Token::K(Keyword::Continue),
            "in" => Token::K(Keyword::In),
            "out" => Token::K(Keyword::Out),
            "return" => Token::K(Keyword::Return),
            "using" => Token::K(Keyword::Using),
            "parent" => Token::K(Keyword::Parent),
            "this" => Token::K(Keyword::This),
            "addressof" => Token::B(BuiltFunc::AddressOf),
            "sizeof" => Token::B(BuiltFunc::SizeOf),
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            _ => Token::Ident(s.to_string()),
        }
    );

    // A single token can be one of the above
    let token = choice ((
        num,
        char_,
        str_,
        //.or(preproc_str
        op,
        ctrl,
        ident,
        preproc,
    ));

    let comment = map(
        then(just("//"), take_until("\n")),
        |(_, s): (&str, &str)| s
    );
    //let directive = just("#").then(take_until(just('\n'))).padded();
    let padding = map(
        choice((comment, space0)),
        |s| Token::Comment(s.to_string())
    );

    map(
        many_until(choice((token, padding)), eof),
        |(v, _)| v
    )(input)
}

pub fn lex(input: &str) -> (Vec<Token>, Vec<Error>) {
    lex(input)
}