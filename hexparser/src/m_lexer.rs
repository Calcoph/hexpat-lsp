use std::{fmt, cell::RefCell, ops::Range};

use nom::{
    character::complete::{
        alpha1,
        alphanumeric1,
        hex_digit1,
        oct_digit1,
        digit1,
        space1
    },
    branch::alt as choice,
    bytes::complete::{
        tag as just,
        take_until,
        tag_no_case as just_no_case,
        take,
        is_a
    },
    combinator::{
        recognize,
        eof,
        map,
        map_opt
    },
    sequence::{pair as then, delimited},
    multi::{
        many0_count,
        many_till as many_until, many0,
    },
    InputTake
};
use nom_supreme::error::{GenericErrorTree, ErrorTree};

use crate::{recovery_err::{IResult, StrSpan, RecoveredError, ParseState, ToRange}, token::{TokSpan, FromStrSpan}};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    K(Keyword),
    Num(String),
    Char(char),
    Str(String),
    Op(String),
    V(ValueType),
    B(BuiltFunc),
    Ident(String),
    Separator(char),
    Bool(bool),
    Pre(PreProc),
    Comment(String),
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
            Token::Comment(s) => write!(f, "{}", s),
            Token::Err => todo!(), // TODO
            //TokenType::PreprocStart(s) => write!(f, "{}", s),
            //TokenType::PreprocStr(s) => write!(f, "{}", s),
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

fn hex_num<'a>(input: StrSpan<'a>) -> IResult<StrSpan, TokSpan> {
    map(
        then(
            just_no_case("0x"),
            |input: StrSpan<'a>| match hex_digit1(input) {
                Ok((p, s)) => {
                    let state = s.extra.clone();
                    Ok((p, TokSpan::from_strspan(Token::Num(s.to_string()), state, s.span())))
                },
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let input = match e {
                        GenericErrorTree::Base { location, kind } => match kind {
                            nom_supreme::error::BaseErrorKind::Expected(ex) => match ex {
                                nom_supreme::error::Expectation::HexDigit => {
                                    location.extra.report_error(RecoveredError(location.span(), "Expected hexadecimal number after \"0x\"".to_string()));
                                    location
                                },
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    let span = input.span();
                    let state = input.extra.clone();
                    Ok((input, TokSpan::from_strspan(Token::Err, state, span)))
                },
                Err(e) => Err(e)
            }
        ),
        |(_, t)| t
    )(input)
}

fn oct_num<'a>(input: StrSpan<'a>) -> IResult<StrSpan, TokSpan> {
    map(
        then(
            just_no_case("0o"),
            |input: StrSpan<'a>| match oct_digit1(input) {
                Ok((p, s)) => {
                    let state = s.extra.clone();
                    Ok((p, TokSpan::from_strspan(Token::Num(s.to_string()), state, s.span())))
                },
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let input = match e {
                        GenericErrorTree::Base { location, kind } => match kind {
                            nom_supreme::error::BaseErrorKind::Expected(ex) => match ex {
                                nom_supreme::error::Expectation::OctDigit => {
                                    location.extra.report_error(RecoveredError(location.span(), "Expected octal number after \"0o\"".to_string()));
                                    location
                                },
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    let span = input.span();
                    let state = input.extra.clone();
                    Ok((input, TokSpan::from_strspan(Token::Err, state, span)))
                },
                Err(e) => Err(e)
            }
        ),
        |(_, t)| t
    )(input)
}

fn bin_num<'a>(input: StrSpan<'a>) -> IResult<StrSpan, TokSpan> {
    map(
        then(
            just_no_case("0b"),
            |input: StrSpan<'a>| match is_a("01")(input) {
                Ok((p, s)) => {
                    let state = s.extra.clone();
                    Ok((p, (Token::Num(s.to_string()), state, s.span())))
                },
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let input = match e {
                        GenericErrorTree::Base { location, kind } => match kind {
                            nom_supreme::error::BaseErrorKind::Kind(k) => match k {
                                nom::error::ErrorKind::IsA => {
                                    location.extra.report_error(RecoveredError(location.span(), "Expected binary number after \"0b\"".to_string()));
                                    location
                                },
                                _ => unreachable!()
                            },
                            _ => unreachable!()
                        },
                        _ => unreachable!()
                    };
                    let span = input.span();
                    let state = input.extra.clone();
                    Ok((input, (Token::Err, state, span)))
                },
                Err(e) => Err(e)
            }
        ),
        |(head, t)| TokSpan::from_strspan(t.0, t.1, head.span().start..t.2.end)
    )(input)
}

fn dec_num(input: StrSpan) -> IResult<StrSpan, TokSpan> {
    match digit1(input) {
        Ok((p, s)) => {
            let state = s.extra.clone();
            Ok((p, TokSpan::from_strspan(Token::Num(s.to_string()), state, s.span())))
        },
        Err(e) => Err(e),
    }
}

// A parser for chars
fn char_<'a>(input: StrSpan<'a>) -> IResult<StrSpan, TokSpan> { // TODO: Better handling of whitespaces (for example require (whitespace|operator|separator) between tokens)
    match then(
        just("\'"),
        take(1 as u8) // TODO: parse also \'
    )(input) {
        Ok((p, (head, c))) => match *c.fragment() {
            "'" => {
                let span = head.span().start..c.span().end;
                let state = c.extra.clone();
                c.extra.report_error(RecoveredError(span.clone(), "Characters cannot be empty".to_string()));
                Ok((p, TokSpan::from_strspan(Token::Err, state, span)))
            },
            _ => match just("\'")(p) {
                Ok((p, tail)) => {
                    let state = tail.extra.clone();
                    Ok((p, TokSpan::from_strspan(
                        Token::Char(c.fragment().chars().next().unwrap()),
                        state,
                        head.span().start..tail.span().end
                    )))
                },
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let (input, span) = match e {
                        GenericErrorTree::Base {location, kind} => match kind {
                            nom_supreme::error::BaseErrorKind::Kind(k) => match k {
                                nom::error::ErrorKind::Tag => {
                                    let span = head.span().start..c.span().end;
                                    location.extra.report_error(RecoveredError(span.clone(), "Missing closing `'`".to_string()));
                                    (location, span)
                                }
                                _ => unreachable!()
                            }
                            _ => unreachable!()
                        }
                        _ => unreachable!()
                    };
                    let state = input.extra.clone();
                    Ok((input, TokSpan::from_strspan(Token::Err, state, span)))
                },
                Err(e) => Err(e),
            }
        },
        Err(e) => Err(e),
    }
}

fn str_<'a>(input: StrSpan<'a>) -> IResult<StrSpan, TokSpan> {
    // A parser for strings
    let inp_start = input.span().start;
    let state = input.extra.clone();
    map( // TODO: Match for the Error of "delimited" (missing closing '"'")
        delimited(
            just("\""),
            many0(choice((
                just("\\\\"),
                just("\\\""),
                map_opt(
                    take(1 as u8),
                    |c: StrSpan| match *c.fragment() {
                        "\"" => None,
                        _ => Some(c)
                    }
                )
            ))),
            just("\"")
        ),
        move |s: Vec<StrSpan<'a>>| {
            let span = if s.len() > 0 {
                s.get(0).unwrap().span().start-1..s.get(s.len()-1).unwrap().span().end+1
            } else {
                inp_start..inp_start+2
            };
            let string = String::from_iter(s.into_iter().map(|s| *s.fragment()));
            TokSpan::from_strspan(Token::Str(string), state.clone(), span)
        }
    )(input)
}

fn lexer<'a>(input: StrSpan<'a>) -> IResult<StrSpan, Vec<TokSpan>> {
    // Integer parser
    // TODO: Floats
    let num = choice(( // TODO: Better error highlighting for hex, oct and bin
        hex_num,
        oct_num,
        bin_num,
        dec_num
    )); // dec_num can return an error

    // A parser for operators
    let op = map(
        choice((
            just("::"),
            choice((
                just("=="),
                just("!="),
                just("<="),
                just(">="),
            )),
            just("&&"),
            just("||"),
            just("^^"),
            just("<<"),
            just(">>"),
            choice((
                just("="),
                just(":"),
                just("+"),
                just("-"),
                just("*"),
                just("/"),
                just("%"),
                just("@"),
                just(">"),
                just("<"),
                just("!"),
                just("|"),
                just("&"),
                just("^"),
                just("~"),
                just("?"),
                just("$"),
            ))
        )),
        |s: StrSpan| {
            let state = s.extra.clone();
            TokSpan::from_strspan(Token::Op(s.fragment().to_string()), state, s.span())
        }
    );

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = map(
        choice((
            just("("),
            just(")"),
            just("["),
            just("]"),
            just("{"),
            just("}"),
            just(";"),
            just(","),
            just("."),
        )),
        |s: StrSpan| {
            let state = s.extra.clone();
            TokSpan::from_strspan(
                Token::Separator(s.fragment().chars().next().unwrap()),
                state,
                s.span()
            )
        }
    );

    // A parser for preproccessor directives start
    let preproc =
        map(
            then(
                then(
                    just("#"),
                    choice(( // TODO: Error recovery here
                        just("include"),
                        just("pragma"),
                        just("define"),
                    ))
                ),
                take_until("\n"),
            ),
            |((pound, command), arg): ((StrSpan, StrSpan), StrSpan)| {
                let pre = match *command.fragment() {
                    "include" => PreProc::Include(arg.to_string()),
                    "pragma" => PreProc::Pragma(arg.to_string()),
                    "define" => PreProc::Define(arg.to_string()),
                    _ => unreachable!()
                };
                let span = pound.span().start..arg.span().end;
                let state = command.extra.clone();
                TokSpan::from_strspan(Token::Pre(pre), state, span)
            }
        );

    // A parser for identifiers and keywords
    let ident = map(
        recognize(
            then(
                choice((alpha1, just("_"))),
                many0_count(choice((alphanumeric1, just("_"))))
            )
        ),
        |s: StrSpan| {
            let token = match *s.fragment() {
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
                s => Token::Ident(s.to_string()),
            };
            let state = s.extra.clone();
            TokSpan::from_strspan(token, state, s.span())
        }
    );

    // A single token can be one of the above
    let token = choice ((
        num,
        char_,
        str_,
        op,
        ctrl,
        ident,
        preproc,
    ));

    let comment = map(
        then(just("//"), take_until("\n")),
        |(_, s): (StrSpan, StrSpan)| s
    );

    let padding = map(
        choice((
            comment,
            just("\n"),
            just("\r"),
            space1
        )),
        |s| {
            let state = s.extra.clone();
            TokSpan::from_strspan(Token::Comment(s.to_string()), state, s.span())
        }
    );

    let mut pos_inputs = choice((token, padding));

    map(
        many_until(
            move |input: StrSpan<'a>| match pos_inputs(input) {
                Ok(r) => Ok(r),
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let input = recover_err(&e);
                    let len = input.fragment().chars().next().unwrap().len_utf8();
                    let (rest, input) = input.take_split(len);
                    let span = input.span();
                    input.extra.report_error(RecoveredError(span.clone(), "Unkown (non-ASCII) character".to_string()));
                    let state = rest.extra.clone();
                    Ok((rest, TokSpan::from_strspan(Token::Err, state, span)))
                },
                Err(e) => Err(e)
            },
            eof
        ),
        |(v, _)| v
    )(input)
}

fn recover_err<'a>(e: &ErrorTree<StrSpan<'a>>) -> StrSpan<'a> {
    match e {
        GenericErrorTree::Base { location, kind: _ } => location.clone(),
        GenericErrorTree::Stack { base, contexts: _ } => recover_err(base),
        GenericErrorTree::Alt(v) => recover_err(v.get(0).unwrap()),
    }
}

pub fn lex<'a>(input: &'a str, errors: &'a RefCell<Vec<RecoveredError>>) -> Vec<TokSpan<'a>> {
    let input = StrSpan::new_extra(input, ParseState(errors));
    let (_, tokens) = lexer(input).expect("Unrecovered error happen");

    tokens
}
