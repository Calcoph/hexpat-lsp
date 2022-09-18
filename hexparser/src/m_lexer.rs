use std::{cell::RefCell, slice};

use nom::{
    character::complete::{
        alpha1,
        alphanumeric1,
        hex_digit1,
        oct_digit1,
        digit1,
        multispace1,
        not_line_ending
    },
    branch::alt as choice,
    bytes::complete::{
        tag as just,
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
    sequence::{pair as then, delimited, preceded},
    multi::{
        many_till as many_until,
        many0,
    },
    InputTake
};
use nom_supreme::error::{GenericErrorTree, ErrorTree};

use crate::{recovery_err::{StrResult, StrSpan, RecoveredError, ParseState, ToRange}, token::{TokSpan, FromStrSpan, Token, PreProc, Keyword, BuiltFunc, ValueType}};

fn hex_num<'a, 'b>(input: StrSpan<'a, 'b>) -> StrResult<StrSpan<'a, 'b>, TokSpan<'a, 'b>> {
    map(
        then(
            just_no_case("0x"),
            |input: StrSpan<'a, 'b>| match hex_digit1(input) {
                Ok((p, s)) => {
                    let state = s.extra;
                    Ok((p, TokSpan::from_strspan(Token::Num(s.fragment()), state, s.span())))
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
                    let state = input.extra;
                    Ok((input, TokSpan::from_strspan(Token::Err, state, span)))
                },
                Err(e) => Err(e)
            }
        ),
        |(_, t)| t
    )(input)
}

fn oct_num<'a, 'b>(input: StrSpan<'a, 'b>) -> StrResult<StrSpan<'a, 'b>, TokSpan<'a, 'b>> {
    map(
        then(
            just_no_case("0o"),
            |input: StrSpan<'a, 'b>| match oct_digit1(input) {
                Ok((p, s)) => {
                    let state = s.extra;
                    Ok((p, TokSpan::from_strspan(Token::Num(s.fragment()), state, s.span())))
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
                    let state = input.extra;
                    Ok((input, TokSpan::from_strspan(Token::Err, state, span)))
                },
                Err(e) => Err(e)
            }
        ),
        |(_, t)| t
    )(input)
}

fn bin_num<'a, 'b>(input: StrSpan<'a, 'b>) -> StrResult<StrSpan<'a, 'b>, TokSpan<'a, 'b>> {
    map(
        then(
            just_no_case("0b"),
            |input: StrSpan<'a, 'b>| match is_a("01")(input) {
                Ok((p, s)) => {
                    let state = s.extra;
                    Ok((p, (Token::Num(s.fragment()), state, s.span())))
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
                    let state = input.extra;
                    Ok((input, (Token::Err, state, span)))
                },
                Err(e) => Err(e)
            }
        ),
        |(head, t)| TokSpan::from_strspan(t.0, t.1, head.span().start..t.2.end)
    )(input)
}

fn dec_num<'a, 'b>(input: StrSpan<'a, 'b>) -> StrResult<StrSpan<'a, 'b>, TokSpan<'a, 'b>> {
    match digit1(input) {
        Ok((p, s)) => {
            let state = s.extra;
            Ok((p, TokSpan::from_strspan(Token::Num(s.fragment()), state, s.span())))
        },
        Err(e) => Err(e),
    }
}

// A parser for chars
fn char_<'a, 'b>(input: StrSpan<'a, 'b>) -> StrResult<StrSpan<'a, 'b>, TokSpan<'a, 'b>> { // TODO: Better handling of whitespaces (for example require (whitespace|operator|separator) between tokens)
    match then(
        just("\'"),
        take(1 as u8) // TODO: parse also \'
    )(input) {
        Ok((p, (head, c))) => match *c.fragment() {
            "'" => {
                let span = head.span().start..c.span().end;
                let state = c.extra;
                c.extra.report_error(RecoveredError(span.clone(), "Characters cannot be empty".to_string()));
                Ok((p, TokSpan::from_strspan(Token::Err, state, span)))
            },
            _ => match just("\'")(p) {
                Ok((p, tail)) => {
                    let state = tail.extra;
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
                    let state = input.extra;
                    Ok((input, TokSpan::from_strspan(Token::Err, state, span)))
                },
                Err(e) => Err(e),
            }
        },
        Err(e) => Err(e),
    }
}

fn str_<'a, 'b>(input: StrSpan<'a, 'b>) -> StrResult<StrSpan<'a, 'b>, TokSpan<'a, 'b>> {
    // A parser for strings
    let inp_start = input.span().start;
    let state = input.extra;
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
        move |s: Vec<StrSpan<'a, 'b>>| {
            let (span, s) = if s.len() > 0 {
                let start = s.get(0).unwrap().span().start;
                let end = s.get(s.len()-1).unwrap().span().end;
                let span = start-1..end+1; // -1 and +1 to also take the " delimeters

                let first_ptr = s.get(0).unwrap().as_ptr();
                let len = end - start;
                
                let s = unsafe {slice::from_raw_parts(first_ptr, len)};
                let s = std::str::from_utf8(s).unwrap();

                (span, s)
            } else {
                (inp_start..inp_start+2, "")
            };
            TokSpan::from_strspan(Token::Str(s), state, span) // TODO: Don't return empty str
        }
    )(input)
}

fn lexer<'a, 'b>(input: StrSpan<'a, 'b>) -> StrResult<StrSpan<'a, 'b>, Vec<TokSpan<'a, 'b>>> {
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
            let state = s.extra;
            TokSpan::from_strspan(Token::Op(s.fragment()), state, s.span())
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
            let state = s.extra;
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
                not_line_ending,
            ),
            |((pound, command), arg): ((StrSpan, StrSpan), StrSpan)| {
                let pre = match *command.fragment() {
                    "include" => PreProc::Include(arg.fragment()),
                    "pragma" => PreProc::Pragma(arg.fragment()),
                    "define" => PreProc::Define(arg.fragment()),
                    _ => unreachable!()
                };
                let span = pound.span().start..arg.span().end;
                let state = command.extra;
                TokSpan::from_strspan(Token::Pre(pre), state, span)
            }
        );

    // A parser for identifiers and keywords
    let ident = map(
        recognize(
            then(
                choice((alpha1, just("_"))),
                many0(choice((alphanumeric1, just("_"))))
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
                "u8" => Token::V(ValueType::U8),
                "s8" => Token::V(ValueType::S8),
                "u16" => Token::V(ValueType::U16),
                "s16" => Token::V(ValueType::S16),
                "u24" => Token::V(ValueType::U24),
                "s24" => Token::V(ValueType::S24),
                "u32" => Token::V(ValueType::U32),
                "s32" => Token::V(ValueType::S32),
                "u48" => Token::V(ValueType::U48),
                "s48" => Token::V(ValueType::S48),
                "u64" => Token::V(ValueType::U64),
                "s64" => Token::V(ValueType::S64),
                "u96" => Token::V(ValueType::U96),
                "s96" => Token::V(ValueType::S96),
                "u128" => Token::V(ValueType::U128),
                "s128" => Token::V(ValueType::S128),
                "float" => Token::V(ValueType::Float),
                "double" => Token::V(ValueType::Double),
                "char" => Token::V(ValueType::Character),
                "char16" => Token::V(ValueType::Character16),
                "bool" => Token::V(ValueType::Boolean),
                "str" => Token::V(ValueType::String),
                "padding" => Token::V(ValueType::Padding),
                "auto" => Token::V(ValueType::Auto),
                s => Token::Ident(s),
            };
            let state = s.extra;
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

    let comment = preceded(just("//"), not_line_ending);

    let padding = map(
        choice((
            comment,
            multispace1
        )),
        |s: StrSpan| {
            let state = s.extra;
            TokSpan::from_strspan(Token::Comment(s.fragment()), state, s.span())
        }
    );

    let mut pos_inputs = choice((padding, token));

    map(
        many_until(
            move |input: StrSpan<'a, 'b>| match pos_inputs(input) {
                Ok(r) => Ok(r),
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let input = recover_err(&e);
                    let len = input.fragment().chars().next().unwrap().len_utf8();
                    let (rest, input) = input.take_split(len);
                    let span = input.span();
                    input.extra.report_error(RecoveredError(span.clone(), "Unkown (non-ASCII) character".to_string()));
                    let state = rest.extra;
                    Ok((rest, TokSpan::from_strspan(Token::Err, state, span)))
                },
                Err(e) => Err(e)
            },
            eof
        ),
        |(v, _)| v
    )(input)
}

fn recover_err<'a, 'b>(e: &ErrorTree<StrSpan<'a, 'b>>) -> StrSpan<'a, 'b> {
    match e {
        GenericErrorTree::Base { location, kind: _ } => *location,
        GenericErrorTree::Stack { base, contexts: _ } => recover_err(base),
        GenericErrorTree::Alt(v) => recover_err(v.get(0).unwrap()),
    }
}

pub fn lex<'a, 'b>(input: &'a str, errors: &'b RefCell<Vec<RecoveredError>>) -> Vec<TokSpan<'a, 'b>> {
    let input = StrSpan::new_extra(input, ParseState(errors));
    let (_, tokens) = lexer(input).expect("Unrecovered error happenned in lexer");

    tokens
}

pub fn lex_tokens<'a, 'b>(input: &'a str, errors: &'b RefCell<Vec<RecoveredError>>) -> Vec<Token<'a>> {
    let input = StrSpan::new_extra(input, ParseState(errors));
    let (_, tokens) = lexer(input).expect("Unrecovered error happenned in lexer");

    tokens.into_iter().map(|a| *a.fragment()).collect()
}
