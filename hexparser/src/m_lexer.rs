use std::fmt;

use nom::{
    IResult,
    character::complete::{
        one_of,
        alpha1,
        alphanumeric1,
        space0,
        newline
    },
    branch::alt as choice,
    bytes::complete::{
        is_not,
        tag as just,
        take_until,
        tag_no_case as just_no_case, take
    },
    combinator::{
        recognize,
        eof,
        map_res
    },
    sequence::{pair as then, delimited},
    multi::{
        many0_count,
        many_till as many_until,
        many1
    }, Err
};

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
    Pre(PreProc)
    //PreprocStr(String),
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
            Token::Pre(_) => write!(f, "PreProc"), // TODO
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

impl ToString for BuiltFunc {
    fn to_string(&self) -> String {
        match self {
            BuiltFunc::AddressOf => "addressof".to_string(),
            BuiltFunc::SizeOf => "sizeof".to_string(),
        }
    }
}

// Integer parsers
#[inline(always)]
fn hex_num(input: &str) -> impl FnMut(&str) -> IResult<&str, Token> {
    map_res(
        then(
            just_no_case("0x"),
            many1(one_of("0123456789abcdefABCDEF"))
        ),
        |(_, s): (_, Vec<char>)| Ok(Token::Num(String::from_iter(s.into_iter())))
    )
}
#[inline(always)]
fn oct_num(input: &str) -> impl FnMut(&str) -> IResult<&str, Token> {
    map_res(
        then(
            just_no_case("0o"),
            many1(one_of("01234567"))
        ),
        |(_, s): (_, Vec<char>)| Ok(Token::Num(String::from_iter(s.into_iter())))
    )
}
#[inline(always)]
fn bin_num(input: &str) -> impl FnMut(&str) -> IResult<&str, Token> {
    map_res(
        then(
            just_no_case("0b"),
            many1(one_of("01"))
        ),
        |(_, s): (_, Vec<char>)| Ok(Token::Num(String::from_iter(s.into_iter())))
    )
}
#[inline(always)]
fn dec_num(input: &str) -> impl FnMut(&str) -> IResult<&str, Token> {
    map_res(
        many1(one_of("0123456789abcdefABCDEF")),
        |s: Vec<char>| Ok(Token::Num(String::from_iter(s.into_iter())))
    )
}

pub fn lexer(input: &str) -> IResult<&str, Vec<(Token, Span)>> {
    // TODO: Floats
    let num = choice((
        hex_num,
        oct_num,
        bin_num,
        dec_num
    ));

    // A parser for chars
    let char_ = map_res(
        then(
            then(
                just("\'"),
                take(1)
            ),
            just("\'")
        ),
        |((_, c), _)| match c {
            "'" => Err(Err::Error("Empty char")),
            _ => Ok(Token::Char(c.to_string()))
        }
    );

    // A parser for strings
    let str_ = map_res(
        delimited(
            just("\""),
            choice((
                map_res(
                    then(
                        just("\\"),
                        just("\\")
                    ),
                    |(_,_)| Ok("\\\\".to_string())
                ),
                map_res(
                    then(
                        just("\\"),
                        just("\"")
                    ),
                    |(_,_)| Ok("\\\"".to_string())
                ),
                map_res(
                    take(1),
                    |c| match c {
                        "\"" => Err(Err::Error("End of String")),
                        _ => Ok(c.to_string())
                    }
                )
            )),
            just("\"")
        ),
        |s| Ok(Token::Str(s))
    );

    // A parser for operators
    let op = map_res(
        choice((
            map_res(just("::"), |_| Ok("::".to_string())),
            map_res(
                then(
                    one_of("=!><"),
                    just("=")
                ),
                |(c, _)| Ok(c.to_string() + "=")
            ),
            map_res(just("&&"), |_| Ok("&&".to_string())),
            map_res(just("||"), |_| Ok("||".to_string())),
            map_res(just("^^"), |_| Ok("^^".to_string())),
            map_res(just("<<"), |_| Ok("<<".to_string())),
            map_res(just(">>"), |_| Ok("<<".to_string())),
            map_res(one_of("=:+-*/%@><!|&^~?$"), |c: char| Ok(c.to_string()))
        )),
        |s| Ok(Token::Op(s))
    );

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = map_res(
        one_of("()[]{};,."),
        |c| Ok(Token::Separator(c))
    );

    // A parser for preproccessor directives start
    let preproc = map_res(
        then(
            then(
                just("#"),
                choice((
                    just("include"),
                    just("pragma"),
                    just("define"),
                ))
            ),
            map_res(
                take_until("\n"),
                |(command, arg)| {
                    match command {
                        "include" => Ok(PreProc::Include(arg)),
                        "pragma" => Ok(PreProc::Pragma(arg)),
                        "define" => Ok(PreProc::Define(arg)),
                        _ => unreachable!()
                    }
                }
            )
        ),
        |p| Ok(Token::Pre(p))
    );
    /*
    let preproc_str = just('<')
        .ignore_then(filter(|c| *c != '>' && *c != '\n').repeated())
        .then_ignore(just('>'))
        .collect::<String>()
        .map(Token::PreprocStr);*/

    // A parser for identifiers and keywords
    let ident = map_res(
        recognize(
            then(
                choice((alpha1, just("_"))),
                many0_count(choice((alphanumeric1, just("_"))))
            )
        ),
        |s| match s {
            "struct" => Ok(Token::K(Keyword::Struct)),
            "bitfield" => Ok(Token::K(Keyword::Bitfield)),
            "le" => Ok(Token::K(Keyword::LittleEndian)),
            "be" => Ok(Token::K(Keyword::BigEndian)),
            "union" => Ok(Token::K(Keyword::Union)),
            "enum" => Ok(Token::K(Keyword::Enum)),
            "namespace" => Ok(Token::K(Keyword::Namespace)),
            "fn" => Ok(Token::K(Keyword::Fn)),
            "if" => Ok(Token::K(Keyword::If)),
            "else" => Ok(Token::K(Keyword::Else)),
            "while" => Ok(Token::K(Keyword::While)),
            "for" => Ok(Token::K(Keyword::For)),
            "break" => Ok(Token::K(Keyword::Break)),
            "continue" => Ok(Token::K(Keyword::Continue)),
            "in" => Ok(Token::K(Keyword::In)),
            "out" => Ok(Token::K(Keyword::Out)),
            "return" => Ok(Token::K(Keyword::Return)),
            "using" => Ok(Token::K(Keyword::Using)),
            "parent" => Ok(Token::K(Keyword::Parent)),
            "this" => Ok(Token::K(Keyword::This)),
            "addressof" => Ok(Token::B(BuiltFunc::AddressOf)),
            "sizeof" => Ok(Token::B(BuiltFunc::SizeOf)),
            "true" => Ok(Token::Bool(true)),
            "false" => Ok(Token::Bool(false)),
            _ => Ok(Token::Ident(s.to_string())),
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

    let comment = then(just("//"), take_until(newline));
    //let directive = just("#").then(take_until(just('\n'))).padded();
    let padding = choice((comment, space0));

    many_until(choice((token, padding)), eof)(input)
}
