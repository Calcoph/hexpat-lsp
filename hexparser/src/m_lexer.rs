use std::fmt;

use chumsky::{prelude::*,Parser};

use super::Span;

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
    //PreprocStart(String),
    //PreprocStr(String),
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

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // Integer parsers
    let hex_num = just('-').or_not().then_ignore(just('0'))
        .then_ignore(just('x').or(just('X')))
        .then(one_of("0123456789ABCDEFabcdef").repeated().at_least(1))
        .map(|(minus, rest)|{
            let s: String = rest.into_iter().collect();
            match minus {
                Some(m) => Token::Num(String::from(m)+&s),
                None => Token::Num(s),
            }
        });

    let oct_num = just('-').or_not().then_ignore(just('0'))
        .then_ignore(just('o').or(just('O')))
        .then(one_of("01234567").repeated().at_least(1))
        .map(|(minus, rest)|{
            let s: String = rest.into_iter().collect();
            match minus {
                Some(m) => Token::Num(String::from(m)+&s),
                None => Token::Num(s),
            }
        });

    let bin_num = just('-').or_not().then_ignore(just('0'))
        .then_ignore(just('b').or(just('B')))
        .then(one_of("01").repeated().at_least(1))
        .map(|(minus, rest)|{
            let s: String = rest.into_iter().collect();
            match minus {
                Some(m) => Token::Num(String::from(m)+&s),
                None => Token::Num(s),
            }
        });

    let dec_num = text::int(10) // TODO: differentiate between ints and floats
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .then_ignore(just('U').or_not())
        .collect::<String>()
        .map(Token::Num);
    
    let num = hex_num
        .or(oct_num)
        .or(bin_num)
        .or(dec_num);

    // A parser for chars
    let char_ = just('\'')
        .ignore_then(filter(|c| *c != '\''))
        .then_ignore(just('\''))
        .map(Token::Char);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for operators
    let op = one_of(":@=!><&|^=+-*/%~?$")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};,.")
        .map(|c| Token::Separator(c));

/*     // A parser for preproccessor directives start
    let preproc = just("#include")
        .or(just("#pragma"))
        .or(just("#define"))
        .map(|s| Token::PreprocStart(s.to_string()));
    
    let preproc_str = just('<')
        .ignore_then(filter(|c| *c != '>' && *c != '\n').repeated())
        .then_ignore(just('>'))
        .collect::<String>()
        .map(Token::PreprocStr);*/

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
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
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = num
        .or(char_)
        .or(str_)
        //.or(preproc_str)
        .or(op)
        .or(ctrl)
        .or(ident)
        //.or(preproc)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();
    let directive = just("#").then(take_until(just('\n'))).padded();

    token
        .padded_by(comment.or(directive).repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}
