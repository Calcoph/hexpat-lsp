use std::{collections::HashMap, ops::Range, io::ErrorKind};

use nom::{
    IResult,
    branch::alt as choice,
    bytes::complete::{
        tag as just, take,
    },
    combinator::{
        eof,
        map,
        peek,
        not,
        opt, map_res
    },
    sequence::{pair as then, terminated, delimited, preceded, separated_pair},
    multi::{
        many_till as many_until,
        many1,
        many0,
        fold_many0,
        separated_list0,
        separated_list1
    }
};
use nom_supreme::error::{ErrorTree, BaseErrorKind};
use serde::{Deserialize, Serialize};

use crate::{token::{Spanned, TokSpan, Tokens, Token, Keyword, ValueType}, combinators::{spanned, ignore, to, map_with_span}, m_parser::{function::{function_statement, function_definition}, operations::{UnaryOp, mathematical_expression}}};

mod function;
mod factor;
mod operations;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    Char(char),
    Func(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Num(x) => write!(f, "{}", x),
            Value::Str(x) => write!(f, "{}", x),
            Value::Char(x) => write!(f, "{}", x),
            Value::Func(name) => write!(f, "<function: {}>", name),
        }
    }
}

// An expression node in the AST. Children are spanned so we can generate useful runtime errors.
#[derive(Debug, Clone)]
pub enum Expr {
    Error,
    Value{ val: Value },
    Dollar,
    ExprList { list: Vec<Spanned<Self>> },
    UnnamedParameter { type_: Spanned<HexType> },
    Local { name: Spanned<String> },
    Unary {
        operation: UnaryOp,
        operand: Box<Spanned<Self>>
    },
    Binary {
        loperand: Box<Spanned<Self>>,
        operator: BinaryOp,
        roperand: Box<Spanned<Self>>
    },
    Ternary {
        loperand: Box<Spanned<Self>>,
        moperand: Box<Spanned<Self>>,
        roperand: Box<Spanned<Self>>
    },
    Call {
        func_name: Box<Spanned<Self>>,
        arguments: Spanned<Vec<Spanned<Self>>>
    },
    If { // TODO: See what is up with unkown
        test_: Box<Spanned<Self>>,
        consequent: Box<Spanned<Self>>,
        alternative: Box<Spanned<Self>>
    },
    Definition {
        value_type: Spanned<HexTypeDef>,
        name: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>
    },
    BitFieldEntry {
        name: Spanned<String>,
        length: Box<Spanned<Self>>
    },
    EnumEntry {
        name: Spanned<String>,
        value: Box<Spanned<Self>>
    },
    NamespaceAccess {
        previous: Box<Spanned<Self>>,
        name: Spanned<String>
    },
    Using {
        new_name: Spanned<String>,
        old_name: Spanned<HexTypeDef>
    },
    Return { value: Box<Spanned<Self>> },
    Continue,
    Break,
    Func {
        name: Spanned<String>,
        args: Vec<Spanned<FuncArgument>>,
        body: Box<Spanned<Self>>
    },
    Struct {
        name: Spanned<String>,
        body: Box<Spanned<Self>>
    },
    Namespace {
        name: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>
    },
    Enum {
        name: Spanned<String>,
        value_type: Spanned<HexTypeDef>,
        body: Box<Spanned<Self>>
    },
    Bitfield {
        name: Spanned<String>,
        body: Box<Spanned<Self>>
    },
    Access {
        item: Box<Spanned<Self>>,
        member: Box<Spanned<Self>>
    },
    Attribute {
        arguments: Spanned<Vec<Spanned<Self>>>
    },
    AttributeArgument {
        name: Spanned<String>,
        value: Box<Spanned<Self>>
    },
    WhileLoop {
        condition: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>
    },
    ForLoop {
        var_init: Box<Spanned<Self>>,
        var_test: Box<Spanned<Self>>,
        var_change: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>,
    },
    Cast {
        cast_operator: Spanned<HexTypeDef>,
        operand: Box<Spanned<Self>>
    },
    Union {
        name: Spanned<String>,
        body: Box<Spanned<Self>>
    },
}

#[derive(Debug, Clone)]
pub enum FuncArgument {
    Parameter(Box<Spanned<Expr>>),
    ParameterPack(Spanned<String>)
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Mod,
    LShift,
    RShift,
    BAnd,
    BXor,
    BOr,
    GreaterEqual,
    LessEqual,
    Greater,
    Less,
    LAnd,
    LXor,
    LOr,
    Assign(Assignment)
}

#[derive(Debug, Clone, Copy)]
pub enum Assignment {
    Just,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    RShift,
    LShift,
    BOr,
    BAnd,
    BXor
}

// TODO: rework all parsers that use this
fn old_function_call<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            old_namespace_resolution,
            delimited(
                just(Token::Separator('(')),
                separated_list0(
                    just(Token::Separator(',')),
                    mathematical_expression
                ),
                just(Token::Separator(')')),
            )
        ),
        |(func_name, arguments)| {
            let args_span = if arguments.len() > 0 {
                arguments.get(0).unwrap().1.start..arguments.get(arguments.len()-1).unwrap().1.end
            } else {
                func_name.1.clone()
            };

            let arguments = (arguments, args_span);

            (
                Expr::Call {
                    func_name: Box::new(func_name),
                    arguments
                },
                func_name.1.start..args_span.end
            )
        }
    )(input)
}

// exaclty the same as above, but used when the rework has been done
fn function_call<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            namespace_resolution,
            delimited(
                just(Token::Separator('(')),
                separated_list0(
                    just(Token::Separator(',')),
                    mathematical_expression
                ),
                just(Token::Separator(')')),
            )
        ),
        |(func_name, arguments)| {
            let args_span = if arguments.len() > 0 {
                arguments.get(0).unwrap().1.start..arguments.get(arguments.len()-1).unwrap().1.end
            } else {
                func_name.1.clone()
            };

            let arguments = (arguments, args_span);

            (
                Expr::Call {
                    func_name: Box::new(func_name),
                    arguments
                },
                func_name.1.start..args_span.end
            )
        }
    )(input)
}

fn string_literal<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    todo!()
}

// TODO: Rework all the parsers so "ident" is not parsed before namespace_resolution.
fn old_namespace_resolution<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_name) = ident_local(input).unwrap(); // TODO: Error recovery instead of unwrap
    fold_many0(
        preceded(
            just(Token::Op("::")),
            ident
        ),
        || first_name,
        |previous, name| {
            let span = previous.1.start..name.1.end;
            let expr = Expr::NamespaceAccess {
                previous: Box::new(previous),
                name
            };

            (expr, span)
        }
    )(input)
}

// exaclty the same as above, but used when the rework has been done
fn namespace_resolution<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    let (input, first_name) = ident_local(input).unwrap(); // TODO: Error recovery instead of unwrap
    fold_many0(
        preceded(
            just(Token::Op("::")),
            ident
        ),
        || first_name,
        |previous, name| {
            let span = previous.1.start..name.1.end;
            let expr = Expr::NamespaceAccess {
                previous: Box::new(previous),
                name
            };

            (expr, span)
        }
    )(input)
}

// r_value
// TODO: Also parse the ident/parent/this that comes before this
fn old_member_access<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        choice((
            ident,
            map_with_span(
                just(Token::K(Keyword::Parent)),
                |_, span| (String::from("parent"), span)
            ),
            map_with_span(
                just(Token::K(Keyword::This)),
                |_, span| (String::from("this"), span)
            )
        )),
        then(
            opt(then(
                just(Token::Separator('[')),
                then(
                    mathematical_expression,
                    just(Token::Separator(']'))
                )
            )),
            opt(then(
                just(Token::Separator('.')),
                then(
                    choice((
                        ident,
                        to(just(Token::K(Keyword::Parent)), String::from("parent"))
                    )),
                    old_member_access
                )
            ))
        )
    )(input);
    todo!()
}

// exaclty the same as above, but used when the rework has been done
// r_value
fn member_access<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        choice((
            ident,
            map_with_span(
                just(Token::K(Keyword::Parent)),
                |_, span| (String::from("parent"), span)
            ),
            map_with_span(
                just(Token::K(Keyword::This)),
                |_, span| (String::from("this"), span)
            )
        )),
        then(
            opt(then(
                just(Token::Separator('[')),
                then(
                    mathematical_expression,
                    just(Token::Separator(']'))
                )
            )),
            opt(then(
                just(Token::Separator('.')),
                then(
                    choice((
                        ident,
                        to(just(Token::K(Keyword::Parent)), String::from("parent"))
                    )),
                    old_member_access
                )
            ))
        )
    )(input);
    todo!()
}

fn attribute_arg<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            ident,
            opt(spanned(delimited(
                just(Token::Separator('(')),
                string_literal,
                just(Token::Separator(')'))
            )))
        ),
        |((name, name_span), value)| {
            let (value, span) = match value {
                Some((val, span)) => {
                    let span = name_span.start..span.end;
                    (Box::new(val), span)
                },
                None => (Box::new((Expr::Value { val: Value::Null }, name_span.clone())), name_span),
            };
            let expr = Expr::AttributeArgument {
                name: (name, name_span),
                value
            };

            (expr, span)
        }
    )(input)
}

fn attribute<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        separated_list1(
            just(Token::Separator(',')),
            attribute_arg
        ),
        |arguments, span| {
            let expr = Expr::Attribute {
                arguments: (
                    arguments,
                    arguments.get(0).unwrap().1.start..arguments.get(arguments.len()-1).unwrap().1.end
                )
            };
            (expr, span)
        }
    )(input)
}

fn statement_body<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map_with_span(
            delimited(
                just(Token::Separator('{')),
                many0(function_statement),
                just(Token::Separator('}'))
            ),
            |list, span| (Expr::ExprList { list }, span)
        ),
        function_statement
    ))(input)
}

fn conditional<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::If)),
            then(
                delimited(
                    just(Token::Separator('(')),
                    mathematical_expression,
                    just(Token::Separator(')'))
                ),
                then(
                    choice((
                        map(
                            delimited(
                                just(Token::Separator('{')),
                                spanned(many0(member)),
                                just(Token::Separator('}'))
                            ),
                            |(list, span)| (Expr::ExprList { list }, span)
                        ),
                        member,
                    )),
                    opt(preceded(
                        just(Token::K(Keyword::Else)),
                        choice((
                            map(
                                delimited(
                                    just(Token::Separator('{')),
                                    spanned(many0(member)),
                                    just(Token::Separator('}'))
                                ),
                                |(list, span)| (Expr::ExprList { list }, span)
                            ),
                            member
                        ))
                    ))
                )
            )
        ),
        |(test_, (consequent, alternative)), span| {
            let alternative = Box::new(match alternative {
                Some(alt) => alt,
                None => (Expr::Value { val: Value::Null }, span),
            });
            (
                Expr::If {
                    test_: Box::new(test_),
                    consequent: Box::new(consequent),
                    alternative
                },
                span
            )
        }
    )(input)
}

#[derive(Debug, Clone)]
pub struct HexTypeDef {
    endianness: Endianness,
    name: Spanned<HexType>
}

#[derive(Debug, Clone)]
pub enum HexType {
    Custom(String),
    Path(Vec<String>),
    V(ValueType),
    Null
}

#[derive(Debug, Clone, Copy)]
pub enum Endianness {
    Little,
    Big,
    Unkown
}

fn namespace_access_to_hextype(expr: Expr) -> HexType {
    let mut v = vec![];
    recursive_namespace_access_to_hextype(expr, &mut v);
    HexType::Path(v)
}

fn recursive_namespace_access_to_hextype(expr: Expr, v: &mut Vec<String>) {
    match expr {
        Expr::Local { name: (name, _) } => v.push(name),
        Expr::NamespaceAccess { previous, name: (name, _) } => {
            v.push(name);
            recursive_namespace_access_to_hextype(previous.0, v)
        },
        Expr::Error => todo!(),
        _ => unreachable!()
    }
}

fn parse_type<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<HexTypeDef>> {
    map_with_span(
        then(
            opt(choice((
                to(just(Token::K(Keyword::LittleEndian)), Endianness::Little),
                to(just(Token::K(Keyword::BigEndian)), Endianness::Big)
            ))),
            choice((
                |input: Tokens<'a>| match namespace_resolution(input) {
                    Ok((input, (n_access, span))) => match n_access {
                        Expr::Local { name: (name, _) } => Ok((input, (HexType::Custom(name), span))),
                        Expr::NamespaceAccess { previous, name } => Ok((input, (namespace_access_to_hextype(Expr::NamespaceAccess { previous, name }), span))),
                        Expr::Error => todo!(),
                        _ => unreachable!()
                    },
                    Err(e) => Err(e),
                },
                value_type_any
            ))
        ),
        |(endianness, name), span| {
            let endianness = match endianness {
                Some((endianness, _)) => endianness,
                None => Endianness::Unkown,
            };

            (
                HexTypeDef {
                    endianness,
                    name
                },
                span
            )
        }
    )(input)
}

fn using_declaration<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<HexTypeDef>> {
    parse_type(input)
}

fn padding<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map_with_span(
            preceded(
                just(Token::K(Keyword::While)),
                delimited(
                    just(Token::Separator('(')),
                    mathematical_expression,
                    just(Token::Separator(')'))
                )
            ),
            |condition, span| (
                Expr::WhileLoop {
                    condition: Box::new(condition),
                    body: Box::new((Expr::Value { val: Value::Null }, span))
                },
                span
            )
        ),
        mathematical_expression
    ))(input)
}

fn pointer_size_type<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<HexTypeDef>> {
    parse_type(input)
}

fn assignment_expr<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        then(
            choice((
                ident_local,
                map_with_span(
                    just(Token::Op("$")),
                    |_, span| (Expr::Local { name: (String::from("$"), span) }, span)
                )
            )),
            then(
                opt(choice((
                    to(just(Token::Op("+")), Assignment::Add),
                    to(just(Token::Op("-")), Assignment::Sub),
                    to(just(Token::Op("*")), Assignment::Mul),
                    to(just(Token::Op("/")), Assignment::Div),
                    to(just(Token::Op("%")), Assignment::Mod),
                    to(just(Token::Op("<<")), Assignment::LShift),
                    to(just(Token::Op(">>")), Assignment::RShift),
                    to(just(Token::Op("|")), Assignment::BOr),
                    to(just(Token::Op("&")), Assignment::BAnd),
                    to(just(Token::Op("^")), Assignment::BXor),
                ))),
                preceded(
                    just(Token::Op("=")),
                    mathematical_expression
                )
            )
        ),
        |(loperand, (assignment, roperand)), span| {
            let assignment = match assignment {
                Some((ass, _)) => ass,
                None => Assignment::Just
            };
            let expr = Expr::Binary {
                loperand: Box::new(loperand),
                operator: BinaryOp::Assign(assignment),
                roperand: Box::new(roperand),
            };
    
            (expr, span)
        }
    )(input)
}

fn array_declr<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Option<Spanned<Expr>>> { // TODO: make an "array declaration" expression, so the "[","]" is also spanned and this returns Spanned<Expr> instead of Option<Spanned<Expr>>
    delimited(
        then(
            just(Token::Separator('[')),
            not(just(Token::Separator('[')))
        ),
        opt(choice((
            map_with_span(
                preceded(
                    just(Token::K(Keyword::While)),
                    delimited(
                        just(Token::Separator(')')),
                        mathematical_expression,
                        just(Token::Separator(')'))
                    )
                ),
                |a, span| (
                    Expr::WhileLoop {
                        condition: Box::new(a),
                        body: Box::new((Expr::Value { val: Value::Null }, span))
                    },
                    span
                )
            ),
            mathematical_expression
        ))),
        just(Token::Separator(']')),
    )(input)
}

fn member_declaration<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        peek(choice((
            ignore(just(Token::K(Keyword::BigEndian))),
            ignore(just(Token::K(Keyword::LittleEndian))),
            ignore(value_type_any),
            ignore(ident)
        ))),
        choice((
            function_call,
            choice((
                then(
                    namespace_resolution,
                    then(
                        array_declr,
                        opt(preceded(
                            just(Token::Op("@")),
                            mathematical_expression
                        ))
                    )
                ),
                then(
                    namespace_resolution,
                    choice((
                        preceded(
                            just(Token::Op("@")),
                            mathematical_expression
                        ),
                        map_with_span(
                            separated_list1(
                                just(Token::Separator(',')),
                                ident_local
                            ),
                            |list, span| (Expr::ExprList { list }, span)
                        )
                    ))
                ),
                separated_pair(
                    parse_type,
                    just(Token::Op("*")),
                    then(
                        separated_pair(
                            ident,
                            just(Token::Op(":")),
                            pointer_size_type
                        ),
                        opt(preceded(
                            just(Token::Op("@")),
                            mathematical_expression
                        ))
                    )
                ),
                separated_pair(
                    parse_type,
                    just(Token::Op("*")),
                    then(
                        ident,
                        then(
                            array_declr,
                            preceded(
                                just(Token::Op(":")),
                                then(
                                    pointer_size_type,
                                    opt(preceded(
                                        just(Token::Op("@")),
                                        mathematical_expression
                                    ))
                                )
                            )
                        )
                    )
                )
            ))
        ))
    )(input)
}

fn member<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    terminated(
        choice((
            assignment_expr,
            member_declaration,
            preceded(
                just(Token::V(ValueType::Padding)),
                delimited(
                    just(Token::Separator('[')),
                    padding,
                    just(Token::Separator(']'))
                )
            ),
            conditional,
            to(just(Token::K(Keyword::Break)), Expr::Break),
            to(just(Token::K(Keyword::Continue)), Expr::Continue)
        )),
        then(
            opt(delimited(
                then(
                    just(Token::Separator('[')),
                    just(Token::Separator('['))
                ),
                attribute,
                then(
                    just(Token::Separator(']')),
                    just(Token::Separator(']'))
                )
            )),
            many1(just(Token::Separator(';')))
        )
    )(input)
}
    
fn parse_struct<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::Struct)),
            then(
                then(
                    ident,
                    opt(preceded(
                        just(Token::Op(":")),
                        separated_list1(
                            just(Token::Separator(',')),
                            ident
                        )
                    )),
                ),
                delimited(
                    just(Token::Separator('{')),
                    spanned(many0(member)),
                    just(Token::Separator('}'))
                )
            )
        ),
        |((name, inheritance), (body, body_span)), span| ( // TODO: Take into account the inheritance
            Expr::Struct {
                name,
                body: Box::new((Expr::ExprList { list: body }, body_span))
            },
            span
        )
    )(input)
}

fn parse_union<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::Union)),
            then(
                ident,
                delimited(
                    just(Token::Separator('{')),
                    spanned(many0(member)),
                    just(Token::Separator('}'))
                )
            )
        ),
        |(name, (body, body_span)), span| (
            Expr::Union {
                name,
                body: Box::new((Expr::ExprList { list: body }, body_span))
            },
            span
        )
    )(input)
}

fn parse_enum<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::Enum)),
            then(
                separated_pair(
                    ident,
                    just(Token::Separator(':')),
                    parse_type
                ),
                delimited(
                    just(Token::Separator('{')),
                    spanned(separated_list0(
                        just(Token::Separator(',')),
                        choice((
                            terminated(
                                ident_local,
                                then(
                                    just(Token::Op("=")),
                                    mathematical_expression
                                )
                            ),
                            ident_local
                        )),
                    )),
                    just(Token::Separator('}'))
                )
            )
        ),
        |((name, value_type), (entries, entries_span)), span| (
            Expr::Enum {
                name,
                value_type,
                body: Box::new((Expr::ExprList { list: entries }, entries_span))
            },
            span
        )
    )(input)
}

fn bitfield_conditional<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::If)),
            then(
                delimited(
                    just(Token::Separator('(')),
                    mathematical_expression,
                    just(Token::Separator(')'))
                ),
                then(
                    choice((
                        map(
                            delimited(
                                just(Token::Separator('{')),
                                spanned(many0(bitfield_entry)),
                                just(Token::Separator('}'))
                            ),
                            |(list, span)| (
                                Expr::ExprList { list },
                                span
                            )
                        ),
                        bitfield_entry
                    )),
                    opt(preceded(
                        just(Token::K(Keyword::Else)),
                        choice((
                            map(
                                delimited(
                                    just(Token::Separator('{')),
                                    spanned(many0(bitfield_entry)),
                                    just(Token::Separator('}'))
                                ),
                                |(list, span)| (
                                    Expr::ExprList { list },
                                    span
                                )
                            ),
                            bitfield_entry
                        ))
                    ))
                )
            )
        ),
        |(test_, (consequent, alternative)), span| {
            let alternative = Box::new(match alternative {
                Some(alt) => alt,
                None => (Expr::Value { val: Value::Null }, span),
            });
            (
                Expr::If {
                    test_: Box::new(test_),
                    consequent: Box::new(consequent),
                    alternative
                },
                span
            )
        }
    )(input)
}

fn bitfield_entry<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    terminated(
        choice((
            map_with_span(
                separated_pair(
                    ident,
                    just(Token::Op(":")),
                    mathematical_expression
                ),
                |(name, length), span| (
                    Expr::BitFieldEntry {
                        name,
                        length: Box::new(length)
                    },
                    span
                )
            ),
            map_with_span(
                separated_pair(
                    spanned(just(Token::V(ValueType::Padding))),
                    just(Token::Op(":")),
                    mathematical_expression
                ),
                |((_, pad_span), length), span| (
                    Expr::BitFieldEntry {
                        name: (String::from("padding"), pad_span),
                        length: Box::new(length)
                    },
                    span
                )
            ),
            bitfield_conditional
        )),
        many0(just(Token::Separator(';')))
    )(input)
}

fn parse_bitfield<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::Bitfield)),
            delimited(
                just(Token::Separator('{')),
                then(
                    ident,
                    terminated(
                        separated_list0(
                            many0(just(Token::Separator(';'))),
                            bitfield_entry
                        ),
                        many0(just(Token::Separator(';')))
                    )
                ),
                just(Token::Separator('}'))
            )
        ),
        |(name, body), span| {
            let body = Box::new({
                let span = match body.len() {
                    0 => span,
                    _ => body.get(0).unwrap().1.start..body.get(body.len()-1).unwrap().1.end
                };
                
                (
                    Expr::ExprList { list: body },
                    span
                )
            });
            (
                Expr::Bitfield {
                    name,
                    body
                },
                span
            )
        }
    )(input)
}

fn array_variable_placement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        then(
            ident_local,
            then(
                array_declr,
                preceded(
                    just(Token::Op("@")),
                    mathematical_expression
                )
            )
        ),
        |(name, (array, body)), span| { // TODO: Take array into account
            let array = Box::new(match array {
                Some(s) => s,
                None => (Expr::Value { val: Value::Null }, span)
            });
            (
                Expr::Definition {
                    value_type: (HexTypeDef {endianness: Endianness::Unkown, name: (HexType::Null, span)}, span),
                    name: Box::new(name),
                    body: Box::new(body)
                },
                span
            )
        }
    )(input)
}

fn pointer_array_variable_placement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            opt(
                then(
                    not(just(Token::Separator(']'))),
                    choice((
                        map(
                            then(
                                then(
                                    just(Token::K(Keyword::While)),
                                    just(Token::Separator('('))
                                ),
                                then(
                                    mathematical_expression,
                                    just(Token::Separator(')'))
                                )
                            ),
                            |(a, (b, c))| b // TODO
                        ),
                        mathematical_expression
                    ))
                )
            ),
            then(
                pointer_size_type,
                mathematical_expression
            )
        ),
        |(a, (b, c))| c // TODO
    )(input)
}

fn parse_namespace<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            just(Token::K(Keyword::Namespace)),
            then(
                separated_list1(
                    just(Token::Op("::")),
                    ident
                ),
                delimited(
                    just(Token::Separator('{')),
                    spanned(many0(statements)),
                    just(Token::Separator('}'))
                )
            )
        ),// ((Null::a)::b)::c
        |(name, (body, body_span)), span| {
            let name = Box::new(name.into_iter()
                .fold((Expr::Value { val: Value::Null }, 0..1), |(accum, acc_span), (next, next_span)| {
                    match accum {
                        Expr::Value { .. } => (Expr::Local { name: (next, next_span) }, next_span), // first case
                        _ => (Expr::NamespaceAccess { previous: Box::new((accum, acc_span)), name: (next, next_span) }, acc_span.start..next_span.end)
                    }
                }));
            (
                Expr::Namespace {
                    name,
                    body: Box::new((Expr::ExprList { list: body }, body_span))
                },
                span
            )
        }
    )(input)
}

fn placement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        parse_type,
        choice((
            array_variable_placement,
            map_with_span(
                then(
                    ident_local,
                    opt(choice((
                        map(
                            preceded(
                                just(Token::Op("@")),
                                mathematical_expression
                            ),
                            |a| Some(a)
                        ),
                        map(just(Token::K(Keyword::In)), |_| None),
                        map(just(Token::K(Keyword::Out)), |_| None),
                    )))
                ),
                |(name, body), span| {
                    let body = Box::new(match body {
                        Some(body) => match body {
                            Some(body) => body,
                            None => (Expr::Value { val: Value::Null }, span),
                        },
                        None => (Expr::Value { val: Value::Null }, span),
                    });
    
                    (
                        Expr::Definition {
                            value_type: (HexTypeDef {endianness: Endianness::Unkown, name: (HexType::Null, span)}, span),
                            name: Box::new(name),
                            body
                        },
                        span
                    )
                }
            ),
            map(
                preceded(
                    just(Token::Op("*")),
                    separated_pair(
                        ident,
                        just(Token::Op(":")),
                        then(
                            pointer_size_type,
                            mathematical_expression
                        )
                    )
                ),
                |(a, (b, c))| b // TODO
            ),
            map(
                then(
                    just(Token::Op("*")),
                    then(
                        ident,
                        then(
                            just(Token::Separator('{')),
                            pointer_array_variable_placement
                        )
                    )
                ),
                |(a, b)| b // TODO
            )
        ))
    )(input)
}

fn statements_choice<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map_with_span(
            preceded(
                just(Token::K(Keyword::Using)),
                then(
                    ident,
                    opt(preceded(
                        just(Token::Op("=")),
                        using_declaration
                    ))
                )
            ),
            |(a, b), span| (
                Expr::Using {
                    new_name: todo!(),
                    old_name: todo!()
                },
                span
            )
        ),
        preceded(
            peek(choice((
                    ignore(just(Token::K(Keyword::BigEndian))),
                    ignore(just(Token::K(Keyword::LittleEndian))),
                    ignore(value_type_any),
            ))),
            placement // TODO: Assign the parsed type to the type-less definition returned by placement
        ),
        preceded(
            peek(then(
                ident,
                then(
                    not(just(Token::Op("="))),
                    then(
                        not(just(Token::Separator('.'))),
                        not(just(Token::Separator('[')))
                    )
                )
            )),
            choice((
                function_call,
                placement
            ))
        ),
        parse_struct,
        parse_union,
        parse_enum,
        parse_bitfield,
        function_definition,
        parse_namespace,
        function_statement
    ))(input)
}

fn statements<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            statements_choice,
            opt(
                preceded(
                    then(
                        just(Token::Separator('[')),
                        just(Token::Separator('['))
                    ),
                    attribute
                )
            ),
        ),
        |(a, b)| a // TODO
    )(input)// TODO: Consume all semicolons with nothing in between
}

fn add_type<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
   todo!()
}

fn ident<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<String>> {
    map_res(
        spanned(take(1 as usize)),
        |(consumed, span): (Tokens, Range<usize>)|{
            match consumed.tokens[0].fragment() {
                Token::Ident(s) => Ok((String::from(*s), span)),
                _ => Err(ErrorTree::Base {
                    location: consumed,
                    kind: BaseErrorKind::External(Box::new(tokio::io::Error::new(ErrorKind::Other, "Expected identifier")))
                }) // TODO: Expand match tree for "found X"
            }
        }
    )(input)
}

fn ident_local<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        ident,
        |(name, span)| (Expr::Local { name: (name, span.clone()) }, span)
    )(input)
}

fn value_type_any<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<HexType>> {
    map_res(
        spanned(take(1 as usize)),
        |(consumed, span): (Tokens, Range<usize>)|{
            match consumed.tokens[0].fragment() {
                Token::V(type_) => Ok((HexType::V(*type_), span)), // TODO: Parse the number instead of always being 0.0
                _ => Err(ErrorTree::Base {
                    location: consumed,
                    kind: BaseErrorKind::External(Box::new(tokio::io::Error::new(ErrorKind::Other, "Expceted identifier")))
                }) // TODO: Expand match tree for "found X"
            }
        }
    )(input)
}

fn numeric<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_res(
        spanned(take(1 as usize)),
        |(consumed, span): (Tokens, Range<usize>)|{
            match consumed.tokens[0].fragment() {
                Token::Num(_) => Ok((Expr::Value { val: Value::Num(0.0) }, span)), // TODO: Parse the number instead of always being 0.0
                _ => Err(ErrorTree::Base {
                    location: consumed,
                    kind: BaseErrorKind::External(Box::new(tokio::io::Error::new(ErrorKind::Other, "Expceted identifier")))
                }) // TODO: Expand match tree for "found X"
            }
        }
    )(input)
}

fn placeholder_parser<'a>(input: Tokens<'a>) -> IResult<Tokens, Spanned<Expr>> {
    map_with_span(
        many_until(
            statements,
            eof
        ),
        |(list, _), span| (Expr::ExprList { list }, span)
    )(input)
}

#[derive(Debug)]
pub enum NamedNode {
    Variable,
    Function(Vec<String>),
    Struct,
    Enum,
    NameSpace,
    BitField
}

// Hashmap contains the names of named expressions and their clones
pub fn placeholder_parse(tokens: Vec<TokSpan>) -> (HashMap<String, Spanned<NamedNode>>, Spanned<Expr>) {
    let hmap = HashMap::new();
    let (_, ex) = placeholder_parser(Tokens{tokens: &tokens}).expect("Unrecovered error happenned in parser");
    //let ex = (Expr::Dollar, 0..1);
    (hmap, ex)
}
