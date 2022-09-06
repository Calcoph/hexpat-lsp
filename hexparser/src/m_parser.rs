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
        fold_many0, separated_list0, separated_list1
    }
};
use nom_supreme::error::{ErrorTree, BaseErrorKind};
use serde::{Deserialize, Serialize};

use crate::{token::{Spanned, TokSpan, Tokens, Token, Keyword, ValueType, BuiltFunc}, combinators::{spanned, ignore, to, map_with_span}, m_parser::function::{function_statement, function_definition}};

mod function;

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
        operation: BinaryOp,
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
        name: Spanned<String>,
        body: Box<Spanned<Self>>
    },
    Enum {
        name: Spanned<String>,
        body: Box<Spanned<Self>>,
        value_type: Spanned<String>
    },
    Bitfield {
        name: Spanned<String>,
        body: Box<Spanned<Self>>
    },
    Access { // TODO: See what's up with unkown
        unkown: Box<Spanned<Self>>,
        unkown2: Box<Spanned<Self>>
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
    }
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

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Add,
    Sub,
    LNot,
    BNot,
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

// TODO: Find difference between this and namespace_resolution
fn scope_resolution<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    many0(then(
        just(Token::Op("::")),
        then(
            ident,
            opt(peek(then(
                just(Token::Op("::")),
                ident
            )))
        )
    ))(input);
    todo!()
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

fn factor<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        numeric,
        preceded(
            peek(choice((
                just(Token::Op("+")),
                just(Token::Op("-")),
                just(Token::Op("~")),
                just(Token::Op("!"))
            ))),
            mathematical_expression
        ),
        delimited(
            just(Token::Separator('(')),
            mathematical_expression,
            just(Token::Separator(')'))
        ),
        map(
            then(
                namespace_resolution,
                choice((
                    old_function_call,
                    scope_resolution,
                    old_member_access
                ))
            ),
            |(_, (expr, _))| expr // TODO
        ),
        preceded( // TODO
            choice((
                just(Token::K(Keyword::Parent)),
                just(Token::K(Keyword::This))
            )),
            old_member_access
        ),
        map_with_span(
            just(Token::Op("$")),
            |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
        ),
        preceded( // TODO
            choice((
                ignore(just(Token::B(BuiltFunc::AddressOf))),
                ignore(just(Token::B(BuiltFunc::SizeOf))),
            )),
            delimited(
                just(Token::Separator('(')),
                choice((
                    map(
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
                            old_member_access
                        ),
                        |(_, expr)| expr // TODO
                    ),
                    map(
                        value_type_any,
                        |(type_, span)| (
                            Expr::UnnamedParameter { type_: (type_, span) },
                            span
                        )
                    ),
                    map_with_span(
                        just(Token::Op("$")),
                        |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
                    ),
                )),
                just(Token::Separator(')'))
            )
        )
    ))(input)
}

fn cast_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map(
            then(
                peek(choice((
                    ignore(just(Token::K(Keyword::BigEndian))),
                    ignore(just(Token::K(Keyword::LittleEndian))),
                    ignore(value_type_any)
                ))),
                then(
                    parse_type,
                    then(
                        peek(just(Token::Separator('('))),
                        factor
                    )
                )
            ),
            |(_, (_, (_, expr)))| expr // TODO
        ),
        factor
    ))(input)
}

fn unary_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map(
            then(
                choice((
                    to(just(Token::Op("+")), UnaryOp::Add),
                    to(just(Token::Op("-")), UnaryOp::Sub),
                    to(just(Token::Op("!")), UnaryOp::LNot),
                    to(just(Token::Op("~")), UnaryOp::BNot)
                )),
                cast_expression
            ),
            |((operation, op_span), operand)| {
                let expr = Expr::Unary {
                    operation,
                    operand: Box::new(operand)
                };

                (expr, op_span.start..operand.1.end)
            }
        ),
        string_literal,
        cast_expression
    ))(input)
}

fn multiplicative_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            unary_expression,
            many0(then(
                choice((
                    just(Token::Op("*")),
                    just(Token::Op("/")),
                    just(Token::Op("%"))
                )),
                unary_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn additive_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            multiplicative_expression,
            many0(then(
                choice((
                    just(Token::Op("+")),
                    just(Token::Op("-"))
                )),
                multiplicative_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn shift_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            additive_expression,
            many0(then(
                choice((
                    just(Token::Op("<<")),
                    just(Token::Op(">>"))
                )),
                additive_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn binary_and_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            shift_expression,
            many0(then(
                just(Token::Op("&")),
                shift_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn binary_xor_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            binary_and_expression,
            many0(then(
                just(Token::Op("^")),
                binary_and_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn binary_or_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            binary_xor_expression,
            many0(then(
                just(Token::Op("|")),
                binary_xor_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn relation_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            binary_or_expression,
            many0(then(
                choice((
                    just(Token::Op(">")),
                    just(Token::Op("<")),
                    just(Token::Op(">=")),
                    just(Token::Op("<="))
                )),
                binary_or_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn equality_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            relation_expression,
            many0(then(
                choice((
                    just(Token::Op("==")),
                    just(Token::Op("!="))
                )),
                relation_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn boolean_and<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            equality_expression,
            many0(then(
                just(Token::Op("&&")),
                equality_expression
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn boolean_xor<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            boolean_and,
            many0(then(
                just(Token::Op("^^")),
                boolean_and
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn boolean_or<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            boolean_xor,
            many0(then(
                just(Token::Op("||")),
                boolean_xor
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn ternary_conditional<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            boolean_or,
            many0(then(
                just(Token::Op("?")),
                then(
                    boolean_or,
                    then(
                        just(Token::Op(":")),
                        boolean_or
                    )
                )
            ))
        ),
        |(a, b)| a // TODO
    )(input)
}

fn mathematical_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    ternary_conditional(input)
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
        terminated(
            separated_list1(
                just(Token::Separator(',')),
                attribute_arg
            ),
            then(
                just(Token::Separator(']')),
                just(Token::Separator(']'))
            )
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
        then(
            mathematical_expression,
            then(
                choice((
                    map_with_span(
                        preceded(
                            just(Token::Separator(')')),
                            delimited(
                                just(Token::Separator('{')),
                                many0(member),
                                just(Token::Separator('}'))
                            )
                        ),
                        |list, span| (Expr::ExprList { list }, span)
                    ),
                    map(
                        preceded(
                            just(Token::Separator(')')),
                            member
                        ),
                        |a| a
                    )
                )),
                opt(preceded(
                    just(Token::K(Keyword::Else)),
                    choice((
                        map_with_span(
                            delimited(
                                just(Token::Separator('{')),
                                many0(member),
                                just(Token::Separator('}'))
                            ),
                            |list, span| (Expr::ExprList { list }, span)
                        ),
                        member
                    ))
                ))
            )
        ),
        |a, span| (a, span) // TODO
    )(input);
    todo!()
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
    terminated(
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
        )),
        just(Token::Separator(']'))
    )(input)
}

fn pointer_size_type<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<HexTypeDef>> {
    parse_type(input)
}

fn member_pointer_variable<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        pointer_size_type,
        opt(then(
            just(Token::Op("@")),
            mathematical_expression
        ))
    )
}

fn member_pointer_array_variable<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        opt(then(
            just(Token::Separator(']')),
            then(
                choice((
                    then(
                        just(Token::K(Keyword::While)),
                        then(
                            just(Token::Separator(')')),
                            then(
                                mathematical_expression,
                                just(Token::Separator(')'))
                            )
                        )
                    ),
                    mathematical_expression
                )),
                just(Token::Separator(']'))
            )
        )),
        then(
            just(Token::Op(":")),
            then(
                pointer_size_type,
                opt(then(
                    just(Token::Op("@")),
                    mathematical_expression
                ))
            )
        )
    )
}

fn member<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        choice((
            then(
                just(Token::Op("$")),
                then(
                    just(Token::Op("=")),
                    function_variable_assignment
                )
            ),
            then(
                just(Token::Op("$")),
                then(
                    choice((
                        just(Token::Op("+")),
                        just(Token::Op("-")),
                        just(Token::Op("*")),
                        just(Token::Op("/")),
                        just(Token::Op("%")),
                        just(Token::Op("<<")),
                        just(Token::Op(">>")),
                        just(Token::Op("|")),
                        just(Token::Op("&")),
                        just(Token::Op("^")),
                    )),
                    then(
                        just(Token::Op("=")),
                        function_variable_compound_assignment
                    )
                )
            ),
            then(
                ident,
                then(
                    just(Token::Op("=")),
                    function_variable_assignment
                )
            ),
            then(
                ident,
                then(
                    choice((
                        just(Token::Op("+")),
                        just(Token::Op("-")),
                        just(Token::Op("*")),
                        just(Token::Op("/")),
                        just(Token::Op("%")),
                        just(Token::Op("<<")),
                        just(Token::Op(">>")),
                        just(Token::Op("|")),
                        just(Token::Op("&")),
                        just(Token::Op("^")),
                    )),
                    then(
                        just(Token::Op("=")),
                        function_variable_compound_assignment
                    )
                )
            ),
            then(
                peek(choice((
                    ignore(just(Token::K(Keyword::BigEndian))),
                    ignore(just(Token::K(Keyword::LittleEndian))),
                    ignore(value_type_any),
                    ignore(ident)
                ))),
                then(
                    opt(peek(then(
                        namespace_resolution,
                        opt(then(
                            peek(just(Token::Separator(')'))),
                            old_function_call
                        ))
                    ))),
                    todo!()
                )
            ),
            then(
                just(Token::V(ValueType::Padding)),
                then(
                    just(Token::Separator('[')),
                    padding
                )
            ),
            then(
                just(Token::K(Keyword::If)),
                then(
                    just(Token::Separator('(')),
                    conditional
                )
            ),
            just(Token::K(Keyword::Break)),
            just(Token::K(Keyword::Continue))
        )),
        then(
            opt(then(
                just(Token::Separator('[')),
                then(
                    just(Token::Separator('[')),
                    attribute
                )
            )),
            many1(just(Token::Separator(';')))
        )
    )
}
    
fn parse_struct<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            just(Token::K(Keyword::Struct)),
            then(
                ident,
                then(
                    opt(then(
                        just(Token::Op(":")),
                        then(
                            ident,
                            many0(then(
                                just(Token::Separator(',')),
                                ident
                            ))
                        )
                    )),
                    then(
                        just(Token::Separator('{')),
                        terminated(
                            many0(member),
                            just(Token::Separator('}'))
                        )
                    )
                )
            )
        ),
        |((a, b), c)| b // TODO
    )(input);
    todo!()
}

fn parse_union<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            just(Token::K(Keyword::Union)),
            then(
                just(Token::Separator('{')),
                then(
                    ident,
                    terminated(
                        many0(member),
                        just(Token::Separator('}'))
                    )
                )
            )
        ),
        |(((a, b), c), d)| c // TODO
    )(input)
}

fn parse_enum<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            then(
                just(Token::K(Keyword::Enum)),
                just(Token::Separator(':'))
            ),
            then(
                then(
                    ident,
                    parse_type,
                ),
                delimited(
                    just(Token::Separator('{')),
                    separated_list0(
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
                    ),
                    just(Token::Separator('}'))
                )
            )
        ),
        |(a, (b, c)), span| (a, span) // TODO
    )(input)
}

fn bitfield_if<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        preceded(
            then(
                just(Token::K(Keyword::If)),
                just(Token::Separator('(')),
            ),
            then(
                mathematical_expression,
                then(
                    choice((
                        map(
                            preceded(
                                just(Token::Separator(')')),
                                delimited(
                                    just(Token::Separator('{')),
                                    spanned(many0(bitfield_entry)),
                                    just(Token::Separator('}'))
                                )
                            ),
                            |(list, span)| (
                                Expr::ExprList { list },
                                span
                            )
                        ),
                        map(
                            preceded(
                                just(Token::Separator(')')),
                                bitfield_entry
                            ),
                            |a| a
                        )
                    )),
                    opt(choice((
                        map(
                            delimited(
                                then(
                                    just(Token::K(Keyword::Else)),
                                    just(Token::Separator('{'))
                                ),
                                spanned(many0(bitfield_entry)),
                                just(Token::Separator('}'))
                            ),
                            |(list, span)| (
                                Expr::ExprList { list },
                                span
                            )
                        ),
                        preceded(
                            just(Token::K(Keyword::Else)),
                            bitfield_entry
                        ),
                    )))
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
            bitfield_if
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

fn forward_declaration<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    todo!()
}

fn array_variable_placement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            opt(then(
                just(Token::Separator(']')),
                choice((
                    map(
                        then(
                            just(Token::K(Keyword::While)),
                            then(
                                just(Token::Separator('(')),
                                then(
                                    mathematical_expression,
                                    just(Token::Separator(')'))
                                )
                            )
                        ),
                        |(a, (b, c))| c // TODO
                    ),
                    mathematical_expression
                ))
            )),
            mathematical_expression
        ),
        |(a, b)| b // TODO
    )(input)
}

fn pointer_variable_placement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            pointer_size_type,
            mathematical_expression
        ),
        |(a, b)| a // TODO
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
    map(
        then(
            just(Token::K(Keyword::Namespace)),
            then(
                ident,
                then(
                    ident,
                    many0(then(
                        just(Token::Op("::")),
                        ident
                    ))
                )
            )
        ),
        |((a, b), c)| b // TODO
    )(input)
}

fn placement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map_with_span(
            then(
                ident_local,
                preceded(
                    just(Token::Separator('[')),
                    array_variable_placement
                )
            ),
            |(name, body), span| (
                Expr::Definition {
                    value_type: (HexTypeDef {endianness: Endianness::Unkown, name: (HexType::Null, span)}, span),
                    name: Box::new(name),
                    body: Box::new(body)
                },
                span
            )
        ),
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
            then(
                then(
                    then(
                        just(Token::Op("*")),
                        ident
                    ),
                    just(Token::Op(":"))
                ),
                pointer_variable_placement
            ),
            |(a, b)| b // TODO
        ),
        map(
            then(
                then(
                    then(
                        just(Token::Op("*")),
                        ident
                    ),
                    just(Token::Separator('{'))
                ),
                pointer_array_variable_placement
            ),
            |(a, b)| b // TODO
        )
    ))(input)
}

fn statements_choice<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map_with_span(
            preceded(
                just(Token::K(Keyword::Using)),
                then(
                    ident,
                    opt(then(
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
        map(
            then(
                peek(choice((
                        ignore(just(Token::K(Keyword::BigEndian))),
                        ignore(just(Token::K(Keyword::LittleEndian))),
                        ignore(value_type_any),
                ))),
                placement // TODO: Assign the parsed type to the type-less definition returned by placement
            ),
            |(_, a)| a
        ),
        map(
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
                then(
                    namespace_resolution,
                    choice((
                        map(
                            then(
                                peek(just(Token::Separator('('))),
                                old_function_call
                            ),
                            |(_, b)| b
                        ),
                        placement // TODO: Assign the parsed type to the type-less definition returned by placement
                    ))
                )
            ),
            |(a, b)| b // TODO
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
                    kind: BaseErrorKind::External(Box::new(tokio::io::Error::new(ErrorKind::Other, "Expceted identifier")))
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
    todo!()
}

fn value_type_auto<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    todo!()
}

fn numeric<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    todo!()
}

fn placeholder_parser<'a>(input: Tokens<'a>) -> IResult<Tokens, Spanned<Expr>> {
    map(
        many_until(
            statements,
            eof
        ),
        |(v, _)| v.into_iter().next().unwrap() // TODO: Fold into ExprList instead of return first element
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
