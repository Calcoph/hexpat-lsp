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
    sequence::{pair as then, terminated},
    multi::{
        many_till as many_until,
        many1, many0, fold_many0, fold_many1
    }
};
use nom_supreme::error::{ErrorTree, BaseErrorKind, Expectation};
use serde::{Deserialize, Serialize};

use crate::{token::{Spanned, TokSpan, Tokens, Token, Keyword, ValueType, BuiltFunc}, recovery_err::ToRange, combinators::{spanned, ignore, to}};

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
    Value{val: Value},
    Dollar,
    ExprList {list: Vec<Spanned<Self>> },
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
        value_type: Spanned<String>,
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
    Using { type_name: Box<Spanned<Self>> },
    Return { unkown: Box<Spanned<Self>> }, // TODO: See what's up with unkown
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

fn function_call<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            old_namespace_resolution,
            then(
                just(Token::Separator('(')),
                terminated(
                    then(
                        many0(then(
                            mathematical_expression,
                            just(Token::Separator(','))
                        )),
                        opt(mathematical_expression)
                    ),
                    just(Token::Separator(')')),
                )
            )
        ),
        |(func_name, (_, (arguments, last_arg)))| {
            let mut arguments = arguments.into_iter()
                .map(|(arg, _)| arg)
                .collect::<Vec<_>>();
            if let Some(last) = last_arg {
                arguments.push(last);
            };
            let args_span = if arguments.len() > 0 {
                arguments.get(0).unwrap().1.start..arguments.get(arguments.len()-1).unwrap().1.end
            } else {
                func_name.1.clone()
            };
            let arguments = (arguments, args_span);
            let expr = Expr::Call {
                func_name: Box::new(func_name),
                arguments
            };
            let span = func_name.1.start..args_span.end;
            (expr, span)
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
        then(
            just(Token::Op("::")),
            ident
        ),
        || first_name,
        |previous, (_, name)| {
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
        then(
            just(Token::Op("::")),
            ident
        ),
        || first_name,
        |previous, (_, name)| {
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
            map(
                spanned(just(Token::K(Keyword::Parent))),
                |(_, span)| (String::from("parent"), span)
            ),
            map(
                spanned(just(Token::K(Keyword::This))),
                |(_, span)| (String::from("this"), span)
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
                        just(Token::K(Keyword::Parent))
                    )),
                    old_member_access
                )
            ))
        )(input)
    )
}

// exaclty the same as above, but used when the rework has been done
fn member_access<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        choice((
            ident,
            map(
                spanned(just(Token::K(Keyword::Parent))),
                |(_, span)| (String::from("parent"), span)
            ),
            map(
                spanned(just(Token::K(Keyword::This))),
                |(_, span)| (String::from("this"), span)
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
                        just(Token::K(Keyword::Parent))
                    )),
                    old_member_access
                )
            ))
        )(input)
    )
}

fn factor<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map(
            spanned(numeric),
            |(_, span)| (Expr::Value { val: Value::Num(0.0) }, span) // TODO: Parse the number
        ),
        map(
            then(
                peek(choice((
                    just(Token::Op("+")),
                    just(Token::Op("-")),
                    just(Token::Op("~")),
                    just(Token::Op("!"))
                ))),
                mathematical_expression
            ),
            |(_, expr)| expr
        ),
        map(
            then(
                just(Token::Separator('(')),
                then(
                    mathematical_expression,
                    just(Token::Separator(')'))
                )
            ),
            |(_, (expr, _))| expr
        ),
        map(
            then(
                ident,
                then(
                    old_namespace_resolution,
                    choice((
                        function_call,
                        scope_resolution,
                        old_member_access
                    ))
                )
            ),
            |(_, (expr, _))| expr // TODO
        ),
        map(
            then(
                choice((
                    just(Token::K(Keyword::Parent)),
                    just(Token::K(Keyword::This))
                )),
                old_member_access
            ),
            |(_, expr)| expr // TODO
        ),
        map(
            spanned(just(Token::Op("$"))),
            |(_, span)| (Expr::Local { name: (String::from("Dollar"), span.clone()) }, span)
        ),
        map(
            then(
                choice((
                    ignore(just(Token::B(BuiltFunc::AddressOf))),
                    ignore(just(Token::B(BuiltFunc::SizeOf))),
                )),
                then(
                    just(Token::Separator('(')),
                    then(
                        choice((
                            map(
                                then(
                                    choice((
                                        ident,
                                        map(
                                            spanned(just(Token::K(Keyword::Parent))),
                                            |(_, span)| (String::from("parent"), span)
                                        ),
                                        map(
                                            spanned(just(Token::K(Keyword::This))),
                                            |(_, span)| (String::from("this"), span)
                                        )
                                    )),
                                    old_member_access
                                ),
                                |(_, expr)| expr // TODO
                            ),
                            value_type_any,
                            map(
                                spanned(just(Token::Op("$"))),
                                |(_, span)| (Expr::Local { name: (String::from("Dollar"), span.clone()) }, span)
                            ),
                        )),
                        just(Token::Separator(')'))
                    )
                )
            ),
            |(a, (b, expr))| expr // TODO
        )
    ))(input);
    todo!()
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
            opt(spanned(then(
                just(Token::Separator('(')),
                then(
                    string_literal,
                    just(Token::Separator(')'))
                )
            )))
        ),
        |((name, name_span), value)| {
            let (value, span) = match value {
                Some(((_, (val, _)), span)) => {
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
    map(
        spanned(then(
            then(
                attribute_arg,
                many0(
                    then(
                        just(Token::Separator(',')),
                        attribute_arg
                    )
                )
            ),
            then(
                just(Token::Separator(']')),
                just(Token::Separator(']'))
            )
        )),
        |(((first, arguments), _), span)| {
            let mut arguments = arguments.into_iter()
            .map(|(_, arg)| {
                arg
            }).collect::<Vec<_>>();
            let arg_span = if arguments.len() > 0 {
                first.1.start..arguments.get(arguments.len()-1).unwrap().1.end
            } else {
                first.1
            };
            arguments.insert(0, first);
            let expr = Expr::Attribute {
                arguments: (arguments, arg_span)
            };
            (expr, span)
        }
    )(input)
}

fn func_arg<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<FuncArgument>> {
    map(
        spanned(then(
            then(
                parse_type,
                opt(ident_local)
            ),
            opt(
                then(
                    just(Token::Op("=")),
                    mathematical_expression
                )
            )
        )),
        |(((value_type, name), body), span)| {
            let name = match name {
                Some(name) => Box::new(name),
                None => Box::new((Expr::Value { val: Value::Null }, span)),
            };
            let body = match body {
                Some((_, body)) => Box::new(body),
                None => Box::new((Expr::Value { val: Value::Null }, span)),
            };
            let expr = Expr::Definition { value_type, name, body };

            (FuncArgument::Parameter(Box::new((expr, span))), span)
        }
    )(input)
}

fn function_definition<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        spanned(then(
            then(
                many0(terminated( // TODO: maybe also parse parameter packs here, with the sole purpose of giving better error messages
                    func_arg,
                    just(Token::Separator(','))
                )),
                choice((
                    map(
                        spanned(then(
                            value_type_auto, // TODO: Maybe don't ignore this
                            then(
                                just(Token::Separator('.')),
                                then(
                                    just(Token::Separator('.')),
                                    then(
                                        just(Token::Separator('.')),
                                        ident
                                    )
                                )
                            )
                        )),
                        |((_, (_, (_, (_, name)))), span)| {
                            (FuncArgument::ParameterPack(name), span)
                        }
                    ),
                    func_arg
                ))
            ),
            then(
                just(Token::Separator(')')),
                then(
                    just(Token::Separator('{')),
                    terminated(
                        many0(function_statement),
                        just(Token::Separator('}'))
                    )
                )
            )
        )),
        |(a, span)| (a, span) // TODO
    )(input);
    todo!()
}

fn function_variable_decl<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        parse_type,
        then(
            ident,
            choice((
                then(
                    just(Token::Separator('[')),
                    then(
                        not(peek(just(Token::Separator(']')))),
                        member_array_variable
                    )
                ),
                then(
                    member_variable,
                    opt(then(
                        just(Token::Op("=")),
                        mathematical_expression
                    ))
                )
            ))
        )
    )(input)
}

fn function_statement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        choice((
            map(
                spanned(then(
                    ident_local,
                    then(
                        just(Token::Op("=")),
                        mathematical_expression
                    )
                )),
                |((loperand, (_, roperand)), span)| {
                    let expr = Expr::Binary {
                        loperand: Box::new(loperand),
                        operation: BinaryOp::Assign(Assignment::Just),
                        roperand: Box::new(roperand),
                    };

                    (expr, span)
                }
            ),
            map(
                spanned(then(
                    just(Token::Op("$")),
                    then(
                        just(Token::Op("=")),
                        mathematical_expression
                    )
                )),
                |(a, span)| a // TODO
            ),
            map(
                spanned(then(
                    ident,
                    then(
                        choice((
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
                        )),
                        then(
                            just(Token::Op("=")),
                            mathematical_expression
                        )
                    )
                )),
                |(a, span)| a // TODO
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
                        mathematical_expression
                    )
                )
            ),
            then(
                choice((
                    just(Token::K(Keyword::Return)),
                    just(Token::K(Keyword::Break)),
                    just(Token::K(Keyword::Continue)),
                )),
                function_controlflow_statement
            ),
            then(
                just(Token::K(Keyword::If)),
                then(
                    just(Token::Separator('(')),
                    function_conditional
                )
            ),
            then(
                just(Token::K(Keyword::While)),
                then(
                    just(Token::Separator('(')),
                    function_while_loop
                )
            ),
            then(
                just(Token::K(Keyword::For)),
                then(
                    just(Token::Separator('(')),
                    function_for_loop
                )
            ),
            then(
                ident,
                then(
                    choice((
                        peek(just(Token::Separator('.'))),
                        peek(just(Token::Separator('['))),
                    )),
                    then(
                        old_member_access,
                        then(
                            just(Token::Op('=')),
                            mathematical_expression
                        )
                    )
                )
            ),
            then(
                ident,
                then(
                    old_namespace_resolution,
                    choice((
                        then(
                            peek(just(Token::Separator('('))),
                            function_call
                        ),
                        function_variable_decl
                    ))
                )
            ),
            then(
                peek(choice((
                    just(Token::K(Keyword::BigEndian)),
                    just(Token::K(Keyword::LittleEndian)),
                    value_type_any
                ))),
                function_variable_decl
            )
        )),
        just(Token::Separator(';')) // TODO: Remove extra ";"
    )
}

fn function_controlflow_statement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        peek(just(Token::Separator(';'))),
        mathematical_expression
    ))(input)
}

fn statement_body<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        then(
            just(Token::Separator('{')),
            terminated(
                many0(function_statement),
                just(Token::Separator('}'))
            )
        ),
        function_statement
    ))(input)
}

fn function_conditional<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        mathematical_expression,
        then(
            just(Token::Separator(')')),
            then(
                statement_body,
                opt(then(
                    just(Token::K(Keyword::Else)),
                    statement_body
                ))
            )
        )
    )(input)
}

fn function_while_loop<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        mathematical_expression,
        then(
            just(Token::Separator(')')),
            statement_body
        )
    )(input)
}

fn function_for_loop<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        function_statement,
        then(
            just(Token::Separator(',')),
            then(
                mathematical_expression,
                then(
                    just(Token::Separator(',')),
                    then(
                        function_statement,
                        then(
                            just(Token::Separator(')')),
                            statement_body
                        )
                    )
                )
            )
        )
    )(input)
}

fn conditional<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        mathematical_expression,
        then(
            choice((
                then(
                    just(Token::Separator(')')),
                    then(
                        just(Token::Separator('{')),
                        terminated(
                            many0(member),
                            just(Token::Separator('}'))
                        )
                    )
                ),
                then(
                    just(Token::Separator(')')),
                    member
                )
            )),
            opt(choice((
                then(
                    just(Token::K(Keyword::Else)),
                    then(
                        just(Token::Separator('{')),
                        terminated(
                            many0(member),
                            just(Token::Separator('}'))
                        )
                    )
                )
            )))
        )
    )
}

fn while_statement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        mathematical_expression,
        just(Token::Separator(')'))
    )
}

fn parse_type<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<String>> {
    todo!()
}

fn using_declaration<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    parse_type(input)
}

fn padding<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        choice((
            then(
                just(Token::K(Keyword::While)),
                then(
                    just(Token::Separator('(')),
                    while_statement
                )
            ),
            mathematical_expression
        )),
        just(Token::Separator(']'))
    )(input)
}

fn member_variable<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        then(
            peek(Token::Separator(',')),
            many1(then(
                just(Token::Separator(',')),
                ident
            ))
        ),
        just(Token::Op('@'))
    ))(input)
}

fn member_array_variable<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        opt(then(
            not(just(Token::Separator(']'))),
            then(
                choice((
                    then(
                        just(Token::K(Keyword::While)),
                        then(
                            just(Token::Separator('(')),
                            while_statement
                        )
                    ),
                    mathematical_expression
                )),
                just(Token::Separator(']'))
            )
        )),
        then(
            just(Token::Op("@")),
            mathematical_expression
        )
    )(input)
}

fn pointer_size_type<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
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
                            while_statement
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
                    just(Token::Op('@')),
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
                just(Token::Op('$')),
                then(
                    just(Token::Op('=')),
                    function_variable_assignment
                )
            ),
            then(
                just(Token::Op('$')),
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
                        just(Token::Op('=')),
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
                    just(Token::K(Keyword::BigEndian)),
                    just(Token::K(Keyword::LittleEndian)),
                    value_type_any,
                    ident
                ))),
                then(
                    opt(peek(then(
                        ident,
                        then(
                            old_namespace_resolution,
                            opt(then(
                                peek(just(Token::Separator(')'))),
                                function_call
                            ))
                        )
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
    then(
        opt(then(
            just(Token::Op(':')),
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
    )(input)
}

fn parse_union<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    terminated(
        many0(member),
        just(Token::Separator('}'))
    )(input)
}

fn parse_enum<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        parse_type,
        then(
            just(Token::Separator('{')),
            terminated(
                many0(then(
                    choice((
                        then(
                            ident,
                            then(
                                just(Token::Op("=")),
                                mathematical_expression
                            )
                        ),
                        ident
                    )),
                    just(Token::Separator(',')) // TODO: , not needed in the last entry
                )),
                just(Token::Separator('}'))
            )
        )
    )
}

fn bitfield_entry<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        choice((
            then(
                ident,
                then(
                    just(Token::Op(":")),
                    mathematical_expression
                )
            ),
            then(
                just(Token::V(ValueType::Padding)),
                then(
                    just(Token::Op(":")),
                    mathematical_expression
                )
            ),
            then(
                just(Token::K(Keyword::If)),
                then(
                    just(Token::Separator('(')),
                    then(
                        mathematical_expression,
                        then(
                            choice((
                                then(
                                    just(Token::Separator(')')),
                                    then(
                                        just(Token::Separator('{')),
                                        terminated(
                                            many0(bitfield_entry),
                                            just(Token::Separator('}'))
                                        )
                                    )
                                ),
                                then(
                                    just(Token::Separator(')')),
                                    bitfield_entry
                                )
                            )),
                            choice((
                                then(
                                    just(Token::K(Keyword::Else)),
                                    terminated(
                                        many0(bitfield_entry),
                                        just(Token::Separator('}'))
                                    )
                                ),
                                then(
                                    just(Token::K(Keyword::Else)),
                                    bitfield_entry
                                )
                            ))
                        )
                    )
                )
            )
        )),
        just(Token::Separator(';'))
    )(input)
}

fn parse_bitfield<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    terminated(
        many0(then(
            bitfield_entry,
            many0(just(Token::Separator(';')))
        )),
        just(Token::Separator('}'))
    )
}

fn forward_declaration<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    todo!()
}

fn variable_placement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map(
            then(
                just(Token::Op("@")),
                mathematical_expression
            ),
            |(a, b)| b // TODO
        ),
        just(Token::K(Keyword::In)),
        just(Token::K(Keyword::Out)),
    ))(input)
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
                                while_statement
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
                                while_statement
                            ),
                            |(a, b)| b // TODO
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
            ident,
            then( // TODO: This should be inside a many0
                just(Token::Op("::")),
                ident
            )
        ),
        |(a, b)| a // TODO
    )(input)
}

fn placement<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map(
            then(
                then(
                    ident,
                    just(Token::Separator('['))
                ),
                array_variable_placement
            ),
            |(a, b)| b // TODO
        ),
        map(
            then(
                ident,
                variable_placement
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
        map(
            then(
                just(Token::K(Keyword::Using)),
                then(
                    ident,
                    then(
                        just(Token::Op("="))
                        using_declaration
                    )
                )
            ),
            |((a, (b,c)), d)| b // TODO
        ),
        map(
            then(
                just(Token::K(Keyword::Using)),
                then(
                    ident,
                    forward_declaration
                )
            ),
            |((a, b), c)| b //TODO
        ),
        map(
            then(
                peek(choice((
                        just(Token::K(Keyword::BigEndian)),
                        just(Token::K(Keyword::LittleEndian)),
                        value_type_any,
                ))),
                placement
            ),
            |(a, b)| b // TODO
        ),
        map(
            then(
                peek(ident),
                then(
                    not(just(Token::Op("="))),
                    then(
                        not(just(Token::Separator('.'))),
                        then(
                            not(just(Token::Separator('['))),
                            then(
                                old_namespace_resolution,
                                choice((
                                    map(
                                        then(
                                            peek(just(Token::Separator('('))),
                                            function_call
                                        ),
                                        |(a, b)| b // TODO
                                    ),
                                    placement
                                ))
                            )
                        )
                    )
                )
            ),
            |(((((a, _), _), _), b), c)| a // TODO
        ),
        map(
            then(
                just(Token::K(Keyword::Struct)),
                then(
                    ident,
                    parse_struct
                )
            ),
            |((a, b), c)| b // TODO
        ),
        map(
            then(
                just(Token::K(Keyword::Union)),
                then(
                    just(Token::Separator('{')),
                    then(
                        ident,
                        parse_union
                    )
                )
            ),
            |(((a, b), c), d)| c // TODO
        ),
        map(
            then(
                just(Token::K(Keyword::Enum)),
                then(
                    just(Token::Separator(':')),
                    then(
                        ident,
                        parse_enum
                    )
                )
            ),
            |(((a, b), c), d)| c // TODO
        ),
        map(
            then(
                just(Token::K(Keyword::Bitfield)),
                then(
                    just(Token::Separator('{')),
                    then(
                        ident,
                        parse_bitfield
                    )
                )
            ),
            |(((a, b), c), d)| c // TODO
        ),
        map(
            then(
                just(Token::K(Keyword::Fn)),
                then(
                    just(Token::Separator('(')),
                    then(
                        ident,
                        function_definition
                    )
                )
            ),
            |(((a, b), c), d)| c // TODO
        ),
        map(
            then(
                just(Token::K(Keyword::Namespace)),
                then(
                    ident,
                    parse_namespace,
                )
            ),
            |((a, b), c)| b // TODO
        ),
        function_statement
    ))(input)
}

fn statements<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            statements_choice,
            opt(
                then(
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

fn value_type_any<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
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
