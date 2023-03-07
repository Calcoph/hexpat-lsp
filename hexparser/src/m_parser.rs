use std::{collections::HashMap, ops::Range, io::ErrorKind};

use nom::{
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
    sequence::{pair as then, terminated, delimited, preceded, separated_pair, tuple},
    multi::{
        many_till as many_until,
        many1,
        many0,
        separated_list0,
        separated_list1
    }, InputTake, Parser
};
use nom_supreme::{error::{ErrorTree, BaseErrorKind, GenericErrorTree}, ParserExt};
use serde::{Deserialize, Serialize};

use crate::{token::{Spanned, TokSpan, Tokens, Token, Keyword, ValueType}, combinators::{spanned, ignore, to, map_with_span, fold_many0_once}, m_parser::{function::{function_statement, function_definition}, operations::mathematical_expression}, recovery_err::{TokResult, ToRange, RecoveredError, TokError, expression_recovery, non_opt}, simple_debug::SimpleDebug};

pub use operations::UnaryOp;

mod function;
mod factor;
pub(crate) mod operations;

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
    If {
        test: Box<Spanned<Self>>,
        consequent: Box<Spanned<Self>>,
    },
    IfBlock {
        ifs: Box<Spanned<Self>>,
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
        args: Spanned<Vec<Spanned<FuncArgument>>>,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

// exaclty the same as above, but used when the rework has been done
fn function_call<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map(
        then(
            namespace_resolution,
            delimited(
                just(Token::Separator('(')),
                separated_list0(
                    just(Token::Separator(',')),
                    mathematical_expression
                ),
                just(Token::Separator(')')).context("Missing )"),
            )
        ),
        |(func_name, arguments)| {
            let args_span = if arguments.len() > 0 {
                arguments.get(0).unwrap().1.start..arguments.get(arguments.len()-1).unwrap().1.end
            } else {
                func_name.1.clone()
            };

            let span = func_name.1.start..args_span.end;
            let arguments = (arguments, args_span);

            (
                Expr::Call {
                    func_name: Box::new(func_name),
                    arguments
                },
                span
            )
        }
    ))(input)
}

fn string_literal<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_res(
        spanned(take(1 as usize)),
        |(consumed, span): (Tokens, Range<usize>)|{
            match consumed.tokens[0].fragment() {
                Token::Str(s) => Ok((Expr::Value { val: Value::Str(String::from(*s)) }, span)),
                Token::Char(c) => Ok((Expr::Value { val: Value::Char(*c) }, span)),
                _ => Err(ErrorTree::Base {
                    location: consumed,
                    kind: BaseErrorKind::External(Box::new(tokio::io::Error::new(ErrorKind::Other, "Expected string literal")))
                }) // TODO: Expand match tree for "found X"
            }
        }
    )(input)
}

pub(crate) fn namespace_resolution<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    let (input, first_name) = ident_local(input)?;
    fold_many0_once(
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
fn member_access<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_with_span(
        tuple((
            choice((
                ident_local,
                map_with_span(
                    just(Token::K(Keyword::Parent)),
                    |_, span| (Expr::Local { name: (String::from("parent"), span.clone())} , span)
                ),
                map_with_span(
                    just(Token::K(Keyword::This)),
                    |_, span| (Expr::Local { name: (String::from("this"), span.clone())} , span)
                )
            )),
            opt(delimited(
                then(
                    just(Token::Separator('[')),
                    not(just(Token::Separator('[')))
                ),
                mathematical_expression,
                just(Token::Separator(']'))
            )),
            opt(preceded(
                just(Token::Separator('.')),
                preceded(
                    peek(choice((
                        ignore(ident),
                        ignore(just(Token::K(Keyword::Parent)))
                    ))),
                    member_access
                )
            ))
        )),
        |(item, array, member), span| {
            match member {
                Some(member) => (
                    Expr::Access {
                        item: Box::new(item),
                        member: Box::new(member)
                    },
                    span
                ),
                None => item
            }
        }
    )(input)
}

fn attribute_arg<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
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
                None => (Box::new((Expr::Value { val: Value::Null }, name_span.clone())), name_span.clone()),
            };
            let expr = Expr::AttributeArgument {
                name: (name, name_span),
                value
            };

            (expr, span)
        }
    )(input)
}

fn attribute<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_with_span(
        separated_list1(
            just(Token::Separator(',')),
            attribute_arg
        ),
        |arguments, span| {
            let arg_span = arguments.get(0).unwrap().1.start..arguments.get(arguments.len()-1).unwrap().1.end;
            let expr = Expr::Attribute {
                arguments: (
                    arguments,
                    arg_span
                )
            };
            (expr, span)
        }
    )(input)
}

fn statement_body<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
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

fn conditional<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        then(
            map_with_span(separated_list1(
                just(Token::K(Keyword::Else)),
                map_with_span(preceded(
                    just(Token::K(Keyword::If)),
                    tuple((
                        delimited(
                            just(Token::Separator('(')).context("Missing ("),
                            mathematical_expression,
                            just(Token::Separator(')')).context("Missing )")
                        ),
                        choice((
                            map(
                                delimited(
                                    just(Token::Separator('{')),
                                    spanned(many0(member)),
                                    just(Token::Separator('}')).context("Missing }")
                                ),
                                |(list, span)| (Expr::ExprList { list }, span)
                            ),
                            member.context("Invalid expression"),
                        ))
                    ))
                ), |a, span| (a, span))
            ), |a, span| (a, span)),
            opt(preceded(
                just(Token::K(Keyword::Else)),
                choice((
                    map(
                        delimited(
                            just(Token::Separator('{')),
                            spanned(many0(member)),
                            non_opt(just(Token::Separator('}'))).context("Missing }")
                        ),
                        |(list, span)| (Expr::ExprList { list }, span)
                    ),
                    member.context("Invalid expression")
                ))
            ))
        ),
        |(ifs, alternative), span| {
            let mut v = vec![];
            for ((test, consequent), span) in ifs.0 {
                v.push((Expr::If {
                    test: Box::new(test),
                    consequent: Box::new(consequent),
                }, span))
            }
            let alternative = Box::new(match alternative {
                Some(alt) => alt,
                None => (Expr::Value { val: Value::Null }, span.clone()),
            });
            (
                Expr::IfBlock {
                    ifs: Box::new((Expr::ExprList { list: v }, ifs.1)),
                    alternative
                },
                span
            )
        }
    ))(input)
}

#[derive(Debug, Clone)]
pub struct HexTypeDef {
    pub endianness: Endianness,
    pub name: Spanned<HexType>
}

#[derive(Debug, Clone)]
pub enum HexType {
    Custom(String),
    Path(Vec<String>),
    V(ValueType),
    Null
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
            recursive_namespace_access_to_hextype(previous.0, v);
            v.push(name);
        },
        Expr::Error => (),
        _ => unreachable!()
    }
}

fn parse_type<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<HexTypeDef>> {
    map_with_span(
        tuple((
            opt(just(Token::K(Keyword::Reference))),
            opt(choice((
                to(just(Token::K(Keyword::LittleEndian)), Endianness::Little),
                to(just(Token::K(Keyword::BigEndian)), Endianness::Big)
            ))),
            choice((
                map(
                    then(
                        |input: Tokens<'a, 'b>| match namespace_resolution.parse(input) {
                            Ok((input, (n_access, span))) => match n_access {
                                Expr::Local { name: (name, _) } => Ok((input, (HexType::Custom(name), span))),
                                Expr::NamespaceAccess { previous, name } => Ok((input, (namespace_access_to_hextype(Expr::NamespaceAccess { previous, name }), span))),
                                Expr::Error => Ok((input, (HexType::Null, span))),
                                _ => unreachable!()
                            },
                            Err(e) => Err(e),
                        },
                        opt(delimited(
                            just(Token::Op("<")),
                            separated_list1(
                                just(Token::Separator(',')),
                                choice((
                                    ignore(parse_type), // TODO: don't ignore
                                    ignore(mathematical_expression) // TODO: don't ignore
                                ))
                            ),
                            just(Token::Op(">"))
                        ))
                    ),
                    |(a, b)| a // TODO: don't ignore B
                ),
                value_type_any
            ))
        )),
        |(reference, endianness, name), span| {//|(reference, endianness, name), span| {
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

fn using_declaration<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<HexTypeDef>> {
    parse_type(input)
}

fn padding<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
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
                    body: Box::new((Expr::Value { val: Value::Null }, span.clone()))
                },
                span
            )
        ),
        mathematical_expression
    ))(input)
}

fn pointer_size_type<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<HexTypeDef>> {
    parse_type(input)
}

fn assignment_expr<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        tuple((
            choice((
                ident_local,
                map_with_span(
                    just(Token::Op("$")),
                    |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
                )
            )),
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
                mathematical_expression.context("Expected mathematical expression")
            )
        )),
        |(loperand, assignment, roperand), span| {
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
    ))(input)
}

fn array_declaration<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Option<Spanned<Expr>>> { // TODO: make an "array declaration" expression, so the "[","]" is also spanned and this returns Spanned<Expr> instead of Option<Spanned<Expr>>
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
                        just(Token::Separator('(')).context("Expected ("),
                        mathematical_expression.context("Expected mathematical expression"),
                        just(Token::Separator(')')).context("Missing )")
                    )
                ),
                |a, span| (
                    Expr::WhileLoop {
                        condition: Box::new(a),
                        body: Box::new((Expr::Value { val: Value::Null }, span.clone()))
                    },
                    span
                )
            ),
            mathematical_expression
        ))),
        just(Token::Separator(']')),
    )(input)
}

fn member_variable<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    choice((
        map_with_span(
            tuple((
                parse_type,
                namespace_resolution,
                array_declaration,
                opt(preceded(
                    just(Token::Op("@")),
                    non_opt(mathematical_expression).context("Expected mathematical expression")
                ))
            )),
            |(value_type, name, array, body), span| {
                let body = Box::new(match body {
                    Some(body) => body,
                    None => (Expr::Value { val: Value::Null }, span.clone()),
                });

                (
                    Expr::Definition {
                        value_type,
                        name: Box::new(name),
                        body
                    },
                    span
                )
            }
        ),
        map_with_span(
            choice((
                tuple((
                    parse_type,
                    namespace_resolution,
                    preceded(
                        just(Token::Op("@")),
                        mathematical_expression.context("Expected mathematical expression")
                    )
                )),
                map_with_span(
                    tuple((
                        parse_type,
                        namespace_resolution,
                        opt(preceded(
                            just(Token::Separator(',')),
                            separated_list1(
                                just(Token::Separator(',')),
                                ident_local
                            )
                        ))
                    )),
                    |(value_type, name, names), span| {
                        let names = match names {
                            Some(mut names) => {
                                let span = name.1.start..names.get(names.len()-1).unwrap().1.end;
                                names.insert(0, name);
                                (Expr::ExprList { list: names }, span)
                            },
                            None => name
                        };
                        (
                            value_type,
                            names,
                            (Expr::Value { val: Value::Null }, span)
                        )
                    }
                )
            )),
            |(value_type, name, body), span| (
                Expr::Definition {
                    value_type,
                    name: Box::new(name),
                    body: Box::new(body)
                },
                span
            )
        ),
        map_with_span(
            separated_pair(
                parse_type,
                just(Token::Op("*")),
                then(
                    separated_pair(
                        ident_local,
                        just(Token::Op(":")),
                        pointer_size_type
                    ),
                    opt(preceded(
                        just(Token::Op("@")),
                        non_opt(mathematical_expression).context("Expected mathematical expression")
                    ))
                )
            ),
            |(value_type,
                ((name, pointer_type),
                    body
                )
            ), span| {
                let body = Box::new(match body {
                    Some(body) => body,
                    None => (Expr::Value { val: Value::Null }, span.clone()),
                });

                (
                    Expr::Definition {
                        value_type,
                        name: Box::new(name),
                        body: body
                    },
                    span
                )
            }
        ),
        map_with_span(
            separated_pair(
                parse_type,
                just(Token::Op("*")),
                tuple((
                    ident_local,
                    array_declaration,
                    preceded(
                        just(Token::Op(":")),
                        then(
                            pointer_size_type,
                            opt(preceded(
                                just(Token::Op("@")),
                                non_opt(mathematical_expression).context("Expected mathematical expression")
                            ))
                        )
                    )
                ))
            ),
            |(value_type,
                (name, array,
                    (pointer_type, body)
                )
            ), span| {
                let body = Box::new(match body {
                    Some(body) => body,
                    None => (Expr::Value { val: Value::Null }, span.clone()),
                });

                (
                    Expr::Definition {
                        value_type,
                        name: Box::new(name),
                        body
                    },
                    span
                )
            }
        )
    ))(input)
}

fn member_declaration<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    preceded(
        peek(choice((
            ignore(just(Token::K(Keyword::BigEndian))),
            ignore(just(Token::K(Keyword::LittleEndian))),
            ignore(value_type_any),
            ignore(ident)
        ))),
        choice((
            function_call,
            member_variable
        ))
    )(input)
}

pub(crate) fn member<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(choice((
        conditional,
        terminated(
            choice((
                assignment_expr,
                member_declaration,
                preceded(
                    just(Token::V(ValueType::Padding)),
                    delimited(
                        just(Token::Separator('[')).context("Missing ["),
                        padding.context("Expected padding expression"),
                        just(Token::Separator(']')).context("Missing ]")
                    )
                ),
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
                    non_opt(then(
                        just(Token::Separator(']')).context("Missing ]]"),
                        just(Token::Separator(']')).context("Missing ]")
                    ))
                )),
                many1(just(Token::Separator(';'))).context("Missing ;")
            )
        )
    )))(input)
}

pub(crate) fn template_list<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_with_span(
        delimited(
            just(Token::Op("<")),
            separated_list1(
                just(Token::Separator(',')),
                then(
                    opt(just(Token::V(ValueType::Auto))),
                    ident_local
                )
            ),
            just(Token::Op(">"))
        ),
        |list, span| {
            let list = list.into_iter().map(|(_, a)| a).collect(); // ignore "auto" for now // TODO: don't ignore it
            (Expr::ExprList { list }, span)
        }
    )(input)
}

pub(crate) fn parse_struct<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Struct)),
            tuple((
                ident.context("Expected struct name"),
                opt(template_list),
                opt(preceded(
                    just(Token::Op(":")),
                    non_opt(separated_list1(
                        just(Token::Separator(',')),
                        ident
                    )).context("Expected parent type")
                )),
                delimited(
                    just(Token::Separator('{')).context("Missing {"),
                    spanned(many0(member)),
                    just(Token::Separator('}')).context("Expected } or valid struct expression")
                )
            ))
        ),
        |(name, template, inheritance, (body, body_span)), span| ( // TODO: Take into account the inheritance. //TODO: Take into account template
            Expr::Struct {
                name,
                body: Box::new((Expr::ExprList { list: body }, body_span))
            },
            span
        )
    ))(input)
}

fn parse_union<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Union)),
            tuple((
                ident.context("Expected identifier"),
                opt(template_list),
                delimited(
                    just(Token::Separator('{')).context("Missing {"),
                    spanned(many0(member)),
                    just(Token::Separator('}')).context("Expected } or valid union expression")
                )
            ))
        ),
        |(name, template, (body, body_span)), span| ( // TODO: don't ignore template
            Expr::Union {
                name,
                body: Box::new((Expr::ExprList { list: body }, body_span))
            },
            span
        )
    ))(input)
}

fn parse_enum<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Enum)),
            then(
                separated_pair(
                    ident.context("Expected enum name"),
                    just(Token::Op(":")).context("Missing :"),
                    parse_type.context("Expected enum type")
                ),
                delimited(
                    just(Token::Separator('{')).context("Missing {"),
                    spanned(separated_list0(
                        just(Token::Separator(',')),
                        choice((
                            then(
                                ident,
                                preceded(
                                    just(Token::Op("=")),
                                    terminated( // TODO: Don't ignore range
                                        mathematical_expression,
                                        opt(preceded(
                                            tuple((
                                                just(Token::Separator('.')),
                                                just(Token::Separator('.')),
                                                just(Token::Separator('.'))
                                            )),
                                            non_opt(mathematical_expression)
                                        ))
                                    )
                                )
                            ),
                            map(ident, |(a, a_span)| ((a, a_span.clone()), (Expr::Value { val: Value::Null }, a_span)))
                        )),
                    )),
                    just(Token::Separator('}')).context("Expected } or valid enum expression")
                )
            )
        ),
        |((name, value_type), (entries, entries_span)), span| {
            let entries = entries.into_iter()
                .map(|((name, name_span), value)| (Expr::EnumEntry { name: (name, name_span.clone()), value: Box::new(value) }, name_span)).collect();
            (
                Expr::Enum {
                    name,
                    value_type,
                    body: Box::new((Expr::ExprList { list: entries }, entries_span))
                },
                span
            )
        }
    ))(input)
}

fn bitfield_conditional<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_with_span(
        then(
            map_with_span(separated_list1(
                just(Token::K(Keyword::Else)),
                map_with_span(preceded(
                    just(Token::K(Keyword::If)),
                    tuple((
                        delimited(
                            just(Token::Separator('(')),
                            mathematical_expression,
                            just(Token::Separator(')'))
                        ),
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
                ), |a, span| (a, span))
            ), |a, span| (a, span)),
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
        ),
        |(ifs, alternative), span| {
            let mut v = vec![];
            for ((test, consequent), span) in ifs.0 {
                v.push((Expr::If {
                    test: Box::new(test),
                    consequent: Box::new(consequent),
                }, span))
            }
            let alternative = Box::new(match alternative {
                Some(alt) => alt,
                None => (Expr::Value { val: Value::Null }, span.clone()),
            });
            (
                Expr::IfBlock {
                    ifs: Box::new((Expr::ExprList { list: v }, ifs.1)),
                    alternative
                },
                span
            )
        }
    )(input)
}

fn bitfield_entry<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
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
    ))(input)
}

fn parse_bitfield<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Bitfield)),
            then(
                ident.context("Expected bitfield name"),
                delimited(
                    just(Token::Separator('{')).context("Missing {"),
                    terminated(
                        separated_list0(
                            many1(just(Token::Separator(';'))),
                            bitfield_entry
                        ),
                        many0(just(Token::Separator(';')))
                    ),
                    just(Token::Separator('}')).context("Expected } or valid bitfield expression")
                )
            )
        ),
        |(name, body), span| {
            let body = Box::new({
                let span = match body.len() {
                    0 => span.clone(),
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
    ))(input)
}

fn array_variable_placement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, (Spanned<Expr>, Spanned<Expr>)> {
    map(
        tuple((
            ident_local,
            array_declaration,
            opt(
                preceded(
                    just(Token::Op("@")),
                    then(
                        non_opt(mathematical_expression).context("Expected mathematical expression"),
                        opt(preceded(
                            just(Token::K(Keyword::In)),
                            non_opt(mathematical_expression).context("Expected mathematical expression")
                        ))
                    )
                )
            )
        )),
        |((name, name_span), array, body)| { // TODO: Take array into account
            let array = Box::new(match array {
                Some(s) => s,
                None => (Expr::Value { val: Value::Null }, name_span.clone())
            });
            let body = match body {
                Some((b, section)) => b,
                None => (Expr::Value { val: Value::Null }, name_span.clone()),
            };
            ((name, name_span), body)
        }
    )(input)
}

fn pointer_array_variable_placement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map(
        tuple((
            opt(
                then(
                    not(just(Token::Separator(']'))),
                    choice((
                        delimited(
                            then(
                                just(Token::K(Keyword::While)),
                                just(Token::Separator('('))
                            ),
                            mathematical_expression,
                            just(Token::Separator(')'))
                        ),
                        mathematical_expression
                    ))
                )
            ),
            just(Token::Separator(']')),
            just(Token::Op(":")),
            pointer_size_type,
            just(Token::Op("@")),
            mathematical_expression,
            opt(preceded(
                just(Token::K(Keyword::In)),
                non_opt(mathematical_expression).context("Expected mathematical expression")
            ))
        )),
        |(a, b, c, d, e, f, section)| f // TODO
    )(input)
}

pub(crate) fn parse_namespace<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Namespace)),
            then(
                separated_list1(
                    just(Token::Op("::")),
                    ident
                ),
                delimited(
                    just(Token::Separator('{')).context("Missing {"),
                    spanned(many0(statements)),
                    just(Token::Separator('}')).context("Expected } or valid expression")
                )
            )
        ),
        |(name, (body, body_span)), span| {
            let name = Box::new(name.into_iter()
                .fold((Expr::Value { val: Value::Null }, 0..1), |(accum, acc_span), (next, next_span)| {
                    match accum {
                        Expr::Value { .. } => (Expr::Local { name: (next, next_span.clone()) }, next_span), // first case
                        _ => (Expr::NamespaceAccess { previous: Box::new((accum, acc_span.clone())), name: (next, next_span.clone()) }, acc_span.start..next_span.end)
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
    ))(input)
}

fn placement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_with_span(
        then(
            parse_type,
            choice((
                array_variable_placement,
                map(
                    then(
                        ident_local,
                        opt(choice((
                            map(
                                preceded(
                                    just(Token::Op("@")),
                                    then(
                                        non_opt(mathematical_expression).context("Expected mathematical expression"),
                                        opt(preceded(
                                            just(Token::K(Keyword::In)),
                                            non_opt(mathematical_expression).context("Expected mathematical expression")
                                        ))
                                    )
                                ),
                                |(a, section)| Some(a)
                            ),
                            map(just(Token::K(Keyword::In)), |_| None),
                            map(just(Token::K(Keyword::Out)), |_| None),
                            map(
                                preceded(
                                    just(Token::Op("=")),
                                    non_opt(mathematical_expression).context("Expected mathematical expression")
                                ),
                                |a| Some(a)
                            )
                        )))
                    ),
                    |((name, fake_span), body)| {
                        let body = match body {
                            Some(body) => match body {
                                Some(body) => body,
                                None => (Expr::Value { val: Value::Null }, fake_span.clone()),
                            },
                            None => (Expr::Value { val: Value::Null }, fake_span.clone()),
                        };
        
                        ((name, fake_span), body)
                    }
                ),
                map(
                    preceded(
                        just(Token::Op("*")),
                        separated_pair(
                            ident_local,
                            just(Token::Op(":")),
                            separated_pair(
                                pointer_size_type,
                                just(Token::Op("@")),
                                then(
                                    mathematical_expression,
                                    opt(preceded(
                                        just(Token::K(Keyword::In)),
                                        non_opt(mathematical_expression).context("Expected mathematical expression")
                                    ))
                                )
                            )
                        )
                    ),
                    |(name, (pointer_type, (body, section)))| (name, body) // TODO take the type of the pointer into account
                ),
                map(
                    preceded(
                        just(Token::Op("*")),
                        then(
                            ident_local,
                            preceded( // TODO: This is probably wrong
                                just(Token::Separator('[')),
                                pointer_array_variable_placement
                            )
                        )
                    ),
                    |(name, body)| (name, body)
                )
            ))
        ),
        |(value_type, (name, body)), span| (
            Expr::Definition {
                value_type,
                name: Box::new(name),
                body: Box::new(body)
            },
            span
        )
    )(input)
}

fn using<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Using)),
            tuple((
                ident.context("Expected identifier after \"using\""),
                opt(template_list),
                opt(preceded(
                    just(Token::Op("=")),
                    non_opt(using_declaration).context("Expected type")
                ))
            ))
        ),
        |(new_name, template, old_name), span| { // TODO: Don't ignore template
            let old_name = match old_name {
                Some(old_name) => old_name,
                None => (HexTypeDef{ endianness: Endianness::Unkown, name: (HexType::Null, span.clone()) }, span.clone()),
            };
            (
                Expr::Using {
                    new_name,
                    old_name
                },
                span
            )
        }
    ))(input)
}

fn normal_placement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(preceded(
        peek(choice((
                ignore(just(Token::K(Keyword::BigEndian))),
                ignore(just(Token::K(Keyword::LittleEndian))),
                ignore(value_type_any),
        ))),
        placement.context("Expected variable declaration")
    ))(input)
}

fn todo_placement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(preceded(
        peek(tuple((
            ident,
            not(just(Token::Op("="))),
            not(just(Token::Separator('.'))),
            not(just(Token::Separator('[')))
        ))),
        choice((
            function_call,
            placement
        )).context("Expected placement")
    ))(input)
}

fn statements<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    let semicolon_expr = choice((
        using,
        normal_placement,
        todo_placement,
        parse_struct,
        parse_union,
        parse_enum,
        parse_bitfield,
        function_definition,
    ));

    let no_semicolon_expr = choice((
        parse_namespace,
        function_statement
    ));

    expression_recovery(choice((
        terminated(
            semicolon_expr,
            then(
                opt(delimited(
                    then(
                        just(Token::Separator('[')),
                        just(Token::Separator('['))
                    ),
                    attribute,
                    non_opt(then(
                        just(Token::Separator(']')).context("Missing ]]"),
                        just(Token::Separator(']')).context("Missing ]")
                    ))
                )),
                many1(just(Token::Separator(';'))).context("Missing ;")
            )
        ),
        terminated(
            no_semicolon_expr,
            then(
                opt(delimited(
                    then(
                        just(Token::Separator('[')),
                        just(Token::Separator('['))
                    ),
                    attribute,
                    non_opt(then(
                        just(Token::Separator(']')).context("Missing ]]"),
                        just(Token::Separator(']')).context("Missing ]")
                    ))
                )),
                many0(just(Token::Separator(';')))
            )
        )
    )))(input)
}

pub(crate) fn ident<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<String>> {
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

fn ident_local<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map(
        ident,
        |(name, span)| (Expr::Local { name: (name, span.clone()) }, span)
    )(input)
}

fn value_type_any<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<HexType>> {
    map_res(
        spanned(take(1 as usize)),
        |(consumed, span): (Tokens<'a, 'b>, Range<usize>)|{
            match consumed.tokens[0].fragment() {
                Token::V(type_) => Ok((HexType::V(*type_), span)), // TODO: Parse the number instead of always being 0.0
                _ => Err(ErrorTree::Base {
                    location: consumed,
                    kind: BaseErrorKind::External(Box::new(tokio::io::Error::new(ErrorKind::Other, "Expceted type")))
                }) // TODO: Expand match tree for "found X"
            }
        }
    )(input)
}

fn numeric<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_res(
        spanned(take(1 as usize)),
        |(consumed, span): (Tokens<'a, 'b>, Range<usize>)|{
            match consumed.tokens[0].fragment() {
                Token::Num(_) => Ok((Expr::Value { val: Value::Num(0.0) }, span)), // TODO: Parse the number instead of always being 0.0
                Token::Bool(b) => Ok((Expr::Value { val: Value::Bool(*b) }, span)),
                _ => Err(ErrorTree::Base {
                    location: consumed,
                    kind: BaseErrorKind::External(Box::new(tokio::io::Error::new(ErrorKind::Other, "Expceted number literal")))
                }) // TODO: Expand match tree for "found X"
            }
        }
    )(input)
}

fn parser<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_with_span(
        many_until(
            |input: Tokens<'a, 'b>| match statements(input) {
                Ok(r) => Ok(r),
                Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                    let input = recover_err(&e);
                    let (rest, input) = input.take_split(1);
                    let span = input.span();
                    let state = input.tokens[0].extra; 
                    state.0.report_error(RecoveredError(span.clone(), "Unexpected token".to_string()));
                    Ok((rest, (Expr::Error, span)))
                },
                Err(e) => Err(e)
            },
            eof
        ),
        |(list, _), span| (Expr::ExprList { list }, span)
    )(input)
}

fn recover_err<'a, 'b>(e: &TokError<'a, 'b>) -> Tokens<'a, 'b> {
    match e {
        GenericErrorTree::Base { location, kind: _ } => *location,
        GenericErrorTree::Stack { base, contexts: _ } => recover_err(base),
        GenericErrorTree::Alt(v) => recover_err(v.get(0).unwrap()),
    }
}

// Hashmap contains the names of named expressions and their clones
pub(crate) fn token_parse(tokens: Vec<TokSpan>) -> Spanned<Expr> {
    let ex = match tokens.len() {
        0 => (Expr::Value { val: Value::Null }, 0..0),
        _ => parser(Tokens::new(&tokens, tokens[0].extra.0)).expect("Unrecovered error happened in parser").1
    };
    //let ex = (Expr::Dollar, 0..1);
    ex
}
