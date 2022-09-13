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
    sequence::{pair as then, terminated, delimited, preceded, separated_pair},
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

use crate::{token::{Spanned, TokSpan, Tokens, Token, Keyword, ValueType}, combinators::{spanned, ignore, to, map_with_span, fold_many0_once}, m_parser::{function::{function_statement, function_definition}, operations::mathematical_expression}, recovery_err::{TokResult, ToRange, RecoveredError, TokError, expression_recovery, non_opt}};

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
    If { // TODO: See what is up with unkown
        test: Box<Spanned<Self>>,
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

// exaclty the same as above, but used when the rework has been done
fn function_call<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

fn string_literal<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    map_res(
        spanned(take(1 as usize)),
        |(consumed, span): (Tokens, Range<usize>)|{
            match consumed.tokens[0].fragment() {
                Token::Str(s) => Ok((Expr::Value { val: Value::Str(String::from(*s)) }, span)),
                _ => Err(ErrorTree::Base {
                    location: consumed,
                    kind: BaseErrorKind::External(Box::new(tokio::io::Error::new(ErrorKind::Other, "Expected string literal")))
                }) // TODO: Expand match tree for "found X"
            }
        }
    )(input)
}

// exaclty the same as above, but used when the rework has been done
pub(crate) fn namespace_resolution<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

// exaclty the same as above, but used when the rework has been done
// r_value
fn member_access<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    map_with_span(
        then(
            choice((
                ident_local,
                map_with_span(
                    just(Token::K(Keyword::Parent)),
                    |_, span| (Expr::Local { name: (String::from("parent"), span.clone())} , span)
                ),
                map_with_span(
                    just(Token::K(Keyword::This)),
                    |_, span| (Expr::Local { name: (String::from("this1"), span.clone())} , span)
                )
            )),
            then(
                opt(delimited(
                    just(Token::Separator('[')),
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
            )
        ),
        |(item, (array, member)), span| {
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

fn attribute_arg<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

fn attribute<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

fn statement_body<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

fn conditional<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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
                None => (Expr::Value { val: Value::Null }, span.clone()),
            });
            (
                Expr::If {
                    test: Box::new(test_),
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
pub(crate) enum Endianness {
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

fn parse_type<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<HexTypeDef>> {
    map_with_span(
        then(
            opt(choice((
                to(just(Token::K(Keyword::LittleEndian)), Endianness::Little),
                to(just(Token::K(Keyword::BigEndian)), Endianness::Big)
            ))),
            choice((
                |input: Tokens<'a>| match namespace_resolution.parse(input) {
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

fn using_declaration<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<HexTypeDef>> {
    parse_type(input)
}

fn padding<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

fn pointer_size_type<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<HexTypeDef>> {
    parse_type(input)
}

fn assignment_expr<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    expression_recovery(map_with_span(
        then(
            choice((
                ident_local,
                map_with_span(
                    just(Token::Op("$")),
                    |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
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
                    mathematical_expression.context("Expected mathematical expression")
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
    ))(input)
}

fn array_declaration<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Option<Spanned<Expr>>> { // TODO: make an "array declaration" expression, so the "[","]" is also spanned and this returns Spanned<Expr> instead of Option<Spanned<Expr>>
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

fn member_variable<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        map_with_span(
            then(
                then(
                    parse_type.context("TODO: remove this context type"),
                    namespace_resolution.context("TODO: remove this context name")
                ),
                then(
                    array_declaration,
                    opt(preceded(
                        just(Token::Op("@")),
                        mathematical_expression
                    ))
                )
            ),
            |((value_type, name), (array, body)), span| {
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
                then(
                    then(
                        parse_type,
                        namespace_resolution
                    ),
                    preceded(
                        just(Token::Op("@")),
                        mathematical_expression
                    )
                ),
                map_with_span(
                    then(
                        then(
                            parse_type,
                            namespace_resolution
                        ),
                        opt(preceded(
                            just(Token::Separator(',')),
                            separated_list1(
                                just(Token::Separator(',')),
                                ident_local
                            )
                        ))
                    ),
                    |((value_type, name), names), span| {
                        let names = match names {
                            Some(mut names) => {
                                let span = name.1.start..names.get(names.len()-1).unwrap().1.end;
                                names.insert(0, name);
                                (Expr::ExprList { list: names }, span)
                            },
                            None => name
                        };
                        (
                            (
                                value_type,
                                names
                            ),
                            (Expr::Value { val: Value::Null }, span)
                        )
                    }
                )
            )),
            |((value_type, name), body), span| (
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
                        mathematical_expression
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
                then(
                    ident_local,
                    then(
                        array_declaration,
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
            ),
            |(value_type,
                (name,
                    (array,
                        (pointer_type, body)
                    )
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

fn member_declaration<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

pub(crate) fn member<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    expression_recovery(choice((
        conditional,
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
                many1(just(Token::Separator(';'))).context("Missing ;")
            )
        )
    )))(input)
}

fn parse_struct<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Struct)),
            then(
                then(
                    ident.context("Expected struct name"),
                    opt(preceded(
                        just(Token::Op(":")),
                        non_opt(separated_list1(
                            just(Token::Separator(',')),
                            ident
                        )).context("Expected parent type")
                    )),
                ),
                delimited(
                    just(Token::Separator('{')).context("Missing {"),
                    spanned(many0(member)),
                    just(Token::Separator('}')).context("Expected } or valid struct expression")
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
    ))(input)
}

fn parse_union<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Union)),
            then(
                ident.context("Expected identifier"),
                delimited(
                    just(Token::Separator('{')).context("Missing {"),
                    spanned(many0(member)),
                    just(Token::Separator('}')).context("Expected } or valid union expression")
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
    ))(input)
}

fn parse_enum<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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
                    just(Token::Separator('}')).context("Expected } or valid enum expression")
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
    ))(input)
}

fn bitfield_conditional<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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
                None => (Expr::Value { val: Value::Null }, span.clone()),
            });
            (
                Expr::If {
                    test: Box::new(test_),
                    consequent: Box::new(consequent),
                    alternative
                },
                span
            )
        }
    )(input)
}

fn bitfield_entry<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

fn parse_bitfield<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

fn array_variable_placement<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, (Spanned<Expr>, Spanned<Expr>)> {
    map(
        then(
            ident_local,
            then(
                array_declaration,
                preceded(
                    just(Token::Op("@")),
                    mathematical_expression
                )
            )
        ),
        |(name, (array, (body, fake_span)))| { // TODO: Take array into account
            let array = Box::new(match array {
                Some(s) => s,
                None => (Expr::Value { val: Value::Null }, fake_span.clone())
            });
            (name, (body, fake_span))
        }
    )(input)
}

fn pointer_array_variable_placement<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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

fn parse_namespace<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Namespace)),
            then(
                separated_list1(
                    just(Token::Op("::")),
                    ident
                ).context("Expected namespace name"),
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

fn placement<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
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
                                    mathematical_expression
                                ),
                                |a| Some(a)
                            ),
                            map(just(Token::K(Keyword::In)), |_| None),
                            map(just(Token::K(Keyword::Out)), |_| None),
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
                            then(
                                pointer_size_type,
                                mathematical_expression
                            )
                        )
                    ),
                    |(name, (pointer_type, body))| (name, body) // TODO take the type of the pointer into account
                ),
                map(
                    preceded(
                        just(Token::Op("*")),
                        then(
                            ident_local,
                            preceded( // TODO: This is probably wrong
                                just(Token::Separator('{')),
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

fn using<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    expression_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Using)),
            then(
                ident.context("Expected identifier after \"using\""),
                opt(preceded(
                    just(Token::Op("=")),
                    non_opt(using_declaration).context("Expected type")
                ))
            )
        ),
        |(new_name, old_name), span| {
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

fn normal_placement<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    expression_recovery(preceded(
        peek(choice((
                ignore(just(Token::K(Keyword::BigEndian))),
                ignore(just(Token::K(Keyword::LittleEndian))),
                ignore(value_type_any),
        ))),
        placement.context("Expected variable declaration")
    ))(input)
}

fn todo_placement<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    expression_recovery(preceded(
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
        )).context("Expected placement")
    ))(input)
}

fn statements<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    let semicolon_expr = choice((
        using,
        normal_placement,
        todo_placement,
        parse_struct,
        parse_union,
        parse_enum,
        parse_bitfield,
        function_definition,
        parse_namespace,
    ));

    let no_semicolon_expr = function_statement;

    expression_recovery(choice((
        terminated(
            semicolon_expr.context("Invalid expression"),
            then(
                opt(
                    preceded(
                        then(
                            just(Token::Separator('[')),
                            just(Token::Separator('['))
                        ),
                        attribute
                    )
                ),
                many1(just(Token::Separator(';'))).context("Missing ;")
            )
        ),
        terminated(
            no_semicolon_expr.context("Invalid expression"),
            then(
                opt(
                    preceded(
                        then(
                            just(Token::Separator('[')),
                            just(Token::Separator('['))
                        ),
                        attribute
                    )
                ),
                many0(just(Token::Separator(';')))
            )
        )
    )))(input)
}

fn ident<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<String>> {
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

fn ident_local<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    map(
        ident,
        |(name, span)| (Expr::Local { name: (name, span.clone()) }, span)
    )(input)
}

fn value_type_any<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<HexType>> {
    map_res(
        spanned(take(1 as usize)),
        |(consumed, span): (Tokens<'a>, Range<usize>)|{
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

fn numeric<'a>(input: Tokens<'a>) -> TokResult<Tokens<'a>, Spanned<Expr>> {
    map_res(
        spanned(take(1 as usize)),
        |(consumed, span): (Tokens<'a>, Range<usize>)|{
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

fn parser<'a>(input: Tokens<'a>) -> TokResult<Tokens, Spanned<Expr>> {
    map_with_span(
        many_until(
            |input: Tokens<'a>| match statements(input) {
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

fn recover_err<'a>(e: &TokError<'a>) -> Tokens<'a> {
    match e {
        GenericErrorTree::Base { location, kind: _ } => *location,
        GenericErrorTree::Stack { base, contexts: _ } => recover_err(base),
        GenericErrorTree::Alt(v) => recover_err(v.get(0).unwrap()),
    }
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
pub(crate) fn token_parse(tokens: Vec<TokSpan>) -> (HashMap<String, Spanned<NamedNode>>, Spanned<Expr>) {
    let hmap = HashMap::new();
    let (_, ex) = parser(Tokens::new(&tokens, tokens[0].extra.0)).expect("Unrecovered error happenned in parser");
    //let ex = (Expr::Dollar, 0..1);
    (hmap, ex)
}
