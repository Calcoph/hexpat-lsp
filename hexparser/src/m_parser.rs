use std::{ops::Range, io::ErrorKind};

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

use crate::{token::{Spanned, TokSpan, Tokens, Token, Keyword, ValueType, HexNum}, combinators::{spanned, ignore, to, map_with_span, fold_many0_once}, m_parser::{function::{function_statement, function_definition}, operations::mathematical_expression}, recovery_err::{TokResult, ToRange, RecoveredError, TokError, expression_recovery, non_opt, statement_recovery}};

pub use operations::UnaryOp;

use self::function::function_controlflow_statement;

mod function;
mod factor;
pub(crate) mod operations;
mod code_block;

pub trait Boxable<T> {
    fn boxed(self) -> Option<Box<T>>;
}

impl<T> Boxable<T> for Option<T> {
    fn boxed(self) -> Option<Box<T>> {
        self.map(|t| Box::new(t))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Null,
    Bool(bool),
    Num(HexNum),
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

#[derive(Debug, Clone)]
pub enum Statement {
    Return { value: Box<Spanned<Expr>> },
    Continue,
    Break,
    Assignment {
        loperand: Box<Spanned<Expr>>,
        operator: AssignmentOp,
        roperand: Box<Spanned<Expr>>
    },
    Func {
        name: Spanned<String>,
        args: Spanned<Vec<Spanned<FuncArgument>>>,
        body: Spanned<Vec<Spanned<Statement>>>
    },
    Struct {
        name: Spanned<String>,
        body: Spanned<Vec<Spanned<Statement>>>,
        template_parameters: Vec<Spanned<Expr>>
    },
    Namespace {
        name: Box<Spanned<Expr>>,
        body: Spanned<Vec<Spanned<Statement>>>
    },
    Enum {
        name: Spanned<String>,
        value_type: Spanned<HexTypeDef>,
        body: Spanned<Vec<Spanned<Expr>>>
    },
    Bitfield {
        name: Spanned<String>,
        body: Spanned<Vec<Spanned<Statement>>>
    },
    Using {
        new_name: Spanned<String>,
        template_parameters: Vec<Spanned<Expr>>,
        old_name: Spanned<HexTypeDef>
    },
    Error,
    Union {
        name: Spanned<String>,
        body: Spanned<Vec<Spanned<Statement>>>,
        template_parameters: Vec<Spanned<Expr>>
    },
    ArrayDefinition {
        value_type: Spanned<HexTypeDef>,
        array_name: Box<Spanned<Expr>>,
        size: Box<Spanned<Expr>>,
        body: Box<Spanned<Expr>>
    },
    WhileLoop {
        condition: Box<Spanned<Expr>>,
        body: Spanned<Vec<Spanned<Statement>>>
    },
    ForLoop {
        var_init: Box<Spanned<Statement>>,
        var_test: Box<Spanned<Expr>>,
        var_change: Box<Spanned<Statement>>,
        body: Spanned<Vec<Spanned<Statement>>>,
    },
    Definition(Definition),
    Padding { padding_body: Spanned<Expr> },
    Call(FuncCall),
    BitFieldEntry {
        name: Spanned<String>,
        length: Box<Spanned<Expr>>
    },
    Match {
        parameters: Vec<Spanned<Expr>>,
        branches: Vec<MatchBranch>
    },
    TryCatch {
        try_block: Spanned<Vec<Spanned<Statement>>>,
        catch_block: Spanned<Vec<Spanned<Statement>>>
    },
    If {
        test: Box<Spanned<Expr>>,
        consequent: Spanned<Vec<Spanned<Statement>>>,
    },
    IfBlock {
        ifs: Spanned<Vec<Spanned<Statement>>>,
        alternative: Spanned<Vec<Spanned<Statement>>>
    },
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub value_type: Spanned<HexTypeDef>,
    pub name: Box<Spanned<Expr>>,
    pub body: Box<Spanned<Expr>>
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub func_name: Box<Spanned<Expr>>,
    pub arguments: Spanned<Vec<Spanned<Expr>>>
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
    Call(FuncCall),
    EnumEntry {
        name: Spanned<String>,
        value: Box<Spanned<Self>>
    },
    NamespaceAccess {
        previous: Box<Spanned<Self>>,
        name: Spanned<String>
    },
    Access {
        item: Box<Spanned<Self>>,
        member: Box<Spanned<Self>>
    },
    ArrayAccess {
        array: Box<Spanned<Self>>,
        index: Box<Spanned<Self>>
    },
    Attribute {
        arguments: Spanned<Vec<Spanned<Self>>>
    },
    AttributeArgument {
        name: Box<Spanned<Self>>,
        value: Vec<Spanned<Self>>
    },
    WhileLoop {
        condition: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>
    },
    Cast {
        cast_operator: Spanned<HexTypeDef>,
        operand: Box<Spanned<Self>>
    },
    Type {
        val: HexTypeDef
    },
    Definition(Definition),
}

#[derive(Debug, Clone)]
pub struct MatchBranch {
    pub case: Spanned<MatchCase>,
    pub body: Spanned<Vec<Spanned<Statement>>>
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
    Assign(AssignmentOp)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentOp {
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

fn parameters<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Vec<Spanned<Expr>>> {
    delimited(
        just(Token::Separator('(')),
        separated_list0(
            just(Token::Separator(',')),
            mathematical_expression
        ),
        just(Token::Separator(')')).context("Missing )"),
    )(input)
}

fn function_call_expr<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    expression_recovery(map(
        then(
            namespace_resolution,
            parameters
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
                Expr::Call(FuncCall {
                    func_name: Box::new(func_name),
                    arguments
                }),
                span
            )
        }
    ))(input)
}


fn function_call_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map(
        then(
            namespace_resolution,
            parameters
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
                Statement::Call(FuncCall {
                    func_name: Box::new(func_name),
                    arguments
                }),
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

// These comments are just to find this function
// r_value
// rvalue
// parseRValue
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
                ),
                map_with_span(
                    just(Token::Op("$")),
                    |_, span| (Expr::Local { name: (String::from("$"), span.clone())} , span)
                ),
                map_with_span(
                    just(Token::K(Keyword::Null)),
                    |_, span| (Expr::Local { name: (String::from("nul"), span.clone())} , span)
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
        |(item, array_index, member), span| {
            match array_index {
                Some(array_index) => {
                    let array_span = item.1.start..array_index.1.end;
                    let item = (Expr::ArrayAccess {
                        array: Box::new(item),
                        index: Box::new(array_index)
                    }, array_span);
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
                },
                None => {
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
            }
        }
    )(input)
}

fn attribute_arg<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map(
        then(
            namespace_resolution,
            opt(spanned(delimited(
                just(Token::Separator('(')),
                separated_list1(
                    just(Token::Separator(',')),
                    mathematical_expression
                ),
                just(Token::Separator(')'))
            )))
        ),
        |((name, name_span), value)| {
            let (value, span) = match value {
                Some((val, span)) => {
                    let span = name_span.start..span.end;
                    (val, span)
                },
                None => (vec![], name_span.clone()),
            };
            let expr = Expr::AttributeArgument {
                name: Box::new((name, name_span)),
                value
            };

            (expr, span)
        }
    )(input)
}

fn attribute<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Option<Spanned<Expr>>> {
    opt(delimited(
        then(
            just(Token::Separator('[')),
            just(Token::Separator('['))
        ),
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
        ),
        non_opt(then(
            just(Token::Separator(']')).context("Missing ]]"),
            just(Token::Separator(']')).context("Missing ]")
        ))
    ))(input)
}

#[derive(Debug, Clone)]
pub enum MatchCaseElement {
    Range(Spanned<Expr>, Spanned<Expr>),
    Expr(Spanned<Expr>)
}

#[derive(Debug, Clone)]
pub enum MatchCaseItem {
    Anything,
    Element(MatchCaseElement),
    OrList(Vec<MatchCaseElement>)
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub case: Vec<Spanned<MatchCaseItem>>
}

fn case_parameters<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<MatchCase>> {
    map_with_span(delimited(
        just(Token::Separator('(')),
        separated_list1(
            just(Token::Separator(',')),
            choice((
                to(just(Token::K(Keyword::Underscore)), MatchCaseItem::Anything),
                map_with_span(separated_list1(
                    just(Token::Separator('|')),
                    map(
                        then(
                            mathematical_expression,
                            opt(preceded(
                                tuple((just(Token::Separator('.')),just(Token::Separator('.')),just(Token::Separator('.')))),
                                mathematical_expression
                            ))
                        ),
                        |(expr, range_end)| {
                            match range_end {
                                Some(range_end) => {
                                    // expr is range_start
                                    MatchCaseElement::Range(expr, range_end)
                                },
                                None => MatchCaseElement::Expr(expr),
                            }
                        }
                    )
                ), |mut elements, span| {
                    if elements.len() == 1 {
                        (MatchCaseItem::Element(elements.pop().unwrap()), span)
                    } else {
                        (MatchCaseItem::OrList(elements), span)
                    }
                })
            )),
        ),
        just(Token::Separator(')'))
    ), |case, span| (MatchCase {case}, span))(input)
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
    Parameted(Box<HexType>, Vec<Spanned<Expr>>),
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

struct TODO;

fn custom_type_parameters<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    map_with_span(opt(delimited(
        just(Token::Op("<")),
        separated_list1(
            just(Token::Separator(',')),
            choice((
                map(parse_type, |(r#type, span)| (Expr::Type { val: r#type }, span)),
                mathematical_expression
            ))
        ),
        just(Token::Op(">"))
    )), |a, span| (Expr::Value { val: Value::Null }, span))(input)
}

fn custom_type<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<HexType>> {
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
            custom_type_parameters
        ),
        |(r#type, type_parameters)| r#type // TODO: don't ignore type_parameters
    )(input)
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
                custom_type,
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

fn non_dolar_assignment_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
        tuple((
            ident_local,
            opt(choice((
                to(just(Token::Op("+")), AssignmentOp::Add),
                to(just(Token::Op("-")), AssignmentOp::Sub),
                to(just(Token::Op("*")), AssignmentOp::Mul),
                to(just(Token::Op("/")), AssignmentOp::Div),
                to(just(Token::Op("%")), AssignmentOp::Mod),
                to(just(Token::Op("<<")), AssignmentOp::LShift),
                to(then(just(Token::Op(">")), just(Token::Op(">"))), AssignmentOp::RShift),
                to(just(Token::Op("|")), AssignmentOp::BOr),
                to(just(Token::Op("&")), AssignmentOp::BAnd),
                to(just(Token::Op("^")), AssignmentOp::BXor),
            ))),
            preceded(
                just(Token::Op("=")),
                mathematical_expression.context("Expected mathematical expression")
            )
        )),
        |(loperand, assignment, roperand), span| {
            let assignment = match assignment {
                Some((ass, _)) => ass,
                None => AssignmentOp::Just
            };
            let expr = Statement::Assignment {
                loperand: Box::new(loperand),
                operator: assignment,
                roperand: Box::new(roperand),
            };

            (expr, span)
        }
    ))(input)
}

fn assignment_expr<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
        tuple((
            choice((
                ident_local,
                map_with_span(
                    just(Token::Op("$")),
                    |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
                )
            )),
            opt(choice((
                to(just(Token::Op("+")), AssignmentOp::Add),
                to(just(Token::Op("-")), AssignmentOp::Sub),
                to(just(Token::Op("*")), AssignmentOp::Mul),
                to(just(Token::Op("/")), AssignmentOp::Div),
                to(just(Token::Op("%")), AssignmentOp::Mod),
                to(just(Token::Op("<<")), AssignmentOp::LShift),
                to(then(just(Token::Op(">")), just(Token::Op(">"))), AssignmentOp::RShift),
                to(just(Token::Op("|")), AssignmentOp::BOr),
                to(just(Token::Op("&")), AssignmentOp::BAnd),
                to(just(Token::Op("^")), AssignmentOp::BXor),
            ))),
            preceded(
                just(Token::Op("=")),
                mathematical_expression.context("Expected mathematical expression")
            )
        )),
        |(loperand, assignment, roperand), span| {
            let assignment = match assignment {
                Some((ass, _)) => ass,
                None => AssignmentOp::Just
            };
            let expr = Statement::Assignment {
                loperand: Box::new(loperand),
                operator: assignment,
                roperand: Box::new(roperand),
            };

            (expr, span)
        }
    ))(input)
}

fn assignment_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
        tuple((
            choice((
                ident_local,
                map_with_span(
                    just(Token::Op("$")),
                    |_, span| (Expr::Local { name: (String::from("$"), span.clone()) }, span)
                )
            )),
            opt(choice((
                to(just(Token::Op("+")), AssignmentOp::Add),
                to(just(Token::Op("-")), AssignmentOp::Sub),
                to(just(Token::Op("*")), AssignmentOp::Mul),
                to(just(Token::Op("/")), AssignmentOp::Div),
                to(just(Token::Op("%")), AssignmentOp::Mod),
                to(just(Token::Op("<<")), AssignmentOp::LShift),
                to(then(just(Token::Op(">")), just(Token::Op(">"))), AssignmentOp::RShift),
                to(just(Token::Op("|")), AssignmentOp::BOr),
                to(just(Token::Op("&")), AssignmentOp::BAnd),
                to(just(Token::Op("^")), AssignmentOp::BXor),
            ))),
            preceded(
                just(Token::Op("=")),
                mathematical_expression.context("Expected mathematical expression")
            )
        )),
        |(loperand, assignment, roperand), span| {
            let assignment = match assignment {
                Some((ass, _)) => ass,
                None => AssignmentOp::Just
            };
            let expr = Statement::Assignment {
                loperand: Box::new(loperand),
                operator: assignment,
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

fn member_variable<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    choice((
        map_with_span(
            tuple(( // parseMemberArrayVariable parse_member_variable
                parse_type,
                namespace_resolution,
                array_declaration,
                opt(choice((
                    preceded(
                        just(Token::Op("@")),
                        non_opt(mathematical_expression).context("Expected mathematical expression")
                    ),
                    preceded(
                        just(Token::Op("=")),
                        delimited(
                            just(Token::Separator('[')),
                            map_with_span(
                                separated_list1(
                                    just(Token::Separator(',')),
                                    mathematical_expression
                                ),
                                |elements, span| {
                                    (
                                        Expr::ExprList { list: elements },
                                        span
                                    )
                                }
                            ),
                            just(Token::Separator(']'))
                        )
                    )
                )))
            )),
            |(value_type, name, array_size, body), span| {
                let body = Box::new(match body {
                    Some(body) => body,
                    None => (Expr::Value { val: Value::Null }, span.clone()),
                });

                match array_size {
                    Some(array_size) => (
                        Statement::ArrayDefinition {
                            value_type,
                            array_name: Box::new(name),
                            size: Box::new(array_size),
                            body
                        },
                        span
                    ),
                    None => (
                        Statement::Definition(Definition {
                            value_type,
                            name: Box::new(name),
                            body
                        }),
                        span
                    ),
                }
            }
        ),
        map_with_span(
            choice((
                tuple((
                    parse_type,
                    namespace_resolution,
                    terminated( // TODO: remove "terminated" so "In" part is taken into account
                        preceded(
                            just(Token::Op("@")),
                            mathematical_expression.context("Expected mathematical expression")
                        ),
                        opt(preceded(
                            just(Token::K(Keyword::In)),
                            mathematical_expression
                        ))
                    )
                )),
                tuple((
                    parse_type,
                    namespace_resolution,
                    preceded(
                        just(Token::Op("=")),
                        mathematical_expression
                    )
                )),
                map_with_span( // parseMemberVariable parse_member_variable
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
                Statement::Definition(Definition {
                    value_type,
                    name: Box::new(name),
                    body: Box::new(body)
                }),
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
                        parse_type
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
                    Statement::Definition(Definition {
                        value_type,
                        name: Box::new(name),
                        body: body
                    }),
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
                            parse_type,
                            opt(preceded(
                                just(Token::Op("@")),
                                non_opt(mathematical_expression).context("Expected mathematical expression")
                            ))
                        )
                    )
                ))
            ),
            |(value_type,
                (name, array_size,
                    (pointer_type, body)
                )
            ), span| {
                let body = Box::new(match body {
                    Some(body) => body,
                    None => (Expr::Value { val: Value::Null }, span.clone()),
                });

                match array_size {
                    Some(array_size) => (
                        Statement::ArrayDefinition {
                            value_type,
                            array_name: Box::new(name),
                            size: Box::new(array_size),
                            body
                        },
                        span
                    ),
                    None => (
                        Statement::Definition(Definition {
                            value_type,
                            name: Box::new(name),
                            body
                        }),
                        span
                    )
                }
            }
        ),
        map_with_span(
            choice((
                tuple((
                    parse_type,
                    terminated( // TODO: remove "terminated" so "In" part is taken into account
                        preceded(
                            just(Token::Op("@")),
                            mathematical_expression.context("Expected mathematical expression")
                        ),
                        opt(preceded(
                            just(Token::K(Keyword::In)),
                            mathematical_expression
                        ))
                    )
                )),
                tuple((
                    parse_type,
                    preceded(
                        just(Token::Op("=")),
                        mathematical_expression
                    )
                )),
                map_with_span( // parseMemberVariable parse_member_variable
                    parse_type,
                    |value_type, span| {
                        (
                            value_type,
                            (Expr::Value { val: Value::Null }, span)
                        )
                    }
                )
            )),
            |(value_type, body), span| (
                Statement::Definition(Definition {
                    value_type,
                    name: Box::new((Expr::Value { val: Value::Null }, span.clone())),
                    body: Box::new(body)
                }),
                span
            )
        ),
    ))(input)
}

fn member_declaration<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    preceded(
        peek(choice((
            ignore(just(Token::K(Keyword::BigEndian))),
            ignore(just(Token::K(Keyword::LittleEndian))),
            ignore(value_type_any),
            ignore(ident)
        ))),
        choice((
            function_call_statement,
            member_variable
        ))
    )(input)
}

pub(crate) fn member<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(choice((
        code_block::member::conditional,
        code_block::member::match_statement,
        code_block::member::try_catch,
        terminated(
            choice((
                assignment_statement,
                map_with_span(preceded(
                    just(Token::V(ValueType::Padding)),
                    delimited(
                        just(Token::Separator('[')).context("Missing ["),
                        padding.context("Expected padding expression"),
                        just(Token::Separator(']')).context("Missing ]")
                    )
                ), |padding_body, span| {
                    (
                        Statement::Padding {
                            padding_body
                        },
                        span
                    )
                }),
                member_declaration,
                function_controlflow_statement
            )),
            then(
                attribute,
                many1(just(Token::Separator(';'))).context("Missing ;")
            )
        )
    )))(input)
}

pub(crate) fn template_list<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Vec<Spanned<Expr>>> {
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
            list
        }
    )(input)
}

pub(crate) fn parse_struct<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Struct)),
            tuple((
                ident.context("Expected struct name"),
                opt(template_list),
                opt(preceded(
                    just(Token::Op(":")),
                    non_opt(separated_list1(
                        just(Token::Separator(',')),
                        custom_type
                    )).context("Expected parent type")
                )),
                delimited(
                    just(Token::Separator('{')).context("Missing {"),
                    spanned(many0(member)),
                    just(Token::Separator('}')).context("Expected } or valid struct expression")
                )
            ))
        ),
        |(name, template_parameters, inheritance, body), span| { // TODO: Take into account the inheritance.
            let template_parameters = match template_parameters {
                Some(t) => t,
                None => Vec::new()
            };
            (
                Statement::Struct {
                    name,
                    body,
                    template_parameters
                },
                span
            )
        }
    ))(input)
}

fn parse_union<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
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
        |(name, template_parameters, body), span| {
            let template_parameters = match template_parameters {
                Some(t) => t,
                None => Vec::new()
            };
            (
                Statement::Union {
                    name,
                    body,
                    template_parameters,
                },
                span
            )
        }
    ))(input)
}

fn parse_enum<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
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
                    spanned(terminated(separated_list0(
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
                    ), opt(just(Token::Separator(','))))),
                    just(Token::Separator('}')).context("Expected } or valid enum expression")
                )
            )
        ),
        |((name, value_type), (entries, entries_span)), span| {
            let entries = entries.into_iter()
                .map(|((name, name_span), value)| (Expr::EnumEntry { name: (name, name_span.clone()), value: Box::new(value) }, name_span)).collect();
            (
                Statement::Enum {
                    name,
                    value_type,
                    body: (entries, entries_span)
                },
                span
            )
        }
    ))(input)
}

fn bitfield_entry_statements<'a, 'b: 'a>() -> (
    impl FnMut(Tokens<'a,'b>) -> TokResult<'a, 'b, Spanned<Statement>>,
    impl FnMut(Tokens<'a,'b>) -> TokResult<'a, 'b, Spanned<Statement>>
) {
    let semicolon_expr = terminated(choice((
        non_dolar_assignment_statement,
        map_with_span(
            separated_pair(
                preceded(
                    opt(choice((
                        just(Token::K(Keyword::Unsigned)),
                        just(Token::K(Keyword::Signed))
                    ))),
                    ident
                ),
                just(Token::Op(":")),
                mathematical_expression
            ),
            |(name, length), span| (
                Statement::BitFieldEntry {
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
                Statement::BitFieldEntry {
                    name: (String::from("padding"), pad_span),
                    length: Box::new(length)
                },
                span
            )
        ),
        map_with_span(
            function_call_statement,
            |(name, n_span), span| (
                Statement::BitFieldEntry {
                    name: ("FUNCTION".to_string(), n_span.clone()), // TODO
                    length: Box::new((Expr::Value { val: Value::Null }, n_span)) // TODO
                },
                span
            )
        ),
        map_with_span(
            then(
                opt(choice((
                    ignore(parse_type), // TODO: Don't ignore
                    ignore(tuple(( // TODO: Don't ignore
                        namespace_resolution,
                        custom_type_parameters,
                    )))
                ))),
                choice((
                    then(
                        ident,
                        delimited(
                            just(Token::Separator('[')),
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
                            )),
                            just(Token::Separator(']')),
                        )
                    ),
                    separated_pair(
                        ident,
                        just(Token::Op(":")),
                        mathematical_expression
                    ),
                    then(
                        ident,
                        to(member_variable, Expr::Value { val: Value::Null }) // TODO: Don't ignore
                    )
                ))
            ),
            |(name, other), span| (
                Statement::BitFieldEntry {
                    name: ("TODO".to_string(), span.clone()),
                    length: Box::new((Expr::Value { val: Value::Null }, span.clone()))
                },
                span
            )
        ),
        function_controlflow_statement
    )), attribute);

    let no_semicolon_expr = choice((
        code_block::bitfield::conditional,
        code_block::bitfield::try_catch,
        code_block::bitfield::match_statement,
    ));

    (semicolon_expr, no_semicolon_expr)
}

fn bitfield_entry<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    let (semicolon_expr, no_semicolon_expr) = bitfield_entry_statements();

    statement_recovery(choice((
        terminated(
            semicolon_expr,
            many1(just(Token::Separator(';'))).context("Missing ;")
        ),
        no_semicolon_expr,
    )))(input)
}

fn parse_bitfield<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
        preceded(
            just(Token::K(Keyword::Bitfield)),
            then(
                ident.context("Expected bitfield name"),
                delimited(
                    just(Token::Separator('{')).context("Missing {"),
                    many0(bitfield_entry),
                    just(Token::Separator('}')).context("Expected } or valid bitfield expression")
                )
            )
        ),
        |(name, body), span| {
            let body = {
                let span = match body.len() {
                    0 => span.clone(),
                    _ => body.get(0).unwrap().1.start..body.get(body.len()-1).unwrap().1.end
                };

                (
                    body,
                    span
                )
            };
            (
                Statement::Bitfield {
                    name,
                    body
                },
                span
            )
        }
    ))(input)
}

fn array_variable_placement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, (Spanned<Expr>, Option<Option<Spanned<Expr>>>, Spanned<Expr>)> {
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
        |((name, name_span), array_size, body)| { // TODO: Take section into account
            let body = match body {
                Some((b, section)) => b,
                None => (Expr::Value { val: Value::Null }, name_span.clone()),
            };
            ((name, name_span), Some(array_size), body)
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
            parse_type,
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

pub(crate) fn parse_namespace<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
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
        |(name, body), span| {
            let name = Box::new(name.into_iter()
                .fold((Expr::Value { val: Value::Null }, 0..1), |(accum, acc_span), (next, next_span)| {
                    match accum {
                        Expr::Value { .. } => ( // first case
                            Expr::Local {
                                name: (next, next_span.clone())
                            },
                            next_span
                        ),
                        _ => ( // general case
                            Expr::NamespaceAccess {
                                previous: Box::new((accum, acc_span.clone())),
                                name: (next, next_span.clone())
                            },
                            acc_span.start..next_span.end
                        )
                    }
                }));
            (
                Statement::Namespace {
                    name,
                    body
                },
                span
            )
        }
    ))(input)
}

fn placement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
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

                        ((name, fake_span), None, body)
                    }
                ),
                map(
                    preceded(
                        just(Token::Op("*")),
                        separated_pair(
                            ident_local,
                            just(Token::Op(":")),
                            separated_pair(
                                parse_type,
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
                    |(name, (pointer_type, (body, section)))| (name, None, body) // TODO take the type of the pointer into account
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
                    |(name, body)| (name, None, body)
                )
            ))
        ),
        |(value_type, (name, array_size, body)), span| {
            match array_size {
                Some(Some(array_size)) => (
                    Statement::ArrayDefinition {
                        value_type,
                        array_name: Box::new(name),
                        body: Box::new(body),
                        size: Box::new(array_size),
                    },
                    span
                ),
                Some(None) => {
                    let (array_name, name_span) = name;
                    (
                        Statement::ArrayDefinition {
                            value_type,
                            array_name: Box::new((array_name, name_span.clone())),
                            body: Box::new(body),
                            size: Box::new((Expr::Value { val: Value::Null }, name_span)),
                        },
                        span
                    )
                },
                None => (
                    Statement::Definition(Definition {
                        value_type,
                        name: Box::new(name),
                        body: Box::new(body)
                    }),
                    span
                )
            }
        }
    )(input)
}

fn using<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(map_with_span(
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
        |(new_name, template_parameters, old_name), span| { // TODO: Don't ignore template_parameters
            let old_name = match old_name {
                Some(old_name) => old_name,
                None => (HexTypeDef{ endianness: Endianness::Unkown, name: (HexType::Null, span.clone()) }, span.clone()),
            };
            let template_parameters = match template_parameters {
                Some(t) => t,
                None => Vec::new()
            };
            (
                Statement::Using {
                    new_name,
                    template_parameters,
                    old_name
                },
                span
            )
        }
    ))(input)
}

fn normal_placement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(preceded(
        peek(choice((
                ignore(just(Token::K(Keyword::BigEndian))),
                ignore(just(Token::K(Keyword::LittleEndian))),
                ignore(value_type_any),
                ))),
        placement.context("Expected variable declaration")
    ))(input)
}

fn todo_placement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    statement_recovery(preceded(
        peek(tuple((
            ident,
            not(just(Token::Op("="))),
            not(just(Token::Separator('.'))),
            not(just(Token::Separator('[')))
        ))),
        choice((
            function_call_statement,
            placement
        )).context("Expected placement")
    ))(input)
}

fn statements<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
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

    statement_recovery(choice((
        terminated(
            semicolon_expr,
            then(
                attribute,
                |input: Tokens<'a, 'b>| {
                    match many1(just(Token::Separator(';'))).context("Missing ;").parse(input) {
                        Ok((i, _)) => Ok((i, (Expr::Error, 0..1))), // It doesn't matter if it's not really an error, it will be ignored by terminated()
                        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                            match e {
                                GenericErrorTree::Stack { base: _, contexts } => {
                                    let (input, context) = contexts[contexts.len()-1];

                                    let span = input.span().start..input.span().start+1;
                                    match context {
                                        nom_supreme::error::StackContext::Context(error_msg) => input.state.report_error(RecoveredError(span.clone(), error_msg.to_string())),
                                        _ => unreachable!()
                                    };

                                    Ok((input, (Expr::Error, span)))
                                },
                                _ => unreachable!()
                            }
                        },
                        Err(e) => Err(e)
                    }
                }
            )
        ),
        terminated(
            no_semicolon_expr,
            then(
                attribute,
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
                Token::V(type_) => Ok((HexType::V(*type_), span)),
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
                Token::Num(num) => Ok((Expr::Value { val: Value::Num(*num) }, span)),
                Token::Bool(b) => Ok((Expr::Value { val: Value::Bool(*b) }, span)),
                _ => Err(ErrorTree::Base {
                    location: consumed,
                    kind: BaseErrorKind::External(Box::new(tokio::io::Error::new(ErrorKind::Other, "Expceted number literal")))
                }) // TODO: Expand match tree for "found X"
            }
        }
    )(input)
}

fn parser<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Vec<Spanned<Statement>>>> {
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
                    Ok((rest, (Statement::Error, span)))
                },
                Err(e) => Err(e)
            },
            eof
        ),
        |(list, _), span| (list, span)
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
pub(crate) fn token_parse(tokens: Vec<TokSpan>) -> Spanned<Vec<Spanned<Statement>>> {
    let ex = match tokens.len() {
        0 => (vec![], 0..0),
        _ => parser(Tokens::new(&tokens, tokens[0].extra.0)).expect("Unrecovered error happened in parser").1
    };
    //let ex = (Expr::Dollar, 0..1);
    ex
}
