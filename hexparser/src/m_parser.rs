use std::collections::HashMap;

use nom::{
    IResult,
    branch::alt as choice,
    bytes::complete::{
        tag as just,
    },
    combinator::{
        eof,
        map,
        peek,
        not,
        opt
    },
    sequence::{pair as then, terminated},
    multi::{
        many_till as many_until,
        many1, many0
    }
};
use serde::{Deserialize, Serialize};

use crate::{token::{Spanned, TokSpan, Tokens, Token, Keyword, ValueType, BuiltFunc}};

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
    ExprList {list: Box<Vec<Spanned<Self>>> },
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
        unkown: Box<Spanned<Self>>,
        condition: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>
    },
    Definition {
        value_type: Spanned<String>,
        name: Spanned<String>,
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
        args: Vec<(Spanned<String>, Spanned<String>)>,
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
}

#[derive(Clone, Debug)]
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
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Add,
    Sub,
    LNot,
    BNot,
}

fn function_call<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            namespace_resolution,
            then(
                just(Token::Separator('(')),
                terminated(
                    many0(then(
                        mathematical_expression,
                        just(Token::Separator(')'))
                    )),
                    just(Token::Separator(')')),
                )
            )
        ),
        |a| Expr::Call((), ()) // TODO
    )(input)
}

fn string_literal<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    todo!()
}

fn namespace_resolution<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    many0(
        then(
            just(Token::Op("::")),
            ident
        )
    )(input)
}

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
    ))(input)
}

fn r_value<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
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
                r_value
            )
        ))
    )(input)
}

fn factor<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        just(numeric),
        then(
            peek(choice((
                just(Token::Op("+")),
                just(Token::Op("-")),
                just(Token::Op("~")),
                just(Token::Op("!"))
            ))),
            mathematical_expression
        ),
        then(
            just(Token::Separator('(')),
            then(
                mathematical_expression,
                just(Token::Separator(')'))
            )
        ),
        then(
            ident,
            then(
                namespace_resolution,
                choice((
                    function_call,
                    scope_resolution,
                    r_value
                ))
            )
        ),
        then(
            choice((
                just(Token::K(Keyword::Parent)),
                just(Token::K(Keyword::This))
            )),
            r_value
        ),
        just(Token::Op("$")),
        then(
            choice((
                just(Token::B(BuiltFunc::AddressOf)),
                just(Token::B(BuiltFunc::SizeOf)),
            )),
            then(
                just(Token::Separator('(')),
                then(
                    choice((
                        then(
                            choice((
                                ident,
                                just(Token::K(Keyword::Parent)),
                                just(Token::K(Keyword::This))
                            )),
                            r_value
                        ),
                        value_type_any,
                        just(Token::Op("$"))
                    )),
                    just(Token::Separator(')'))
                )
            )
        )
    ))(input)
}

fn cast_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        then(
            peek(choice((
                just(Token::K(Keyword::BigEndian)),
                just(Token::K(Keyword::LittleEndian)),
                value_type_any
            ))),
            then(
                parse_type,
                then(
                    peek(just(Token::Separator('('))),
                    factor
                )
            )
        ),
        factor
    ))(input)
}

fn unary_expression<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    choice((
        then(
            choice((
                just(Token::Op("+")),
                just(Token::Op("-")),
                just(Token::Op("!")),
                just(Token::Op("~"))
            )),
            cast_expression
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

fn attribute<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    map(
        then(
            then(
                ident,
                then(
                    just(Token::Separator('(')),
                    opt(then(
                        string_literal,
                        just(Token::Separator(')'))
                    ))
                ) // TODO: Comma separated
            ),
            then(
                just(Token::Separator(']')),
                just(Token::Separator(']'))
            )
        ),
        |((a, b), c)| a // TODO
    )(input)
}

fn function_definition<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        choice((
            then(
                value_type_auto,
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
            ),
            then(
                then(
                    then(
                        parse_type,
                        opt(ident)
                    ),
                    opt(
                        then(
                            just(Token::Op("=")),
                            mathematical_expression
                        )
                    )
                ),
                just(Token::Separator(','))
            )
        )),
        then(
            just(Token::Separator(')')),
            then(
                just(Token::Separator('{')),
                function_statement
            )
        )
    )(input)
}

fn function_variable_decl<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        parse_type,
        then(
            ident,
            choice((
                then(
                    just(Token::Separator('{')),
                    then(
                        not(peek(just(Token::Separator('{')))),
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
            then(
                ident,
                then(
                    just(Token::Op("=")),
                    function_variable_assignment
                )
            ),
            then(
                just(Token::Op("$")),
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
                        r_value,
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
                    namespace_resolution,
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

fn function_variable_assignment<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    mathematical_expression(input)
}

fn function_variable_compound_assignment<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    mathematical_expression(input)
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
    ))
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
    )
}

fn function_while_loop<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    then(
        mathematical_expression,
        then(
            just(Token::Separator(')')),
            statement_body
        )
    )
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
    )
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

fn parse_type<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
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
    ))
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
    )
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
                            namespace_resolution,
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
                then(
                    just(Token::K(Keyword::Using)),
                    then(
                        ident,
                        just(Token::Op("="))
                    )
                ),
                using_declaration
            ),
            |((a, (b,c)), d)| b // TODO
        ),
        map(
            then(
                then(
                    just(Token::K(Keyword::Using)),
                    ident
                ),
                forward_declaration
            ),
            |((a, b), c)| b //TODO
        ),
        map(
            then(
                peek(
                    choice((
                            just(Token::K(Keyword::BigEndian)),
                            just(Token::K(Keyword::LittleEndian)),
                            value_type_any,
                    )) 
                ),
                placement
            ),
            |(a, b)| b // TODO
        ),
        map(
            then(
                then(
                    then(
                        then(
                            then(
                                peek(ident),
                                not(just(Token::Op("=")))
                            ),
                            not(just(Token::Separator('.')))
                        ),
                        not(just(Token::Separator('[')))
                    ),
                    namespace_resolution
                ),
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
            ),
            |(((((a, _), _), _), b), c)| a // TODO
        ),
        map(
            then(
                then(
                    just(Token::K(Keyword::Struct)),
                    ident
                ),
                parse_struct
            ),
            |((a, b), c)| b // TODO
        ),
        map(
            then(
                then(
                    then(
                        just(Token::K(Keyword::Union)),
                        just(Token::Separator('{'))
                    ),
                    ident
                ),
                parse_union
            ),
            |(((a, b), c), d)| c // TODO
        ),
        map(
            then(
                then(
                    then(
                        just(Token::K(Keyword::Enum)),
                        just(Token::Separator(':'))
                    ),
                    ident
                ),
                parse_enum
            ),
            |(((a, b), c), d)| c // TODO
        ),
        map(
            then(
                then(
                    then(
                        just(Token::K(Keyword::Bitfield)),
                        just(Token::Separator('{'))
                    ),
                    ident
                ),
                parse_bitfield
            ),
            |(((a, b), c), d)| c // TODO
        ),
        map(
            then(
                then(
                    then(
                        just(Token::K(Keyword::Fn)),
                        just(Token::Separator('('))
                    ),
                    ident
                ),
                function_definition
            ),
            |(((a, b), c), d)| c // TODO
        ),
        map(
            then(
                then(
                    just(Token::K(Keyword::Namespace)),
                    ident
                ),
                parse_namespace
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

fn ident<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Spanned<Expr>> {
    todo!()
}

fn value_type_any<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Tokens<'a>> {
    todo!()
}

fn value_type_auto<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Tokens<'a>> {
    todo!()
}

fn numeric<'a>(input: Tokens<'a>) -> IResult<Tokens<'a>, Tokens<'a>> {
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

// Hashmap contains the names of named expressions and their clones
pub fn placeholder_parse(tokens: Vec<TokSpan>) -> (HashMap<String, Spanned<NamedNode>>, Spanned<Expr>) {
    let hmap = HashMap::new();
    let (_, ex) = placeholder_parser(Tokens{tokens: &tokens}).expect("Unrecovered error happenned in parser");
    //let ex = (Expr::Dollar, 0..1);
    (hmap, ex)
}
