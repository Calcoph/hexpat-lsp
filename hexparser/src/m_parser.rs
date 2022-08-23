use std::{collections::HashMap, ops::Range};

use chumsky::{prelude::*,Parser};
use serde::{Deserialize, Serialize};

use self::{enum_::{Enum, enum_parser}, struct_::{Struct, struct_parser}, func::{Func, funcs_parser}, bitfield::{BitField, bitfield_parser}, namespace::{NameSpace, namespace_parser}};

use super::{m_lexer::{Token, Keyword}, Span};

pub type Spanned<T> = (T, Span);

pub mod enum_;
pub mod func;
pub mod namespace;
pub mod struct_;
pub mod bitfield;

struct Error {
    span: Span,
    msg: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    Char(char),
    Func(String),
}

impl Value {
    fn num(self, span: Span) -> Result<f64, Error> {
        if let Value::Num(x) = self {
            Ok(x)
        } else {
            Err(Error {
                span,
                msg: format!("'{}' is not a number", self),
            })
        }
    }
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
    Value(Value),
    Dollar,
    Local(Spanned<String>),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Unary(UnaryOp, Box<Spanned<Self>>), // something
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>), // something operator something_else
    Ternary(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>), // something ? something_else : something_else_else
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>), // name arguments
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>), // if condition body
    Definition(Spanned<String>, Spanned<String>, Box<Spanned<Self>>), // type name everything_else
    BitFieldEntry(Spanned<String>, Box<Spanned<Self>>, Box<Spanned<Self>>), // name length next_entry
    EnumEntry(Spanned<String>, Box<Spanned<Self>>, Box<Spanned<Self>>), // name value next_entry
    MemberAccess(Box<Spanned<Self>>, Spanned<String>),
    ArrayAccess(Box<Spanned<Self>>, Box<Spanned<Self>>),
    NamespaceAccess(Box<Spanned<Self>>, Spanned<String>),
    Using(Box<Spanned<Self>>, Box<Spanned<Self>>), // new_name old_name
    Continue,
    Break,
}

impl Expr {
    pub fn as_value(&self) -> Option<&Value> {
        if let Self::Value(v) = self {
            Some(v)
        } else {
            None
        }
    }
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

fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let raw_expr = recursive(|raw_expr| {
            let val = filter_map(|span, tok| match tok {
                Token::Bool(x) => Ok(Expr::Value(Value::Bool(x))),
                Token::Num(_) => Ok(Expr::Value(Value::Num(42342.0))),// TODO: change 42342.0 to a proper (hex, bin, oct, dec) str->f64
                Token::Str(s) => Ok(Expr::Value(Value::Str(s))),
                Token::Char(c) => Ok(Expr::Value(Value::Char(c))),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("value");

            let ident = filter_map(|span: Range<usize>, tok| match tok {
                Token::Ident(ident) => Ok((ident.clone(), span)),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("identifier");

            let builtin_func = filter_map(|span, tok| match tok {
                Token::B(func) => Ok((func.clone(), span)),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("built in function");

            let array_definition = expr.clone()
                .or(just(Token::K(Keyword::While)).ignore_then(expr.clone().delimited_by(just(Token::Separator('(')), just(Token::Separator(')'))))) // TODO: Custom parser instead of "expr"
                .delimited_by(just(Token::Separator('[')), just(Token::Separator(']')))
                .recover_with(nested_delimiters(
                    Token::Separator('('),
                    Token::Separator(')'),
                    [
                        (Token::Separator('{'), Token::Separator('}')),
                    ],
                    |span| (Expr::Error, span),
                ));

            let definition = ident.clone()
                .then(ident)
                .then_ignore(
                    array_definition
                    .or(
                        just(Token::Separator('['))
                        .ignore_then(just(Token::Separator(']')))
                        .ignored()
                        .map_with_span(|(), span| (Expr::Value(Value::Null), span))
                    ).or_not()
                    .then_ignore(
                        just(Token::K(Keyword::Out))
                        .or(just(Token::K(Keyword::In)))
                        .or_not()
                    )
                )
                .then(just(Token::Op("@".to_string())).ignore_then(raw_expr.clone()).or_not())
                .then_ignore(
                    just(Token::Separator('['))
                        .then(just(Token::Separator('[')))
                        .then(take_until(just(Token::Separator(']')).then(just(Token::Separator(']')))))
                        .or_not()
                )
                //.map(|((name, val), body)| {
                .map_with_span(|((type_, name), val), span| {
                    let val = match val {
                        Some(val) => Box::new(val),
                        None => Box::new((Expr::Value(Value::Null), span.clone()))
                    };
                    (
                        Expr::Definition(type_, name, val),
                        span
                    )
                });

            // A list of expressions
            let items = definition.clone()
                .chain(just(Token::Separator(',')).ignore_then(definition.clone()).repeated())
                .then_ignore(just(Token::Separator(',')).or_not())
                .or_not()
                .map(|item| item.unwrap_or_else(Vec::new));

            let member_access = choice((
                    ident.clone(),
                    just(Token::K(Keyword::This)).map_with_span(|_, span| ("this".to_string(), span)),
                    just(Token::K(Keyword::Parent)).map_with_span(|_, span| ("parent".to_string(), span))
                ))
                .map_with_span(|a, span| (Expr::Local(a), span))
                .then(
                    just(Token::Separator('.'))
                    .ignore_then(choice((
                        ident.clone(),
                        just(Token::K(Keyword::Parent)).map_with_span(|_, span| ("parent".to_string(), span))
                    )))
                    .repeated()
                    .at_least(1)
                ).foldl(|a, b| {
                    let span = b.1.clone(); // TODO: Not correct
                    (Expr::MemberAccess(Box::new(a), b), span)
                });

            let namespace_access = ident.clone()
                .map_with_span(|a, span| (Expr::Local(a), span))
                .then(
                    just(Token::Op("::".to_string()))
                    .ignore_then(ident)
                    .repeated()
                    .at_least(1)
                ).foldl(|a, b| {
                    let span = b.1.clone(); // TODO: Not correct
                    (Expr::NamespaceAccess(Box::new(a), b), span)
                });

            let array_access = ident.clone()
                .map_with_span(|a, span| (Expr::Local(a), span))
                .then(
                    just(Token::Separator('['))
                    .ignore_then(expr.clone())
                    .then_ignore(just(Token::Separator(']')))
                ).map(|(a, b)| {
                    let span = a.1.clone(); // TODO: Not correct
                    (Expr::ArrayAccess(Box::new(a), Box::new(b)), span)
                });

            // 'Atoms' are expressions that contain no ambiguity
            let atom = choice((
                val.map_with_span(|expr, span| (expr, span)),
                array_access,
                namespace_access,
                member_access,
                ident.map_with_span(|a, span| (Expr::Local(a), span)),
                just(Token::Op("$".to_string()))
                    .map_with_span(|_, span: Range<usize>| (Expr::Local(("$".to_string(), span.clone())), span)),
                //.or(ident.map(Expr::Local).map_with_span(|expr, span| (expr, span)))
                builtin_func
                    .map_with_span(|a, span| (Expr::Local((a.0.to_string(), a.1)), span)),
                // Atoms can also just be normal expressions, but surrounded with parentheses
                expr.clone()
                    .delimited_by(just(Token::Separator('(')), just(Token::Separator(')'))),
            ))
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with(nested_delimiters(
                Token::Separator('('),
                Token::Separator(')'),
                [
                    (Token::Separator('['), Token::Separator(']')),
                    (Token::Separator('{'), Token::Separator('}')),
                ],
                |span| (Expr::Error, span),
            ));

            // Function calls have very high precedence so we prioritise them
            let call = atom
                .then(
                    items
                        .delimited_by(just(Token::Separator('(')), just(Token::Separator(')')))
                        .map_with_span(|args, span| (args, span))
                        .repeated(),
                )
                .foldl(|f, args| {
                    let span = f.1.start..args.1.end;
                    (Expr::Call(Box::new(f), args), span)
                });
            
            let unary = choice((
                just(Token::Op("+".to_string())).to(UnaryOp::Add),
                just(Token::Op("-".to_string())).to(UnaryOp::Sub),
                just(Token::Op("~".to_string())).to(UnaryOp::LNot),
                just(Token::Op("!".to_string())).to(UnaryOp::BNot),
            )).or_not().then(call)
                .map_with_span(|(op, a), span| {
                match op {
                    Some(o) => (Expr::Unary(o, Box::new(a)), span),
                    None => a,
                }
                });

            // Product ops (multiply and divide) have equal precedence
            let op = choice((
                just(Token::Op("*".to_string())).to(BinaryOp::Mul),
                just(Token::Op("/".to_string())).to(BinaryOp::Div),
                just(Token::Op("%".to_string())).to(BinaryOp::Mod),
            ));

            let product = unary.clone()
                .then(op.then(unary).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            // Sum ops (add and subtract) have equal precedence
            let op = just(Token::Op("+".to_string()))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub));
            let sum = product
                .clone()
                .then(op.then(product).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            // Shift ops
            let op = just(Token::Op("<<".to_string()))
                .to(BinaryOp::LShift)
                .or(just(Token::Op(">>".to_string())).to(BinaryOp::RShift));
            let shift = sum.clone()
                .then(op.then(sum).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });
            
            // Binary and
            let op = just(Token::Op("&".to_string()))
                .to(BinaryOp::BAnd);
            let b_and = shift.clone()
                .then(op.then(shift).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                }).boxed();
            
            // Binary xor
            let op = just(Token::Op("^".to_string()))
                .to(BinaryOp::BXor);
            let b_xor = b_and.clone()
                .then(op.then(b_and).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });
            
            // Binary or
            let op = just(Token::Op("|".to_string()))
                .to(BinaryOp::BOr);
            let b_or = b_xor.clone()
                .then(op.then(b_xor).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });
            
            // Relationship
            let op = choice((
                just(Token::Op(">=".to_string())).to(BinaryOp::GreaterEqual),
                just(Token::Op("<=".to_string())).to(BinaryOp::LessEqual),
                just(Token::Op(">".to_string())).to(BinaryOp::Greater),
                just(Token::Op("<".to_string())).to(BinaryOp::Less)
            ));
            
            let relation = b_or.clone()
                .then(op.then(b_or).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            // Comparison ops (equal, not-equal) have equal precedence
            let op = just(Token::Op("==".to_string()))
                .to(BinaryOp::Eq)
                .or(just(Token::Op("!=".to_string())).to(BinaryOp::NotEq));
            let comp = relation
                .clone()
                .then(op.then(relation).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });
            
            // Logical and
            let op = just(Token::Op("&&".to_string()))
                .to(BinaryOp::LAnd);
            let l_and = comp.clone()
                .then(op.then(comp).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            // Logical xor
            let op = just(Token::Op("^^".to_string()))
                .to(BinaryOp::LXor);
            let l_xor = l_and.clone()
                .then(op.then(l_and).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            // Logical or
            let op = just(Token::Op("||".to_string()))
                .to(BinaryOp::LOr);
            let l_or = l_xor.clone()
                .then(op.then(l_xor).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            l_or.clone()
                .then(
                    just(Token::Op("?".to_string()))
                        .ignore_then(l_or.clone())
                        .then_ignore(just(Token::Op(":".to_string())))
                        .then(l_or)
                        .repeated()
                ).foldl(|a, (b, c)| {
                    let span = a.1.start..c.1.end;
                    (Expr::Ternary(Box::new(a), Box::new(b), Box::new(c)), span)
                })
        });

        let ident = filter_map(|span: Range<usize>, tok| match tok {// TODO: Instead of declaring twice the parser, clone it for inside the closure
            Token::Ident(ident) => Ok((ident.clone(), span)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        })
        .labelled("identifier");

        let member_access = choice((// TODO: Instead of declaring twice the parser, clone it for inside the closure
                ident.clone(),
                just(Token::K(Keyword::This)).map_with_span(|_, span| ("this".to_string(), span)),
                just(Token::K(Keyword::Parent)).map_with_span(|_, span| ("parent".to_string(), span))
            ))
            .map_with_span(|a, span| (Expr::Local(a), span))
            .then(
                just(Token::Separator('.'))
                .ignore_then(choice((
                    ident.clone(),
                    just(Token::K(Keyword::Parent)).map_with_span(|_, span| ("parent".to_string(), span))
                )))
                .repeated()
                .at_least(1)
            ).foldl(|a, b| {
                let span = b.1.clone(); // TODO: Not correct
                (Expr::MemberAccess(Box::new(a), b), span)
            });

        let namespace_access = ident.clone()// TODO: Instead of declaring twice the parser, clone it for inside the closure
            .map_with_span(|a, span| (Expr::Local(a), span))
            .then(
                just(Token::Op("::".to_string()))
                .ignore_then(ident)
                .repeated()
                .at_least(1)
            ).foldl(|a, b| {
                let span = b.1.clone(); // TODO: Not correct
                (Expr::NamespaceAccess(Box::new(a), b), span)
            });

        let array_access = ident.clone()// TODO: Instead of declaring twice the parser, clone it for inside the closure
            .map_with_span(|a, span| (Expr::Local(a), span))
            .then(
                just(Token::Separator('['))
                .ignore_then(expr.clone())
                .then_ignore(just(Token::Separator(']')))
            ).map(|(a, b)| {
                let span = a.1.clone(); // TODO: Not correct
                (Expr::ArrayAccess(Box::new(a), Box::new(b)), span)
            });

        let array_definition = expr.clone() // TODO: Instead of declaring twice the parser, clone it for inside the closure
            .or(just(Token::K(Keyword::While)).ignore_then(expr.clone().delimited_by(just(Token::Separator('(')), just(Token::Separator(')'))))) // TODO: Custom parser instead of "expr"
            .delimited_by(just(Token::Separator('[')), just(Token::Separator(']')))
            .recover_with(nested_delimiters(
                Token::Separator('('),
                Token::Separator(')'),
                [
                    (Token::Separator('{'), Token::Separator('}')),
                ],
                |span| (Expr::Error, span),
            ));

        let definition = ident.clone()// TODO: Instead of declaring twice the parser, clone it for inside the closure
                .then(ident)
                .then_ignore(
                    array_definition
                    .or(
                        just(Token::Separator('['))
                        .ignore_then(just(Token::Separator(']')))
                        .ignored()
                        .map_with_span(|(), span| (Expr::Value(Value::Null), span))
                    ).or_not()
                    .then_ignore(
                        just(Token::K(Keyword::Out))
                        .or(just(Token::K(Keyword::In)))
                        .or_not()
                    )
                )
                .then(just(Token::Op("@".to_string())).ignore_then(raw_expr.clone()).or_not())
                .then_ignore(
                    just(Token::Separator('['))
                        .then(just(Token::Separator('[')))
                        .then(take_until(just(Token::Separator(']')).then(just(Token::Separator(']')))))
                        .or_not()
                )
                //.map(|((name, val), body)| {
                .map_with_span(|((type_, name), val), span| {
                    let val = match val {
                        Some(val) => Box::new(val),
                        None => Box::new((Expr::Value(Value::Null), span.clone()))
                    };
                    (
                        Expr::Definition(type_, name, val),
                        span
                    )
                });
        
        let assignable = choice((// TODO: Instead of declaring twice the parser, clone it for inside the closure
            just(Token::Op("$".to_string()))
                .map_with_span(|_, span| (Expr::Dollar, span)),
            array_access.clone(),
            namespace_access.clone(),
            member_access.clone(),
            ident.clone()
                .map_with_span(|a, span| (Expr::Local(a), span))
        ));
        
        let assignment = assignable.then(// TODO: Instead of declaring twice the parser, clone it for inside the closure
            choice((
                just(Token::Op("+".to_string())),
                just(Token::Op("-".to_string())),
                just(Token::Op("*".to_string())),
                just(Token::Op("/".to_string())),
                just(Token::Op("%".to_string())),
                just(Token::Op("|".to_string())),
                just(Token::Op("&".to_string())),
                just(Token::Op("^".to_string())),
            )).or_not()
        ).ignore_then(just(Token::Op("=".to_string())))
        .ignore_then(expr.clone());

        // Blocks are expressions but delimited with braces
        let block = expr.clone()
            .delimited_by(just(Token::Separator('{')), just(Token::Separator('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(nested_delimiters(
                Token::Separator('{'),
                Token::Separator('}'),
                [
                    (Token::Separator('('), Token::Separator(')')),
                    (Token::Separator('['), Token::Separator(']')),
                ],
                |span| (Expr::Error, span),
            ));

        let if_ = recursive(|if_| {
            just(Token::K(Keyword::If))
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::K(Keyword::Else))
                        .ignore_then(block.clone().or(if_))
                        .or_not(),
                )
                .map_with_span(|((cond, a), b), span| {
                    (
                        Expr::If(
                            Box::new(cond),
                            Box::new(a),
                            Box::new(match b {
                                Some(b) => b,
                                // If an `if` expression has no trailing `else` block, we magic up one that just produces null
                                None => (Expr::Value(Value::Null), span.clone()),
                            }),
                        ),
                        span,
                    )
                })
        });

        // Both blocks and `if` are 'block expressions' and can appear in the place of statements
        let block_expr = block.or(if_).labelled("block");

        let block_chain = block_expr
            .clone()
            .then(block_expr.clone().repeated())
            .foldl(|a, b| {
                let span = a.1.start..b.1.end;
                (Expr::Then(Box::new(a), Box::new(b)), span)
            });
        
        let using = just(Token::K(Keyword::Using))
            .ignore_then(ident.map_with_span(|a, span| (Expr::Local(a), span)))
            .then(
                just(Token::Op("=".to_string()))
                    .ignore_then(ident.map_with_span(|a, span| (Expr::Local(a), span))).or_not()
            ).map_with_span(|(new_type, old_type), span| {
                (Expr::Using(
                    Box::new(new_type),
                    Box::new(match old_type {
                        Some(a) => a,
                        None => (Expr::Value(Value::Null), span.clone()),
                    })
                ), span)
            });
        
        let control_flow = just(Token::K(Keyword::Break)).map_with_span(|_, span| (Expr::Break, span))
            .or(just(Token::K(Keyword::Continue)).map_with_span(|_, span| (Expr::Continue, span)));

        choice((
            definition,
            assignment,
            using,
            block_chain,
            raw_expr,
            control_flow
        )).then(just(Token::Separator(';')).ignore_then(expr.or_not()).repeated())
            .foldl(|a, b| {
                let span = a.1.clone(); // TODO: Not correct
                (
                    Expr::Then(
                        Box::new(a),
                        Box::new(match b {
                            Some(b) => b,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span,
                )
            }).labelled("Expr")
    })
}

pub enum SpanASTNode {
    Expr(Spanned<Expr>),
    Func(Spanned<String>, Func),
    Struct(Spanned<String>, Struct),
    Enum(Spanned<String>, Enum),
    Namespace(Spanned<String>, NameSpace),
    Bitfield(Spanned<String>, BitField)
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub type_: Spanned<String>,
    pub name: Spanned<String>,
    pub body: Spanned<Expr>,
}

#[derive(Debug, Clone)]
pub enum NamedASTNode {
    Expr(Declaration), // type name everything_else
    Func(Func),
    Struct(Struct),
    Enum(Enum),
    Namespace(NameSpace),
    Bitfield(BitField)
}

impl NamedASTNode {// TODO: remove this impl
    pub fn getname(&self) -> &Spanned<String> {// TODO: remove this func
        match self {
            NamedASTNode::Func(f) => &f.name,
            NamedASTNode::Struct(s) => &s.name,
            NamedASTNode::Enum(e) => &e.name,
            NamedASTNode::Namespace(n) => &n.name,
            NamedASTNode::Bitfield(b) => &b.name,
            NamedASTNode::Expr(d) => &d.name
        }
    }

    pub fn getbody(&self) -> Option<&Spanned<Expr>> { // TODO: remove this func
        match self {
            NamedASTNode::Func(f) => Some(&f.body),
            NamedASTNode::Struct(s) => Some(&s.body),
            NamedASTNode::Enum(e) => Some(&e.body),
            NamedASTNode::Namespace(n) => Some(&n.body),
            NamedASTNode::Bitfield(b) => Some(&b.body),
            NamedASTNode::Expr(d) => Some(&d.body),
        }
    }
}

#[derive(Debug)]
pub enum NormalASTNode {
    Expr(Spanned<Expr>)
}

fn register_defined_names(named_nodes: &mut HashMap<String, NamedASTNode>, e: &Expr) -> Result<(), Simple<Token>> {
    match e {
        Expr::Error => Ok(()),
        Expr::Value(_) => Ok(()),
        Expr::Local(_) => Ok(()),
        Expr::Then(e1, e2) => {
            match register_defined_names(named_nodes, &e1.0) {
                Ok(_) => register_defined_names(named_nodes, &e2.0),
                Err(e) => Err(e),
            }
        },
        Expr::Binary(e1, _, e2) => match register_defined_names(named_nodes, &e1.0) {
            Ok(_) => register_defined_names(named_nodes, &e2.0),
            Err(e) => Err(e),
        },
        Expr::Call(e1, e2) => match register_defined_names(named_nodes, &e1.0) {
            Ok(_) => {
                for e in &e2.0 {
                    match register_defined_names(named_nodes, &e.0) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    };    
                };
                Ok(())
            },
            Err(e) => Err(e),
        },
        Expr::If(e1, e2, e3) => match register_defined_names(named_nodes, &e1.0) {
            Ok(_) => match register_defined_names(named_nodes, &e2.0) {
                Ok(_) => register_defined_names(named_nodes, &e3.0),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        Expr::Definition(type_, (name, name_span), body) => {
            let expr = NamedASTNode::Expr(Declaration {
                type_: type_.clone(),
                name: (name.clone(), name_span.clone()),
                body: *body.clone(),
            });
            if named_nodes.insert(name.clone(), expr).is_some() {
                return Err(Simple::custom(
                    name_span.clone(),
                    format!("Variable '{}' already exists", name),
                ));
            }
            register_defined_names(named_nodes, &body.0)
        },
        Expr::BitFieldEntry(_, _, _) => Ok(()), // This should never happen
        Expr::EnumEntry(_, _, _) => Ok(()), // This should never happen
        Expr::MemberAccess(e, _) => register_defined_names(named_nodes, &e.0),
        Expr::ArrayAccess(e1, e2) => match register_defined_names(named_nodes, &e1.0) {
            Ok(_) => register_defined_names(named_nodes, &e2.0),
            Err(e) => Err(e),
        },
        Expr::Ternary(e1, e2, e3) => match register_defined_names(named_nodes, &e1.0) {
            Ok(_) => match register_defined_names(named_nodes, &e2.0) {
                Ok(_) => register_defined_names(named_nodes, &e3.0),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        Expr::NamespaceAccess(e, _) => register_defined_names(named_nodes, &e.0),
        Expr::Dollar => Ok(()),
        Expr::Unary(_, e) => register_defined_names(named_nodes, &e.0),
        Expr::Using(e1, e2) => match register_defined_names(named_nodes, &e1.0) {
            Ok(_) => register_defined_names(named_nodes, &e2.0),
            Err(e) => Err(e),
        },
        Expr::Continue => Ok(()),
        Expr::Break => Ok(()),
    }
}

// Hashmap is for named nodes: Structs, namespaces, funcs, etc.
// Vec is for normal nodes: Expressions, whiles, fors, etc.
pub fn parser() -> impl Parser<Token, (HashMap<String, NamedASTNode>, Vec<NormalASTNode>), Error = Simple<Token>> + Clone {
    choice((
        funcs_parser(),
        struct_parser(),
        enum_parser(),
        namespace_parser(),
        bitfield_parser(),
        expr_parser().map(|expr| SpanASTNode::Expr(expr)),
    )).repeated()
        .try_map(|nodes, _| {
            let mut named_nodes = HashMap::new();
            let mut normal_nodes = Vec::new();
            for node in nodes {
                match node {
                    SpanASTNode::Expr((e, span)) => {
                        normal_nodes.push(NormalASTNode::Expr((e.clone(), span.clone())));
                        match register_defined_names(&mut named_nodes, &e) {
                            Ok(_) => (),
                            Err(e) => return Err(e),
                        };
                    },
                    SpanASTNode::Func((name, name_span), f) => {
                        let f = NamedASTNode::Func(f);
                        if named_nodes.insert(name.clone(), f).is_some() {
                            return Err(Simple::custom(
                                name_span.clone(),
                                format!("Function '{}' already exists", name),
                            ));
                        }
                    },
                    SpanASTNode::Struct((name, name_span), s) => {
                        let s = NamedASTNode::Struct(s);
                        if named_nodes.insert(name.clone(), s).is_some() {
                            return Err(Simple::custom(
                                name_span.clone(),
                                format!("Struct '{}' already exists", name),
                            ));
                        }
                    },
                    SpanASTNode::Enum((name, name_span), e) => {
                        let e = NamedASTNode::Enum(e);
                        if named_nodes.insert(name.clone(), e).is_some() {
                            return Err(Simple::custom(
                                name_span.clone(),
                                format!("Enum '{}' already exists", name),
                            ));
                        }
                    },
                    SpanASTNode::Namespace((name, name_span), n) => {
                        let n = NamedASTNode::Namespace(n);
                        if named_nodes.insert(name.clone(), n).is_some() {
                            return Err(Simple::custom(
                                name_span.clone(),
                                format!("Namespace '{}' already exists", name),
                            ));
                        }
                    },
                    SpanASTNode::Bitfield((name, name_span), b) => {
                        let b = NamedASTNode::Bitfield(b);
                        if named_nodes.insert(name.clone(), b).is_some() {
                            return Err(Simple::custom(
                                name_span.clone(),
                                format!("Bitfield '{}' already exists", name),
                            ));
                        }
                    },
                }
            }
            Ok((named_nodes, normal_nodes))
        })
        .then_ignore(end())
}
