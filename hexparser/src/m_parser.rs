use std::{collections::HashMap, ops::Range};

use chumsky::{prelude::*,Parser};
use serde::{Deserialize, Serialize};

use super::{m_lexer::{Token, Keyword}, Span};

pub type Spanned<T> = (T, Span);

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
    ExprList(Box<Vec<Spanned<Self>>>),
    Local(Spanned<String>),
    Unary(UnaryOp, Box<Spanned<Self>>), // something
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>), // something operator something_else
    Ternary(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>), // something ? something_else : something_else_else
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>), // name arguments
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>), // if condition body
    Definition(Spanned<String>, Spanned<String>, Box<Spanned<Self>>), // type name everything_else
    BitFieldEntry(Spanned<String>, Box<Spanned<Self>>, Box<Spanned<Self>>), // name length next_entry
    EnumEntry(Spanned<String>, Box<Spanned<Self>>, Box<Spanned<Self>>), // name value next_entry
    MemberAccess(Box<Spanned<Self>>, Spanned<String>),
    ArrayAccess(Box<Spanned<Self>>, Box<Spanned<Self>>), // name value
    NamespaceAccess(Box<Spanned<Self>>, Spanned<String>),
    Using(Box<Spanned<Self>>),
    Return(Box<Spanned<Self>>),
    Continue,
    Break,
    Func(Spanned<String>, Vec<(Spanned<String>, Spanned<String>)>, Box<Spanned<Self>>), // name args body
    Struct(Spanned<String>, Box<Spanned<Self>>), // name body
    Namespace(Spanned<String>, Box<Spanned<Self>>), // name body
    Enum(Spanned<String>, Box<Spanned<Self>>, Spanned<String>), // name body type
    Bitfield(Spanned<String>, Box<Spanned<Self>>), // name body
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

fn enum_entries_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span: Span, tok| match tok {
        Token::Ident(ident) => Ok((ident.clone(), span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let val = filter_map(|span, tok| match tok {
        Token::Num(_) => Ok((Expr::Value(Value::Num(42342.0)), span)),// TODO: change 42342.0 to a proper (hex, bin, oct, dec) str->f64
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });
    let entry = ident.clone()
        .then(
            just(Token::Op("=".to_string()))
            .ignore_then(val)
            .or_not()
        );

    entry.clone()
        .map_with_span(|(name, val), span| {
            (
                Expr::EnumEntry(
                    name,
                    Box::new(match val {
                        Some(v) => v,
                        None => (Expr::Value(Value::Null), span.clone()),
                    }),
                    Box::new((Expr::Value(Value::Null), span.clone()))
                ),
                span
            )
        })
        .then(
            just(Token::Separator(','))
                .ignore_then(entry)
                .map_with_span(|a, span| (a, span))
                .repeated()
                .then_ignore(just(Token::Separator(',')).or_not())
        )
        .foldl(|(a, a_span), ((b_name, b_val), b_span)| {
            let span = a_span.start..b_span.end;
            match a {
                Expr::EnumEntry(name, val, last) => {
                    let next = Box::new((
                        Expr::EnumEntry(
                            b_name,
                            Box::new(match b_val {
                                Some(v) => v,
                                None => (Expr::Value(Value::Null), b_span.clone())
                            }),
                            last
                        ),
                        b_span
                    ));
                    (Expr::EnumEntry(name, val, next), span)
                },
                _ => unreachable!()
            }
        })
}

fn bitfield_entries_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok((ident.clone(), span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let val = filter_map(|span, tok| match tok {
        Token::Num(_) => Ok((Expr::Value(Value::Num(42342.0)), span)),// TODO: change 42342.0 to a proper (hex, bin, oct, dec) str->f64
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });
    recursive(|entry| {
        ident
            .then_ignore(just(Token::Op(":".to_string())))
            .then(val)
            .then_ignore(just(Token::Separator(';')))
            .then(entry.or_not())
            .map_with_span(|((name, value), next), span| {
                (
                    Expr::BitFieldEntry(
                        name,
                        Box::new(value),
                        Box::new(match next {
                            Some(b) => b,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span,
                )
            })
    }).then(ident
                .then_ignore(just(Token::Op(":".to_string())))
                .then(val)
                .or_not()
                .map_with_span(|a, span| {
                    match a {
                        Some((name, val)) => (Some(Expr::BitFieldEntry(
                            name,
                            Box::new(val),
                            Box::new((Expr::Value(Value::Null), span.clone())))),
                            span
                        ),
                        None => (None, span),
                    }
                })
    ).map(|(a, b)| {
        if let (Expr::BitFieldEntry(name, val, next), span) = a {
            match b {
                (Some(c), spanb) => (Expr::BitFieldEntry(
                    name,
                    val,
                    Box::new((c, spanb))
                ), span),
                (None, span) => (Expr::BitFieldEntry(name, val, next), span)
            }
        } else {
            a
        }
    })
}

fn ident() -> impl Parser<Token, Spanned<String>, Error = Simple<Token>> + Clone {
    filter_map(|span: Range<usize>, tok| match tok {
        Token::Ident(ident) => Ok((ident.clone(), span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
}

fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr: Recursive<Token, Spanned<Expr>, Simple<Token>>| {
        let val = filter_map(|span, tok| match tok {
            Token::Bool(x) => Ok(Expr::Value(Value::Bool(x))),
            Token::Num(_) => Ok(Expr::Value(Value::Num(42342.0))),// TODO: change 42342.0 to a proper (hex, bin, oct, dec) str->f64
            Token::Str(s) => Ok(Expr::Value(Value::Str(s))),
            Token::Char(c) => Ok(Expr::Value(Value::Char(c))),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        })
        .labelled("value");

        let ident = ident();

        let builtin_func = filter_map(|span, tok| match tok {
            Token::B(func) => Ok((func.clone(), span)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        })
        .labelled("built in function");

        // A list of expressions
        let items = expr.clone()
            .chain(just(Token::Separator(',')).ignore_then(expr.clone()).repeated())
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
                let span = a.1.start..b.1.end;
                (Expr::MemberAccess(Box::new(a), b), span)
            });

        let namespace_access = ident.clone()
            .map_with_span(|a, span| (Expr::Local(a), span))
            .then(
                just(Token::Op("::".to_string()))
                .ignore_then(ident.clone())
                .repeated()
                .at_least(1)
            ).foldl(|a, b| {
                let span = a.1.start..b.1.end;
                (Expr::NamespaceAccess(Box::new(a), b), span)
            });

        let array_access = ident.clone()
            .map_with_span(|a, span| (Expr::Local(a), span))
            .then(
                just(Token::Separator('['))
                .ignore_then(expr.clone())
                .then_ignore(just(Token::Separator(']')))
            ).map(|(a, b)| {
                let span = a.1.start..b.1.end;
                (Expr::ArrayAccess(Box::new(a), Box::new(b)), span)
            });

        // 'Atoms' are expressions that contain no ambiguity
        let atom = choice((
            val.map_with_span(|expr, span| (expr, span)),
            array_access.clone(),
            namespace_access.clone(),
            member_access.clone(),
            ident.clone().map_with_span(|a, span| (Expr::Local(a), span)),
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

        let raw_expr = l_or.clone()
            .then(
                just(Token::Op("?".to_string()))
                    .ignore_then(l_or.clone())
                    .then_ignore(just(Token::Op(":".to_string())))
                    .then(l_or)
                    .repeated()
            ).foldl(|a, (b, c)| {
                let span = a.1.start..c.1.end;
                (Expr::Ternary(Box::new(a), Box::new(b), Box::new(c)), span)
            });

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
                .then(ident.clone())
                .then_ignore(
                    array_definition.clone()
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
        
        let assignable = choice((
            just(Token::Op("$".to_string()))
                .map_with_span(|_, span| (Expr::Dollar, span)),
            array_access.clone(),
            namespace_access.clone(),
            member_access.clone(),
            ident.clone()
                .map_with_span(|a, span| (Expr::Local(a), span))
        ));
        
        let assignment = assignable.then(
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
            .or(just(Token::Separator('{')).ignore_then(just(Token::Separator('}'))).ignored().map_with_span(|_, span| (Expr::Value(Value::Null), span)))
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
            .map(|(a, b)| {
                let span = if b.len() > 0 {
                    a.1.start..b.get(b.len()-1).unwrap().1.end
                } else {
                    a.1.clone()
                };

                let mut a = vec![a];
                a.extend(b);
                (Expr::ExprList(Box::new(a)), span)
            });
        
        let using = just(Token::K(Keyword::Using))
            .ignore_then(ident.clone())
            .then(
                just(Token::Op("=".to_string()))
                    .ignore_then(ident.clone()).or_not()
            ).map_with_span(|(new_type, old_type), span| {
                let new_type = match old_type {
                    Some(s) => (Expr::Definition(new_type.clone(), s, Box::new((Expr::Value(Value::Null), span.clone()))), new_type.1),
                    None => (Expr::Definition(new_type.clone(), new_type.clone(), Box::new((Expr::Value(Value::Null), span.clone()))), new_type.1),
                };
                (
                    Expr::Using(
                        Box::new(new_type)
                    ),
                    span
                )
            });
        
        let control_flow = just(Token::K(Keyword::Break)).map_with_span(|_, span| (Expr::Break, span))
            .or(just(Token::K(Keyword::Continue)).map_with_span(|_, span| (Expr::Continue, span)));

        let arg_definition = ident.clone()
            .then(ident.clone())
            .or(
                just(Token::Ident("auto".to_string())).map_with_span(|_, span| ("auto".to_string(), span))
                    .then_ignore(just(Token::Separator('.')))
                    .then_ignore(just(Token::Separator('.')))
                    .then_ignore(just(Token::Separator('.')))
                    .then(ident.clone())
            );

        // Argument lists are just identifiers separated by commas, surrounded by parentheses
        let args = arg_definition
            .separated_by(just(Token::Separator(',')))
            .allow_trailing()
            .delimited_by(just(Token::Separator('(')), just(Token::Separator(')')))
            .labelled("function args");

        let func_definition_alternative = ident.clone()
            .then(ident.clone())
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
            .then(just(Token::Op("@".to_string())).or(just(Token::Op("=".to_string()))).ignore_then(raw_expr.clone()).or_not())
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

        let func_body = recursive(|func_body| {
            choice((
                func_definition_alternative,
                expr.clone(),
                just(Token::K(Keyword::Return)).ignore_then(expr.clone()).map_with_span(|e, span| (Expr::Return(Box::new(e)), span)),
            )).then(just(Token::Separator(';')).ignore_then(func_body.or_not()).repeated())
            .map(|(a, b)| {
                let span = a.1.clone(); // TODO: Not correct
                ((vec![a], span), b)
            }).foldl(|(mut a, span), b| {
                match b {
                    Some(b) => match b {
                        (Expr::ExprList(b), _) => a.extend((*b).into_iter()),
                        _ => unreachable!()
                    },
                    None => ()
                }
                (a, span)
            }).map(|(a, span)| (Expr::ExprList(Box::new(a)), span))
        });

        let func = just(Token::K(Keyword::Fn))
            .ignore_then(
                ident.clone()
                    .labelled("function name"),
            )
            .then(args)
            .then(
                func_body
                    .delimited_by(just(Token::Separator('{')), just(Token::Separator('}')))
                    .or(just(Token::Separator('{')).ignore_then(just(Token::Separator('}'))).ignored().map_with_span(|_, span| (Expr::Value(Value::Null), span)))
                    // Attempt to recover anything that looks like a function body but contains errors
                    .recover_with(nested_delimiters(
                        Token::Separator('{'),
                        Token::Separator('}'),
                        [
                            (Token::Separator('('), Token::Separator(')')),
                            (Token::Separator('['), Token::Separator(']')),
                        ],
                        |span| (Expr::Error, span),
                    )),
            ).map_with_span(|((name, args), body), span| {
                (
                    Expr::Func(
                        name,
                        args,
                        Box::new(body),
                    ),
                    span
                )
            }).labelled("function");
        
        let strct = just(Token::K(Keyword::Struct))
            .ignore_then(
                ident.clone()
                    .labelled("struct name"),
            )
            .then(
                expr.clone()
                    .delimited_by(just(Token::Separator('{')), just(Token::Separator('}')))
                    .or(just(Token::Separator('{')).ignore_then(just(Token::Separator('}'))).ignored().map_with_span(|_, span| (Expr::Value(Value::Null), span)))
                    // Attempt to recover anything that looks like a function body but contains errors
                    .recover_with(nested_delimiters(
                        Token::Separator('{'),
                        Token::Separator('}'),
                        [
                            (Token::Separator('('), Token::Separator(')')),
                            (Token::Separator('['), Token::Separator(']')),
                        ],
                        |span| (Expr::Error, span),
                    )),
            ).map_with_span(|(name, body), span| {
                (
                    Expr::Struct(
                        name.clone(),
                        Box::new(body),
                    ),
                    span
                )
            }).labelled("struct");

        let namespace_name = ident.clone() // TODO: Rework this parser
            .then(
                just(Token::Op("::".to_string()))
                .ignore_then(ident.clone())
                .repeated()
            ).foldl(|a, b| {
                (a.0 + "::" + &b.0, a.1.start..b.1.end)
            });
        
        let nspace = just(Token::K(Keyword::Namespace))
            .ignore_then(namespace_name.labelled("namespace name"))
            .then(
                expr.clone()
                    .delimited_by(just(Token::Separator('{')), just(Token::Separator('}')))
                    .or(just(Token::Separator('{')).ignore_then(just(Token::Separator('}'))).ignored().map_with_span(|_, span| (Expr::Value(Value::Null), span)))
                    // Attempt to recover anything that looks like a function body but contains errors
                    .recover_with(nested_delimiters(
                        Token::Separator('{'),
                        Token::Separator('}'),
                        [
                            (Token::Separator('('), Token::Separator(')')),
                            (Token::Separator('['), Token::Separator(']')),
                        ],
                        |span| (Expr::Error, span),
                    )),
            ).map_with_span(|(name, body), span| {
                (
                    Expr::Namespace(
                        name.clone(),
                        Box::new(body),
                    ),
                    span
                )
            });
        
        let enm = enum_();

        let bitfield = bitfield_parser();

        let semicolon_expr = choice((
            definition,
            assignment,
            using,
            raw_expr,
            control_flow,
            func,
            strct,
            enm,
            bitfield
        ));

        let not_semicolon_expr = choice((
            block_chain,
            nspace
        ));

        semicolon_expr
            .then(just(Token::Separator(';')).ignore_then(expr.clone().or_not()).repeated())
            .or(
                not_semicolon_expr
                    .then(
                        expr.clone()
                            .map(|a| Some(a))
                            .or(just(Token::Separator(';')).ignore_then(expr.clone().or_not()))
                            .repeated()
                    )
            ).map(|(a, b)| {
                let span = a.1.clone(); // TODO: Not correct
                ((vec![a], span), b)
            }).foldl(|(mut a, span), b| {
                match b {
                    Some(b) => match b {
                        (Expr::ExprList(b), _) => a.extend((*b).into_iter()),
                        _ => unreachable!()
                    },
                    None => ()
                }
                (a, span)
            }).map(|(a, span)| (Expr::ExprList(Box::new(a)), span))
    })
}

fn enum_() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    let ident = ident();
    just(Token::K(Keyword::Enum))
        .ignore_then(ident.clone())
        .then_ignore(just(Token::Op(":".to_string())))
        .then(ident)
        .then(
            enum_entries_parser()
                .delimited_by(just(Token::Separator('{')), just(Token::Separator('}')))
                .or(just(Token::Separator('{')).ignore_then(just(Token::Separator('}'))).ignored().map_with_span(|_, span| (Expr::Value(Value::Null), span)))
                // Attempt to recover anything that looks like a function body but contains errors
                .recover_with(nested_delimiters(
                    Token::Separator('{'),
                    Token::Separator('}'),
                    [
                        (Token::Separator('('), Token::Separator(')')),
                        (Token::Separator('['), Token::Separator(']')),
                    ],
                    |span| (Expr::Error, span),
                )),
        )
        .map_with_span(|((name, type_), body), span| {
            (
                Expr::Enum(
                    name.clone(),
                    Box::new(body),
                    type_,
                ),
                span
            )
        })
}

fn bitfield_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    just(Token::K(Keyword::Bitfield))
        .ignore_then(ident())
        .then(
            bitfield_entries_parser()
                .delimited_by(just(Token::Separator('{')), just(Token::Separator('}')))
                .or(just(Token::Separator('{')).ignore_then(just(Token::Separator('}'))).ignored().map_with_span(|_, span| (Expr::Value(Value::Null), span)))
                // Attempt to recover anything that looks like a function body but contains errors
                .recover_with(nested_delimiters(
                    Token::Separator('{'),
                    Token::Separator('}'),
                    [
                        (Token::Separator('('), Token::Separator(')')),
                        (Token::Separator('['), Token::Separator(']')),
                    ],
                    |span| (Expr::Error, span),
                )),
        ).map_with_span(|(name, body), span| {
            (
                Expr::Bitfield(
                    name.clone(),
                    Box::new(body),
                ),
                span
            )
        })
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub type_: Spanned<String>,
    pub name: Spanned<String>,
    pub body: Spanned<Expr>,
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

fn register_defined_names(named_nodes: &mut HashMap<String, Spanned<NamedNode>>, e: &Expr) -> Result<(), Simple<Token>> {
    match e {
        Expr::Error => Ok(()),
        Expr::Value(_) => Ok(()),
        Expr::Local(_) => Ok(()),
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
        Expr::Definition(_, (name, name_span), body) => {
            if named_nodes.insert(name.clone(), (NamedNode::Variable, name_span.clone())).is_some() {
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
        Expr::Using(e) => register_defined_names(named_nodes, &e.0),
        Expr::Continue => Ok(()),
        Expr::Break => Ok(()),
        Expr::ExprList(box_) => {
            for expr in box_.as_ref() {
                match register_defined_names(named_nodes, &expr.0) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            };
            Ok(())
        },
        Expr::Func(_, _, _) => Ok(()), // TODO
        Expr::Struct(_, _) => Ok(()), // TODO
        Expr::Namespace(_, _) => Ok(()), // TODO
        Expr::Enum(_, _, _) => Ok(()), // TODO
        Expr::Bitfield(_, _) => Ok(()), // TODO
        Expr::Return(_) => Ok(()), // TODO
    }
}

// Hashmap contains the names of named expressions and their clones
pub fn parser() -> impl Parser<Token, (HashMap<String, Spanned<NamedNode>>, Spanned<Expr>), Error = Simple<Token>> + Clone {
    expr_parser()
        .try_map(|expr, _| {
            let mut named_exprs = HashMap::new();
            match register_defined_names(&mut named_exprs, &expr.0) {
                Ok(_) => (),
                Err(e) => return Err(e),
            };
            Ok((named_exprs, expr))
        })
        .then_ignore(end())
}
