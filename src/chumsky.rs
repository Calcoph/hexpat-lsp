use chumsky::Parser;
use chumsky::{prelude::*, stream::Stream};
use core::fmt;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, env, fs};
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

use crate::semantic_token::{self, LEGEND_TYPE};

/// This is the parser and interpreter for the 'Foo' language. See `tutorial.md` in the repository's root to learn
/// about it.
pub type Span = std::ops::Range<usize>;
#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Null,
    Bool(bool),
    Num(String),
    Str(String),
    Op(String),
    Ctrl(char),
    Ident(String),
    Fn,
    Let,
    Print,
    If,
    Else,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Null => write!(f, "null"),
            Token::Bool(x) => write!(f, "{}", x),
            Token::Num(n) => write!(f, "{}", n),
            Token::Str(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Print => write!(f, "print"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
        }
    }
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Num);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for operators
    let op = one_of("+-*/!=")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};,").map(|c| Token::Ctrl(c));

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::Fn,
        "let" => Token::Let,
        "print" => Token::Print,
        "if" => Token::If,
        "else" => Token::Else,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "null" => Token::Null,
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = num
        .or(str_)
        .or(op)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .padded_by(comment.repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    List(Vec<Value>),
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
            Self::Null => write!(f, "null"),
            Self::Bool(x) => write!(f, "{}", x),
            Self::Num(x) => write!(f, "{}", x),
            Self::Str(x) => write!(f, "{}", x),
            Self::List(xs) => write!(
                f,
                "[{}]",
                xs.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Func(name) => write!(f, "<function: {}>", name),
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
}

pub type Spanned<T> = (T, Span);

// An expression node in the AST. Children are spanned so we can generate useful runtime errors.
#[derive(Debug)]
pub enum Expr {
    Error,
    Value(Value),
    List(Vec<Spanned<Self>>),
    Local(Spanned<String>),
    Let(String, Box<Spanned<Self>>, Box<Spanned<Self>>, Span),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Print(Box<Spanned<Self>>),
}

impl Expr {
    /// Returns `true` if the expr is [`Error`].
    ///
    /// [`Error`]: Expr::Error
    fn is_error(&self) -> bool {
        matches!(self, Self::Error)
    }

    /// Returns `true` if the expr is [`Let`].
    ///
    /// [`Let`]: Expr::Let
    fn is_let(&self) -> bool {
        matches!(self, Self::Let(..))
    }

    /// Returns `true` if the expr is [`Value`].
    ///
    /// [`Value`]: Expr::Value
    fn is_value(&self) -> bool {
        matches!(self, Self::Value(..))
    }

    fn try_into_value(self) -> Result<Value, Self> {
        if let Self::Value(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    fn as_value(&self) -> Option<&Value> {
        if let Self::Value(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

// A function node in the AST.
#[derive(Debug)]
pub struct Func {
    pub args: Vec<Spanned<String>>,
    pub body: Spanned<Expr>,
    pub name: Spanned<String>,
    pub span: Span,
}

fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let raw_expr = recursive(|raw_expr| {
            let val = filter_map(|span, tok| match tok {
                Token::Null => Ok(Expr::Value(Value::Null)),
                Token::Bool(x) => Ok(Expr::Value(Value::Bool(x))),
                Token::Num(n) => Ok(Expr::Value(Value::Num(n.parse().unwrap()))),
                Token::Str(s) => Ok(Expr::Value(Value::Str(s))),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("value");

            let ident = filter_map(|span, tok| match tok {
                Token::Ident(ident) => Ok((ident.clone(), span)),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("identifier");

            // A list of expressions
            let items = expr
                .clone()
                .chain(just(Token::Ctrl(',')).ignore_then(expr.clone()).repeated())
                .then_ignore(just(Token::Ctrl(',')).or_not())
                .or_not()
                .map(|item| item.unwrap_or_else(Vec::new));

            // A let expression
            let let_ = just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Op("=".to_string())))
                .then(raw_expr)
                .then_ignore(just(Token::Ctrl(';')))
                .then(expr.clone())
                .map(|((name, val), body)| {
                    Expr::Let(name.0, Box::new(val), Box::new(body), name.1)
                });

            let list = items
                .clone()
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                .map(Expr::List);

            // 'Atoms' are expressions that contain no ambiguity
            let atom = val
                .or(ident.map(Expr::Local))
                .or(let_)
                .or(list)
                // In Nano Rust, `print` is just a keyword, just like Python 2, for simplicity
                .or(just(Token::Print)
                    .ignore_then(
                        expr.clone()
                            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                    )
                    .map(|expr| Expr::Print(Box::new(expr))))
                .map_with_span(|expr, span| (expr, span))
                // Atoms can also just be normal expressions, but surrounded with parentheses
                .or(expr
                    .clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
                // Attempt to recover anything that looks like a parenthesised expression but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [
                        (Token::Ctrl('['), Token::Ctrl(']')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                ))
                // Attempt to recover anything that looks like a list but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('['),
                    Token::Ctrl(']'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                ));

            // Function calls have very high precedence so we prioritise them
            let call = atom
                .then(
                    items
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                        .map_with_span(|args, span| (args, span))
                        .repeated(),
                )
                .foldl(|f, args| {
                    let span = f.1.start..args.1.end;
                    (Expr::Call(Box::new(f), args), span)
                });

            // Product ops (multiply and divide) have equal precedence
            let op = just(Token::Op("*".to_string()))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/".to_string())).to(BinaryOp::Div));
            let product = call
                .clone()
                .then(op.then(call).repeated())
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

            // Comparison ops (equal, not-equal) have equal precedence
            let op = just(Token::Op("==".to_string()))
                .to(BinaryOp::Eq)
                .or(just(Token::Op("!=".to_string())).to(BinaryOp::NotEq));
            let compare = sum
                .clone()
                .then(op.then(sum).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            compare
        });

        // Blocks are expressions but delimited with braces
        let block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Expr::Error, span),
            ));

        let if_ = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
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

        block_chain
            // Expressions, chained by semicolons, are statements
            .or(raw_expr.clone())
            .then(just(Token::Ctrl(';')).ignore_then(expr.or_not()).repeated())
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
            })
    })
}

pub fn funcs_parser() -> impl Parser<Token, HashMap<String, Func>, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    // Argument lists are just identifiers separated by commas, surrounded by parentheses
    let args = ident
        .map_with_span(|name, span| (name, span))
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .labelled("function args");

    let func = just(Token::Fn)
        .ignore_then(
            ident
                .map_with_span(|name, span| (name, span))
                .labelled("function name"),
        )
        .then(args)
        .then(
            expr_parser()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                // Attempt to recover anything that looks like a function body but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('{'),
                    Token::Ctrl('}'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('['), Token::Ctrl(']')),
                    ],
                    |span| (Expr::Error, span),
                )),
        )
        .map_with_span(|((name, args), body), span| {
            (
                name.clone(),
                Func {
                    args,
                    body,
                    name,
                    span,
                },
            )
        })
        .labelled("function");

    func.repeated()
        .try_map(|fs, _| {
            let mut funcs = HashMap::new();
            for ((name, name_span), f) in fs {
                if funcs.insert(name.clone(), f).is_some() {
                    return Err(Simple::custom(
                        name_span.clone(),
                        format!("Function '{}' already exists", name),
                    ));
                }
            }
            Ok(funcs)
        })
        .then_ignore(end())
}

struct Error {
    span: Span,
    msg: String,
}

pub fn type_inference(expr: &Spanned<Expr>, symbol_type_table: &mut HashMap<Span, Value>) {
    match &expr.0 {
        Expr::Error => {}
        Expr::Value(_) => {}
        Expr::List(exprs) => exprs
            .iter()
            .for_each(|expr| type_inference(expr, symbol_type_table)),
        Expr::Local(_) => {}
        Expr::Let(name, lhs, rest, name_span) => {
            if let Some(value) = lhs.0.as_value() {
                symbol_type_table.insert(name_span.clone(), value.clone());
            }
            type_inference(rest, symbol_type_table);
        }
        Expr::Then(first, second) => {
            type_inference(first, symbol_type_table);
            type_inference(second, symbol_type_table);
        }
        Expr::Binary(_, _, _) => {}
        Expr::Call(_, _) => {}
        Expr::If(test, consequent, alternative) => {
            type_inference(consequent, symbol_type_table);
            type_inference(alternative, symbol_type_table);
        }
        Expr::Print(expr) => {
            type_inference(expr, symbol_type_table);
        }
    }
}
// fn eval_expr(
//     expr: &Spanned<Expr>,
//     funcs: &HashMap<String, Func>,
//     stack: &mut Vec<(String, Value)>,
// ) -> Result<Value, Error> {
//     Ok(match &expr.0 {
//         Expr::Error => unreachable!(), // Error expressions only get created by parser errors, so cannot exist in a valid AST
//         Expr::Value(val) => val.clone(),
//         Expr::List(items) => Value::List(
//             items
//                 .iter()
//                 .map(|item| eval_expr(item, funcs, stack))
//                 .collect::<Result<_, _>>()?,
//         ),
//         Expr::Local(name) => stack
//             .iter()
//             .rev()
//             .find(|(l, _)| l == name)
//             .map(|(_, v)| v.clone())
//             .or_else(|| Some(Value::Func(name.clone())).filter(|_| funcs.contains_key(name)))
//             .ok_or_else(|| Error {
//                 span: expr.1.clone(),
//                 msg: format!("No such variable '{}' in scope", name),
//             })?,
//         Expr::Let(local, val, body) => {
//             let val = eval_expr(val, funcs, stack)?;
//             stack.push((local.clone(), val));
//             let res = eval_expr(body, funcs, stack)?;
//             stack.pop();
//             res
//         }
//         Expr::Then(a, b) => {
//             eval_expr(a, funcs, stack)?;
//             eval_expr(b, funcs, stack)?
//         }
//         Expr::Binary(a, BinaryOp::Add, b) => Value::Num(
//             eval_expr(a, funcs, stack)?.num(a.1.clone())?
//                 + eval_expr(b, funcs, stack)?.num(b.1.clone())?,
//         ),
//         Expr::Binary(a, BinaryOp::Sub, b) => Value::Num(
//             eval_expr(a, funcs, stack)?.num(a.1.clone())?
//                 - eval_expr(b, funcs, stack)?.num(b.1.clone())?,
//         ),
//         Expr::Binary(a, BinaryOp::Mul, b) => Value::Num(
//             eval_expr(a, funcs, stack)?.num(a.1.clone())?
//                 * eval_expr(b, funcs, stack)?.num(b.1.clone())?,
//         ),
//         Expr::Binary(a, BinaryOp::Div, b) => Value::Num(
//             eval_expr(a, funcs, stack)?.num(a.1.clone())?
//                 / eval_expr(b, funcs, stack)?.num(b.1.clone())?,
//         ),
//         Expr::Binary(a, BinaryOp::Eq, b) => {
//             Value::Bool(eval_expr(a, funcs, stack)? == eval_expr(b, funcs, stack)?)
//         }
//         Expr::Binary(a, BinaryOp::NotEq, b) => {
//             Value::Bool(eval_expr(a, funcs, stack)? != eval_expr(b, funcs, stack)?)
//         }
//         Expr::Call(func, (args, args_span)) => {
//             let f = eval_expr(func, funcs, stack)?;
//             match f {
//                 Value::Func(name) => {
//                     let f = &funcs[&name];
//                     let mut stack = if f.args.len() != args.len() {
//                         return Err(Error {
//                             span: args_span.clone(),
//                             msg: format!("'{}' called with wrong number of arguments (expected {}, found {})", name, f.args.len(), args.len()),
//                         });
//                     } else {
//                         f.args
//                             .iter()
//                             .zip(args.iter())
//                             .map(|(name, arg)| Ok((name.clone(), eval_expr(arg, funcs, stack)?)))
//                             .collect::<Result<_, _>>()?
//                     };fn eval_expr(
//     expr: &Spanned<Expr>,
//     funcs: &HashMap<String, Func>,
//     stack: &mut Vec<(String, Value)>,
// ) -> Result<Value, Error> {
//     Ok(match &expr.0 {
//         Expr::Error => unreachable!(), // Error expressions only get created by parser errors, so cannot exist in a valid AST
//         Expr::Value(val) => val.clone(),
//         Expr::List(items) => Value::List(
//             items
//                 .iter()
//                 .map(|item| eval_expr(item, funcs, stack))
//                 .collect::<Result<_, _>>()?,
//         ),
//         Expr::Local(name) => stack
//             .iter()
//             .rev()
//             .find(|(l, _)| l == name)
//             .map(|(_, v)| v.clone())
//             .or_else(|| Some(Value::Func(name.clone())).filter(|_| funcs.contains_key(name)))
//             .ok_or_else(|| Error {
//                 span: expr.1.clone(),
//                 msg: format!("No such variable '{}' in scope", name),
//             })?,
//         Expr::Let(local, val, body) => {
//             let val = eval_expr(val, funcs, stack)?;
//             stack.push((local.clone(), val));
//             let res = eval_expr(body, funcs, stack)?;
//             stack.pop();
//             res
//         }
//         Expr::Then(a, b) => {
//             eval_expr(a, funcs, stack)?;
//             eval_expr(b, funcs, stack)?
//         }
//         Expr::Binary(a, BinaryOp::Add, b) => Value::Num(
//             eval_expr(a, funcs, stack)?.num(a.1.clone())?
//                 + eval_expr(b, funcs, stack)?.num(b.1.clone())?,
//         ),
//         Expr::Binary(a, BinaryOp::Sub, b) => Value::Num(
//             eval_expr(a, funcs, stack)?.num(a.1.clone())?
//                 - eval_expr(b, funcs, stack)?.num(b.1.clone())?,
//         ),
//         Expr::Binary(a, BinaryOp::Mul, b) => Value::Num(
//             eval_expr(a, funcs, stack)?.num(a.1.clone())?
//                 * eval_expr(b, funcs, stack)?.num(b.1.clone())?,
//         ),
//         Expr::Binary(a, BinaryOp::Div, b) => Value::Num(
//             eval_expr(a, funcs, stack)?.num(a.1.clone())?
//                 / eval_expr(b, funcs, stack)?.num(b.1.clone())?,
//         ),
//         Expr::Binary(a, BinaryOp::Eq, b) => {
//             Value::Bool(eval_expr(a, funcs, stack)? == eval_expr(b, funcs, stack)?)
//         }
//         Expr::Binary(a, BinaryOp::NotEq, b) => {
//             Value::Bool(eval_expr(a, funcs, stack)? != eval_expr(b, funcs, stack)?)
//         }
//         Expr::Call(func, (args, args_span)) => {
//             let f = eval_expr(func, funcs, stack)?;
//             match f {
//                 Value::Func(name) => {
//                     let f = &funcs[&name];
//                     let mut stack = if f.args.len() != args.len() {
//                         return Err(Error {
//                             span: args_span.clone(),
//                             msg: format!("'{}' called with wrong number of arguments (expected {}, found {})", name, f.args.len(), args.len()),
//                         });
//                     } else {
//                         f.args
//                             .iter()
//                             .zip(args.iter())
//                             .map(|(name, arg)| Ok((name.clone(), eval_expr(arg, funcs, stack)?)))
//                             .collect::<Result<_, _>>()?
//                     };
//                     eval_expr(&f.body, funcs, &mut stack)?
//                 }
//                 f => {
//                     return Err(Error {
//                         span: func.1.clone(),
//                         msg: format!("'{:?}' is not callable", f),
//                     })
//                 }
//             }
//         }
//         Expr::If(cond, a, b) => {
//             let c = eval_expr(cond, funcs, stack)?;
//             match c {
//                 Value::Bool(true) => eval_expr(a, funcs, stack)?,
//                 Value::Bool(false) => eval_expr(b, funcs, stack)?,
//                 c => {
//                     return Err(Error {
//                         span: cond.1.clone(),
//                         msg: format!("Conditions must be booleans, found '{:?}'", c),
//                     })
//                 }
//             }
//         }
//         Expr::Print(a) => {
//             let val = eval_expr(a, funcs, stack)?;
//             println!("{}", val);
//             val
//         }
//     })
// }}
//                 f => {
//                     return Err(Error {
//                         span: func.1.clone(),
//                         msg: format!("'{:?}' is not callable", f),
//                     })
//                 }
//             }
//         }
//         Expr::If(cond, a, b) => {
//             let c = eval_expr(cond, funcs, stack)?;
//             match c {
//                 Value::Bool(true) => eval_expr(a, funcs, stack)?,
//                 Value::Bool(false) => eval_expr(b, funcs, stack)?,
//                 c => {
//                     return Err(Error {
//                         span: cond.1.clone(),
//                         msg: format!("Conditions must be booleans, found '{:?}'", c),
//                     })
//                 }
//             }
//         }
//         Expr::Print(a) => {
//             let val = eval_expr(a, funcs, stack)?;
//             println!("{}", val);
//             val
//         }
//     })
// }

pub fn parse(
    src: &str,
) -> (
    Option<HashMap<String, Func>>,
    Vec<Simple<String>>,
    Vec<ImCompleteSemanticToken>,
) {
    let (tokens, errs) = lexer().parse_recovery(src);

    let (ast, tokenize_errors, semantic_tokens) = if let Some(tokens) = tokens {
        // info!("Tokens = {:?}", tokens);
        let semantic_tokens = tokens
            .iter()
            .filter_map(|(token, span)| match token {
                Token::Null => None,
                Token::Bool(_) => None,

                Token::Num(_) => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::NUMBER)
                        .unwrap(),
                }),
                Token::Str(_) => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::STRING)
                        .unwrap(),
                }),
                Token::Op(_) => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::OPERATOR)
                        .unwrap(),
                }),
                Token::Ctrl(_) => None,
                Token::Ident(_) => None,
                Token::Fn => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                }),
                Token::Let => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                }),
                Token::Print => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::FUNCTION)
                        .unwrap(),
                }),
                Token::If => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                }),
                Token::Else => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                }),
            })
            .collect::<Vec<_>>();
        let len = src.chars().count();
        let (ast, parse_errs) =
            funcs_parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        // println!("{:#?}", ast);
        // if let Some(funcs) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
        //     if let Some(main) = funcs.get("main") {
        //         assert_eq!(main.args.len(), 0);
        //         match eval_expr(&main.body, &funcs, &mut Vec::new()) {
        //             Ok(val) => println!("Return value: {}", val),
        //             Err(e) => errs.push(Simple::custom(e.span, e.msg)),
        //         }
        //     } else {
        //         panic!("No main function!");
        //     }
        // }

        (ast, parse_errs, semantic_tokens)
    } else {
        (None, Vec::new(), vec![])
    };

    let parse_errors = errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(
            tokenize_errors
                .into_iter()
                .map(|e| e.map(|tok| tok.to_string())),
        )
        .collect::<Vec<_>>();

    (ast, parse_errors, semantic_tokens)
    // .for_each(|e| {
    //     let report = match e.reason() {
    //         chumsky::error::SimpleReason::Unclosed { span, delimiter } => {}
    //         chumsky::error::SimpleReason::Unexpected => {}
    //         chumsky::error::SimpleReason::Custom(msg) => {}
    //     };
    // });
}
