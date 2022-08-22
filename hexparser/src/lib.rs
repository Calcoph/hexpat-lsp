use chumsky::Parser;
use chumsky::{prelude::*, stream::Stream};
use std::collections::HashMap;
use tower_lsp::lsp_types::SemanticTokenType;

use parserlib::LEGEND_TYPE;

use self::m_lexer::{Token, Keyword, BuiltFunc};
use self::m_parser::{parser, NormalASTNode, NamedASTNode};
pub use self::m_parser::{Spanned, Expr, Value};
pub mod m_lexer;
pub mod m_parser;
//pub mod cpp_parser;

use m_lexer::lexer;


pub type Span = std::ops::Range<usize>;
#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
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

pub fn type_inference(expr: &Spanned<Expr>, symbol_type_table: &mut HashMap<Span, Value>) {
    match &expr.0 {
        Expr::Error => {}
        Expr::Value(_) => {}
        Expr::Local(_) => {}
        Expr::Then(first, second) => {
            type_inference(first, symbol_type_table);
            type_inference(second, symbol_type_table);
        }
        Expr::Binary(_, _, _) => {}
        Expr::Call(_, _) => {}
        Expr::If(_test, consequent, alternative) => {
            type_inference(consequent, symbol_type_table);
            type_inference(alternative, symbol_type_table);
        }
        Expr::Definition(_, _, _) => (),
        Expr::BitFieldEntry(_, _, _) => (),
        Expr::EnumEntry(_, _, _) => (),
        Expr::MemberAccess(_, _) => (),
        Expr::ArrayAccess(_, _) => (),
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
    Option<(HashMap<String, NamedASTNode>, Vec<NormalASTNode>)>,
    Vec<Simple<String>>,
    Vec<ImCompleteSemanticToken>,
) {
    let mut cur_start = 0;
    let mut last_indx = 0;
    let len = src.as_bytes().len();
    let (tokens, errs) = lexer().parse_recovery(
        Stream::from_iter(len..len + 1, src.char_indices()
        .map(|(index, chr)| {
            let res = (chr, cur_start..index+1);
            cur_start += index - last_indx;
            last_indx = index;
            res
        })));

    let (ast, tokenize_errors, semantic_tokens) = if let Some(tokens) = tokens {
        // info!("Tokens = {:?}", tokens);
        let semantic_tokens = tokens
            .iter()
            .filter_map(|(token, span)| match token {
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
                Token::Char(_) => Some(ImCompleteSemanticToken {
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
                Token::Separator(_) => None,
                Token::Ident(_) => None,
                Token::K(k) => match k {
                    Keyword::Struct => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Bitfield => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Union => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Enum => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Namespace => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Fn => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::If => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Else => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::While => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::For => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Break => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Continue => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::In => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Out => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Return => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Using => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::Parent => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::This => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::LittleEndian => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    Keyword::BigEndian => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                }
                Token::V(_) => Some(ImCompleteSemanticToken { // TODO: use the ValueType
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                }),
                Token::B(b) => match b {
                    BuiltFunc::AddressOf => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                    BuiltFunc::SizeOf => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::KEYWORD)
                            .unwrap(),
                    }),
                },
            })
            .collect::<Vec<_>>();
        let len = src.chars().count();
        let (ast, parse_errs) = parser()
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

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
