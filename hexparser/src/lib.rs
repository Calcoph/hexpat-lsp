use std::cell::RefCell;
use std::{collections::HashMap, ops::Range};
use std::path::Path;
use tower_lsp::lsp_types::SemanticTokenType;

use parserlib::LEGEND_TYPE;

use crate::m_parser::NamedNode;
use crate::token::{Token, Keyword, BuiltFunc};
use crate::{m_lexer::lex, recovery_err::RecoveredError, m_parser::token_parse, token::Spanned};

pub use self::m_parser::{Expr, Value};
mod m_lexer;
pub mod m_parser;
pub mod recovery_err;
pub mod token;
mod combinators;


#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}

pub fn type_inference(expr: &Spanned<Expr>, symbol_type_table: &mut HashMap<Range<usize>, Value>) {
    match &expr.0 {
        Expr::Error => {}
        Expr::Value {..} => {}
        Expr::Local {..} => {}
        Expr::Binary {..} => {}
        Expr::Call {..} => {}
        Expr::If { test: test_, consequent, alternative } => {
            type_inference(consequent, symbol_type_table);
            type_inference(alternative, symbol_type_table);
        },
        Expr::Definition {..} => (), // TODO
        Expr::BitFieldEntry {..} => (), // TODO
        Expr::EnumEntry {..} => (), // TODO
        Expr::Ternary {..} => (), // TODO
        Expr::NamespaceAccess {..} => (), // TODO
        Expr::Dollar => (), // TODO
        Expr::Unary {..} => (), // TODO
        Expr::Using {..} => (), // TODO
        Expr::Continue => (), // TODO
        Expr::Break => (), // TODO
        Expr::ExprList {..} => (), // TODO
        Expr::Func {..} => (), // TODO
        Expr::Struct {..} => (), // TODO
        Expr::Namespace {..} => (), // TODO
        Expr::Enum {..} => (), // TODO
        Expr::Bitfield {..} => (), // TODO
        Expr::Return {..} => (), // TODO
        Expr::Access {..} => (), // TODO
        Expr::Attribute {..} => (), // TODO
        Expr::AttributeArgument {..} => (), // TODO
        Expr::WhileLoop { .. } => (), // TODO
        Expr::ForLoop { .. } => (), // TODO
        Expr::UnnamedParameter { .. } => (), // TODO
        Expr::Cast { .. } => (), // TODO
        Expr::Union { .. } => (), // TODO
    }
}

pub fn parse(
    src: &str,
    includeable_folders: &Vec<String>
) -> (
    (HashMap<String, Spanned<NamedNode>>, Spanned<Expr>),
    Vec<RecoveredError>,
    Vec<ImCompleteSemanticToken>,
) {
    let errs = RefCell::new(Vec::new());
    let tokens = lex(src, &errs);
    //let (tokens, errors) = expand_preprocessor_tokens(tokens, includeable_folders);

    //errs.borrow_mut().extend(errors.into_iter());

    let (ast, semantic_tokens) = { // TODO: Do it without a code block
        // info!("Tokens = {:?}", tokens);
        let semantic_tokens = tokens
            .iter()
            .map(|tok| {
                let off = tok.location_offset();
                (tok.fragment(), off..off+tok.extra.1)
            })
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
                Token::Pre(_) => None,
                Token::Comment(_) => None,
                Token::Err => None,
            })
            .collect::<Vec<_>>();
        let ast = token_parse(tokens.into_iter()
            .filter(|tok| match tok.fragment() {
                Token::Comment(_) => false,
                _ => true
            }
        ).collect());

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

        (ast, semantic_tokens)
    };

    (ast, errs.into_inner(), semantic_tokens)
    // .for_each(|e| {
    //     let report = match e.reason() {
    //         chumsky::error::SimpleReason::Unclosed { span, delimiter } => {}
    //         chumsky::error::SimpleReason::Unexpected => {}
    //         chumsky::error::SimpleReason::Custom(msg) => {}
    //     };
    // });
}

/* fn expand_preprocessor_tokens(tokens: Vec<(Token, std::ops::Range<usize>)>, includeable_folders: &Vec<String>) -> (Vec<(Token, std::ops::Range<usize>)>, Vec<RecoveredError>) {
    let mut errors = vec![];
    let mut defines = HashMap::new();
    let v = expand_preprocessor_tokens_recursive(tokens, &mut errors, &mut defines, includeable_folders);
    (v, errors)
}

fn expand_preprocessor_tokens_recursive(
        tokens: Vec<(Token, std::ops::Range<usize>)>,
        errors: &mut Vec<RecoveredError>,
        defines: &mut HashMap<String, Vec<Token>>,
        includeable_folders: &Vec<String>
    ) -> Vec<(Token, std::ops::Range<usize>)> {
    let mut v = vec![];

    for token in tokens.into_iter() {
        match token.0 {
            Token::Ident(i) => {
                v.extend(try_define(i, &defines).into_iter().map(|tok| (tok, token.1.clone())))
            },
            Token::Pre(p) => match p {
                m_lexer::PreProc::Include(i) => {
                    let (tokens, errs) = add_include((i, token.1.clone()), includeable_folders);
                    match tokens {
                        Some(tokens) => {
                            let span = token.1.clone();
                            let res = expand_preprocessor_tokens_recursive(tokens, errors, defines, includeable_folders);
                            v.extend(res.into_iter().map(|tok| (tok.0, span.clone())))
                        },
                        None => (),
                    };
                    errors.extend(errs.into_iter());
                },
                m_lexer::PreProc::Define(d) => errors.extend(add_define(defines, (d, token.1)).into_iter()),
                m_lexer::PreProc::Pragma(_) => (),
            },
            _ => v.push(token),
        }
    };

    v
}

fn try_define(i: String, defines: &HashMap<String, Vec<Token>>) -> Vec<Token> {
    match defines.get(&i) {
        Some(v) => v.clone(),
        None => vec![Token::Ident(i)],
    }
}

fn add_include(i: Spanned<String>, includeable_folders: &Vec<String>) -> (Option<Vec<Spanned<Token>>>, Vec<RecoveredError>) {
    let quote_parser = just::<_, _, Simple<char>>('"').ignore_then(take_until(just('"'))).map(|(a,_)| a).collect::<String>();
    let angle_parser = just('<').ignore_then(take_until(just('>'))).map(|(a,_)| a).collect::<String>();
    let include_parser = choice((
        quote_parser,
        angle_parser
    )).padded().then_ignore(end()).map_with_span(|a, span| (a, span));

    let (res, mut errors) = include_parser.parse_recovery(i.0);

    let tokens = match res {
        Some(p) => {
            let span = p.1.clone();
            let (tokens, errs) = get_include_tokens(p, i.1.clone(), includeable_folders);
            errors.extend(errs.into_iter().map(|err| Simple::custom(span.clone(), err.to_string())));
            tokens
        },
        None => None,
    };

    (tokens, errors)
}

fn get_include_tokens(p: Spanned<String>, span: Span, includeable_folders: &Vec<String>) -> (Option<Vec<Spanned<Token>>>, Vec<Simple<char>>) {
    let mut result = (None, vec![Simple::custom(p.1, "File not found")]);
    for path in includeable_folders {
        let path = Path::new(path);
        let path = match shellexpand::full(&p.0.clone()) {
            Ok(p) => match expand_str::expand_string_with_env(p.as_ref()) {
                Ok(p) => Some(path.join("includes").join(p)),
                Err(_) => None,
            },
            Err(_) => None
        };
        if let Some(path) = path {
            if path.exists() {
                result = get_path_tokens(path, span);
                break
            }
        }
    }

    result
}

fn get_path_tokens(path: std::path::PathBuf, span: Span) -> (Option<Vec<Spanned<Token>>>, Vec<Simple<char>>) {
    let source = std::fs::read_to_string(path).unwrap();

    let (tokens, errs) = lexer().parse_recovery(source);
    let tokens = match tokens {
        Some(tokens) => Some(tokens.into_iter().map(|(a, _)| (a, span.clone())).collect()),
        None => None,
    };

    (tokens, errs)
}

fn add_define(defines: &mut HashMap<String, Vec<Token>>, new_define: Spanned<String>) -> Vec<Simple<char>> {
    let define_lexer = text::ident().then(lexer()).padded();

    let (res, errors) = define_lexer.parse_recovery(new_define.0);
    if let Some((name, tokens)) = res {
        let tokens = tokens.into_iter()
            .map(|tok| tok.0)
            .collect();
        
        defines.insert(name, tokens);
    };

    let errors = errors.into_iter()
        .map(|_| Simple::custom(new_define.1.clone(), "Define not formed correctly")).collect();

    errors
}
 */