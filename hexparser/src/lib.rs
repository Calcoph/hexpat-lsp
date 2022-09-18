use std::cell::RefCell;
use std::{collections::HashMap, ops::Range};
use std::path::Path;
use nom::IResult;
use nom::bytes::complete::{tag as just, take_until};
use nom::character::complete::{multispace1, alpha1, alphanumeric1, not_line_ending};
use nom::combinator::{map, eof, recognize};
use nom::branch::alt as choice;
use nom::multi::many0;
use nom::sequence::{pair as then, preceded, terminated, delimited};
use tower_lsp::lsp_types::SemanticTokenType;

use parserlib::LEGEND_TYPE;

use crate::m_lexer::lex_tokens;
use crate::m_parser::NamedNode;
use crate::recovery_err::{ToRange, ParseState};
use crate::token::{Token, Keyword, BuiltFunc, TokSpan, FromStrSpan};
use crate::{m_lexer::lex, recovery_err::RecoveredError, m_parser::token_parse, token::Spanned};

pub use self::m_parser::{Expr, Value};
mod m_lexer;
pub mod m_parser;
pub mod recovery_err;
pub mod token;
mod combinators;
mod simple_debug;

#[cfg(test)]
mod tests;


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
        Expr::If { .. } => (), // TODO
        Expr::IfBlock { .. } => (), // TODO
        Expr::Definition {..} => (), // TODO
        Expr::BitFieldEntry {..} => (), // TODO
        Expr::EnumEntry {..} => (), // TODO
        Expr::Ternary {..} => (), // TODO
        Expr::NamespaceAccess {..} => (), // TODO
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
    let included_files = RefCell::new(vec![]); // This vec is only so references are valid
    let defines = RefCell::new(HashMap::new());
    let tokens = expand_preprocessor_tokens(tokens, &defines, includeable_folders, &included_files, &errs);

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
                Token::Op(op) => match *op {
                    "$" => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::new("dollar"))
                            .unwrap(),
                    }),
                    "::" => None,
                    _ => Some(ImCompleteSemanticToken {
                        start: span.start,
                        length: span.len(),
                        token_type: LEGEND_TYPE
                            .iter()
                            .position(|item| item == &SemanticTokenType::OPERATOR)
                            .unwrap(),
                    })
                },
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
                Token::V(_) => Some(ImCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::TYPE)
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

        (ast, semantic_tokens)
    };

    (ast, errs.into_inner(), semantic_tokens)
}

fn expand_preprocessor_tokens<'a, 'b>(
        tokens: Vec<TokSpan<'a, 'b>>,
        defines: &'a RefCell<HashMap<String, Vec<Token<'a>>>>,
        includeable_folders: &Vec<String>,
        included_files: &'a RefCell<Vec<String>>,
        errors: &'b RefCell<Vec<RecoveredError>>
    ) -> Vec<TokSpan<'a, 'b>> {
    let mut v = vec![];
    for token in tokens.into_iter() {
        match token.fragment() {
            Token::Ident(i) => {
                v.extend(try_define(i, defines).into_iter().map(|tok| TokSpan::from_strspan(tok, token.extra.0, token.span())))
            },
            Token::Pre(p) => match p {
                token::PreProc::Include(i) => {
                    let new_errors = RefCell::new(Vec::new());
                    let tokens = add_include(i, includeable_folders, included_files, &new_errors, ParseState(errors));
                    let res = expand_preprocessor_tokens(tokens, defines, includeable_folders, included_files, &new_errors);
                    v.extend(res.into_iter().map(|tok| TokSpan::from_strspan(*tok.fragment(), token.extra.0, token.span())));
                    errors.borrow_mut().extend(new_errors.into_inner().into_iter().map(|RecoveredError(_, err)| RecoveredError(token.span(), err)));
                },
                token::PreProc::Define(d) => add_define(&mut defines.borrow_mut(), d, errors),
                token::PreProc::Pragma(_) => (),
            },
            _ => v.push(token),
        }
    };

    v
}

fn try_define<'a>(i: &'a str, defines: &'a RefCell<HashMap<String, Vec<Token<'a>>>>) -> Vec<Token<'a>> {
    match defines.borrow().get(i) {
        Some(v) => v.clone(),
        None => vec![Token::Ident(i)],
    }
}

fn comment(input: &str) -> IResult<&str, &str> {
    preceded(just("//"), not_line_ending)(input)
}

fn padding(input: &str) -> IResult<&str, &str> {
    choice((
        comment,
        multispace1
    ))(input)
}

fn add_include<'a, 'b>(i: &str, includeable_folders: &Vec<String>, included_files: &'a RefCell<Vec<String>>, errors: &RefCell<Vec<RecoveredError>>, state: ParseState<'b>) -> Vec<TokSpan<'a, 'b>> {
    let quote_parser = delimited(
        just("\""),
        take_until("\""),
        just("\"")
    );
    let angle_parser = delimited(
        just("<"),
        take_until(">"),
        just(">"),
    );
    let mut include_parser = terminated(
        delimited(
            many0(padding),
            choice((
                quote_parser,
                angle_parser
            )),
            many0(padding)
        ),
        eof
    );

    let res = include_parser(i);

    match res {
        Ok((_, p)) => {
            match get_include_tokens(p, includeable_folders, included_files, errors, state) {
                Some(tokens) => tokens,
                None => vec![],
            }
        },
        Err(_) => {
            errors.borrow_mut().push(RecoveredError(0..1, String::from("Invalid include syntax")));
            vec![]
        },
    }
}

fn get_include_tokens<'a, 'b>(p: &str, includeable_folders: &Vec<String>, included_files: &'a RefCell<Vec<String>>, errors: &RefCell<Vec<RecoveredError>>, state: ParseState<'b>) -> Option<Vec<TokSpan<'a, 'b>>> {
    let mut result = None;
    for path in includeable_folders {
        let path = match shellexpand::full(path) {
            Ok(path) => match expand_str::expand_string_with_env(path.as_ref()) {
                Ok(path) => Some(Path::new(&path).join("includes").join(p)),
                Err(_) => None,
            },
            Err(_) => None
        };
        if let Some(path) = path {
            if path.exists() {
                result = Some(get_path_tokens(path, errors, included_files, state));
                break
            }
        }
    }

    if result.is_none() {
        errors.borrow_mut().push(RecoveredError(0..1, String::from("File not found")));
    }

    result
}

fn get_path_tokens<'a, 'b>(path: std::path::PathBuf, errors: &RefCell<Vec<RecoveredError>>, included_files: &'a RefCell<Vec<String>>, state: ParseState<'b>) -> Vec<TokSpan<'a, 'b>> {
    included_files.borrow_mut().push(std::fs::read_to_string(path).unwrap());
    // This code is safe because it's basically included_files.borrow() without RefCell's check.
    // If included_files.borrow() would fail, includued_files.borrow_mut() would have failed in the previous line
    let tokens = lex(unsafe{(*included_files.as_ptr()).last()}.unwrap(), &errors);

    tokens.into_iter().map(|token| TokSpan::from_strspan(*token.fragment(), state, token.span())).collect()
}

fn add_define<'a>(defines: &mut HashMap<String, Vec<Token<'a>>>, new_define: &'a str, errors: &RefCell<Vec<RecoveredError>>) {
    let ident = map(
        recognize(
            then(
                choice((alpha1, just("_"))),
                many0(choice((alphanumeric1, just("_"))))
            )
        ),
        |s| String::from(s)
    );

    let mut define_lexer = delimited(
        many0(padding),
        then(
            ident,
            |input| Ok(("", lex_tokens(input, errors)))
        ),
        many0(padding)
    );

    let res = define_lexer(new_define);
    if let Ok((_, (name, tokens))) = res {
        defines.insert(name, tokens);
    };
}

/* pub enum DefinedToken {
    K(Keyword),
    Num(String),
    Char(char),
    Str(String),
    Op(String),
    V(ValueType),
    B(BuiltFunc),
    Ident(String),
    Separator(char),
    Bool(bool),
    Comment(String),
    Err
}

impl DefinedToken {
    fn from_tok(tok: Token) -> Self {

    }

    fn to_tok(&self) -> Token {
        match self {
            DefinedToken::K(_) => todo!(),
            DefinedToken::Num(_) => todo!(),
            DefinedToken::Char(_) => todo!(),
            DefinedToken::Str(_) => todo!(),
            DefinedToken::Op(_) => todo!(),
            DefinedToken::V(_) => DefinedToken::V(_),
            DefinedToken::B(_) => DefinedToken::B(_),
            DefinedToken::Ident(_) => DefinedToken::Ident(_),
            DefinedToken::Separator(_) => DefinedToken::Separator(_),
            DefinedToken::Bool(_) => DefinedToken::Bool(_),
            DefinedToken::Comment(_) => DefinedToken::Comment(_),
            DefinedToken::Err => DefinedToken::Err,
        }
    }
}
 */