use chumsky::{prelude::*,Parser};

use crate::{m_lexer::Keyword, Span, Value};

use super::{Token, expr_parser, Expr, Spanned, SpanASTNode};

// A struct node in the AST.
#[derive(Debug, Clone)]
pub struct Struct {
    pub body: Spanned<Expr>,
    pub name: Spanned<String>,
    pub span: Span,
}

pub fn struct_parser() -> impl Parser<Token, SpanASTNode, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let strct = just(Token::K(Keyword::Struct))
        .ignore_then(
            ident
                .map_with_span(|name, span| (name, span))
                .labelled("struct name"),
        )
        .then(
            expr_parser()
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
        ).then_ignore(just(Token::Separator(';')))
        .map_with_span(|(name, body), span| {
            SpanASTNode::Struct(
                name.clone(),
                Struct {
                    body,
                    name,
                    span,
                },
            )
        })
        .labelled("struct");

    strct
}
