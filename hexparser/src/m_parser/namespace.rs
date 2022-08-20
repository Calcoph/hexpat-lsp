use chumsky::{prelude::*,Parser};

use crate::{m_lexer::Keyword, Span};

use super::{Token, expr_parser, Expr, Spanned, SpanASTNode};

// A namespace node in the AST.
#[derive(Debug, Clone)]
pub struct NameSpace {
    pub body: Spanned<Expr>,
    pub name: Spanned<String>,
    pub span: Span,
}

pub fn namespace_parser() -> impl Parser<Token, SpanASTNode, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let nspace = just(Token::K(Keyword::Namespace))
        .ignore_then(
            ident
                .map_with_span(|name, span| (name, span))
                .labelled("namespace name"),
        )
        .then(
            expr_parser()
                .or(just(Token::Separator('{')).ignore_then(just(Token::Separator('}'))).ignored().map_with_span(|_, span| (Expr::Empty, span)))
                .delimited_by(just(Token::Separator('{')), just(Token::Separator('}')))
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
            SpanASTNode::Namespace(
                name.clone(),
                NameSpace {
                    body,
                    name,
                    span,
                },
            )
        })
        .labelled("namespace");

    nspace
}
