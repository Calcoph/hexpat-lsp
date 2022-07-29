use chumsky::{prelude::*,Parser};

use crate::chumsky::{m_lexer::Keyword, Span};

use super::{Token, expr_parser, Expr, Spanned, SpanASTNode};

// A struct node in the AST.
#[derive(Debug, Clone)]
pub struct BitField {
    pub body: Spanned<Expr>,
    pub name: Spanned<String>,
    pub span: Span,
}

pub fn bitfield_parser() -> impl Parser<Token, SpanASTNode, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let strct = just(Token::K(Keyword::Bitfield))
        .ignore_then(
            ident
                .map_with_span(|name, span| (name, span))
                .labelled("bitfield name"),
        )
        .then(
            expr_parser()
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
            SpanASTNode::Bitfield(
                name.clone(),
                BitField {
                    body,
                    name,
                    span,
                },
            )
        })
        .labelled("bitfield");

    strct
}
