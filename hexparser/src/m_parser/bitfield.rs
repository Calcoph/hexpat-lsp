use chumsky::{prelude::*,Parser};

use crate::{m_lexer::Keyword, Span, Value};

use super::{Token, Expr, Spanned, SpanASTNode};

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

    let bitfield = just(Token::K(Keyword::Bitfield))
        .ignore_then(
            ident
                .map_with_span(|name, span| (name, span))
                .labelled("bitfield name"),
        )
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

    bitfield
}

fn bitfield_entries_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok((ident.clone(), span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("identifier");

    let val = filter_map(|span, tok| match tok {
        Token::Num(_) => Ok((Expr::Value(Value::Num(42342.0)), span)),// TODO: change 42342.0 to a proper (hex, bin, oct, dec) str->f64
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("value");
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
