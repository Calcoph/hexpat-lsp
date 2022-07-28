use std::collections::HashMap;

use chumsky::{prelude::*,Parser};

use crate::chumsky::{lexer::Keyword, Span};

use super::{Token, expr_parser, Expr, Spanned};

// A struct node in the AST.
#[derive(Debug)]
pub struct Struct {
    pub body: Spanned<Expr>,
    pub name: Spanned<String>,
    pub span: Span,
}

fn struct_parser() -> impl Parser<Token, HashMap<String, Struct>, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let strct = just(Token::K(Keyword::Fn))
        .ignore_then(
            ident
                .map_with_span(|name, span| (name, span))
                .labelled("struct name"),
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
            (
                name.clone(),
                Struct {
                    body,
                    name,
                    span,
                },
            )
        })
        .labelled("struct");

    strct.repeated()
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
