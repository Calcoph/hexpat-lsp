use chumsky::{prelude::*,Parser};

use crate::{Span, m_lexer::Keyword};

use super::{Token, Expr, expr_parser, Spanned, SpanASTNode};

// A function node in the AST.
#[derive(Debug, Clone)]
pub struct Func {
    pub args: Vec<Spanned<String>>,
    pub body: Spanned<Expr>,
    pub name: Spanned<String>,
    pub span: Span,
}

pub fn funcs_parser() -> impl Parser<Token, SpanASTNode, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    // Argument lists are just identifiers separated by commas, surrounded by parentheses
    let args = ident
        .map_with_span(|name, span| (name, span))
        .clone()
        .separated_by(just(Token::Separator(',')))
        .allow_trailing()
        .delimited_by(just(Token::Separator('(')), just(Token::Separator(')')))
        .labelled("function args");

    let func = just(Token::K(Keyword::Fn))
        .ignore_then(
            ident
                .map_with_span(|name, span| (name, span))
                .labelled("function name"),
        )
        .then(args)
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
        .map_with_span(|((name, args), body), span| {
            SpanASTNode::Func(
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

    func/*.repeated()
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
        .then_ignore(end())*/
}
