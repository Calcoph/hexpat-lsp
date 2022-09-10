use std::cell::RefCell;

use crate::{parse, m_lexer, m_parser, simple_debug::SimpleDebug, token::{Token, Tokens}, recovery_err::RecoveredError};

macro_rules! get_tokens {
    ( $test_str:expr, $var:ident ) => {
        let errs = RefCell::new(Vec::new());
        let tokens = m_lexer::lex($test_str, &errs).into_iter()
            .filter(|tok| match tok.fragment() {
                Token::Comment(_) => false,
                _ => true
            }
        ).collect::<Vec<_>>();
        let $var = Tokens::new(&tokens, tokens[0].extra.0);
    };
}

/* #[test]
fn test1() {
    let test_str = "  
struct MiStruct {
    u8 a;
};
";
    let v = vec![];
    let errs = RefCell::new(Vec::new());
    let tokens = m_lexer::lex(test_str, &errs);
    //tokens.dbg();
    let (ast, errors, _semantic_tokens) = parse(test_str, &v);
    //dbg!(errors);
    //dbg!(ast);
} */

#[test]
fn test2() {
    let test_str = "  
MyStruct a;
";
    get_tokens!(test_str, tokens);
    tokens.dbg_ln(0);
    let a = m_parser::member(tokens);
    a.dbg_ln(0);
}
