use std::collections::HashMap;

use im_rc::Vector;
use nrs_language_server::chumsky::{parse, type_inference};

fn main() {
    let source = include_str!("./test.nrs");
    // let source = r#"
    // test
    // println!("{:?}", &source[10..11]);
    let (ast, errors, semantic_tokens) = parse(source);
    if let Some(ref ast) = ast {
        println!("{:#?}", ast);
    } else {
        println!("{:?}", errors);
    }
    println!("{:?}", semantic_tokens);
    let mut hashmap = HashMap::new();
    if let Some(ast) = ast {
        ast.into_iter().for_each(|(k, v)| {
            type_inference(&v.body, &mut hashmap);
        });
    }
    println!("{:?}", hashmap);
}
