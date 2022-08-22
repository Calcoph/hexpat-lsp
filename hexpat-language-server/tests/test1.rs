use hexparser::parse;

#[test]
fn test1() {
    let test_str = "  
a[5];
";
    let (ast, errors, semantic_tokens) = parse(test_str);
    dbg!(ast.unwrap());
}