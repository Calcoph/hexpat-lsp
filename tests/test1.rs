use hexparser::parse;

#[test]
fn test1() {
    let test_str = "  
dsadas dsadasdf;
";
    let (ast, errors, semantic_tokens) = parse(test_str);
    dbg!(ast.unwrap());
}