use hexparser::parse;

#[test]
fn test1() {
    let test_str = "  
a[5];
";
    let v = vec![];
    let (ast, errors, semantic_tokens) = parse(test_str, &v);
    //dbg!(ast.unwrap());
}