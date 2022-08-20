use hexparser::parse;

#[test]
fn test1() {
    let test_str = "
aaa asd @ 0x23;

ads asds @ 0x00;

sads dd @ 0x00;

aaaaaaa qewe @ 0x0;

htyhty hytht @ 0x00;

asd as @ qs;    
";
    let (ast, errors, semantic_tokens) = parse(test_str);
    dbg!(semantic_tokens);
}