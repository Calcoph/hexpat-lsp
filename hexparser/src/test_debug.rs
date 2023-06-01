/// Just a way to run the parser without debugging the whole extension, not actual tests

use crate::{parse, simple_debug};

#[test]
fn debug_test() {
    let test_str = "fn myfunc() {
        dasdsads
        u8 myothervariable;
        while (true) {
            u8 myvariable = 0x0;
            u8 c = myothervariable;
        }
        u8 d = my;
        u8 b = myothervariable;
        u8 a = myothervariable;
    };";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let ((expr, _), errs, _) = parse(test_str, &includeable_folders);
    for err in errs {
        println!("{}",&test_str[err.0.start..err.0.end-1]);
        dbg!(err.1);
    }

    use simple_debug::SimpleDebug;

    expr.dbg(1);
}
