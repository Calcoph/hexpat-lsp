#![allow(unused)]

use std::{cell::RefCell, collections::HashMap, env::args};

use nom::multi::separated_list1;
use nom::bytes::complete::tag as just;
use crate::{m_lexer, m_parser::{self, token_parse, parse_namespace, ident, parse_struct, BinaryOp, HexTypeDef, Endianness, HexType, FuncArgument, UnaryOp}, simple_debug::SimpleDebug, token::{Token, Tokens, ValueType}, expand_preprocessor_tokens, Expr, Value, parse};

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

macro_rules! local {
    ($name:tt) => {
        (Expr::Local {
            name: (String::from($name), 0..0)
        }, 0..0)
    };
}

macro_rules! blocal {
    ($name:tt) => {
        Box::new(local!($name))
    };
}

macro_rules! bnull {
    () => {
        Box::new((Expr::Value { val: Value::Null }, 0..0))
    };
}

macro_rules! bnum {
    () => {
        Box::new((Expr::Value { val: Value::Num(0.0) }, 0..0))
    };
}

macro_rules! spanbox {
    ($tokens: expr) => {
        Box::new(($tokens, 0..0))
    };
}

/* #[test]
fn test1() {
    let test_str = "struct MoveList {
        Move moves[while(!std::mem::eof())];
};";
    //let v = vec![];
    let errs = RefCell::new(Vec::new());
    let tokens = m_lexer::lex(test_str, &errs);
    let included_files = RefCell::new(vec![]); // This vec is only so references are valid
    let defines = RefCell::new(HashMap::new());
    let includeable_folders = vec![];
    let tokens = expand_preprocessor_tokens(tokens, &defines, &includeable_folders, &included_files, &errs).into_iter()
        .filter(|tok| match tok.fragment() {
            Token::Comment(_) => false,
            _ => true
        }).collect::<Vec<_>>();
    tokens.dbg_ln(0);
    let (_, ex) = parse_struct(Tokens::new(&tokens, tokens[0].extra.0)).expect("Unrecovered error happenned in parser");
    /* let ast = token_parse(tokens.into_iter()
            .filter(|tok| match tok.fragment() {
                Token::Comment(_) => false,
                _ => true
            }
        ).collect()); */
    //dbg!(errors);
    dbg!(ex);
} */

/*#[test]
fn test2() {
    let test_str = "  
type != LSTrigger::VariableValue
";
    get_tokens!(test_str, tokens);
    tokens.dbg_ln(0);
    let a = m_parser::operations::mathematical_expression(tokens);
    a.dbg_ln(0);
}*/

#[derive(Debug)]
struct CompErr;

fn eq_hextype(ht1: &HexType, ht2: &HexType) -> bool {
    match (ht1, ht2) {
        (HexType::Custom(s1), HexType::Custom(s2)) => s1 == s2,
        (HexType::Path(v1), HexType::Path(v2)) => {
            for (s1, s2) in v1.iter().zip(v2.iter()) {
                if s1 != s2 {
                    return false
                }
            }
            v1.len() == v2.len()
        },
        (HexType::V(v1), HexType::V(v2)) => v1 == v2,
        (HexType::Null, HexType::Null) => true,
        _ => false
    }
} 

fn expr_comparer(expr1: &Expr, expr2: &Expr) -> Result<(), CompErr>{
    match (expr1, expr2) {
        (Expr::Error, Expr::Error) => Ok(()),
        (Expr::Value { val: val1 }, Expr::Value { val: val2 }) => match val1 == val2 {
            true => Ok(()),
            false => {println!("Different values |{val1:?} {val2:?}|");Err(CompErr)},
        },
        (Expr::ExprList { list: list1 }, Expr::ExprList { list: list2 }) => {
            for ((l1, _), (l2, _)) in list1.iter().zip(list2.iter()) {
                match expr_comparer(l1, l2) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            };
            if list1.len() == list2.len() {
                Ok(())
            } else {
                println!("Diferent length ExprList");
                Err(CompErr)   
            }
        },
        (Expr::UnnamedParameter { type_: (type1, _) }, Expr::UnnamedParameter { type_: (type2, _) }) => match eq_hextype(type1, type2) {
            true => Ok(()),
            false => {println!("Different types |{type1:?}, {type2:?}|");Err(CompErr)},
        },
        (Expr::Local { name: (name1, _) }, Expr::Local { name: (name2, _) }) => match name1 == name2 {
            true => Ok(()),
            false => {println!("Different names |{name1:?}, {name2:?}|");Err(CompErr)},
        },
        (Expr::Unary { operation: oper1, operand: op1 },
            Expr::Unary { operation: oper2, operand: op2 }
        ) => match expr_comparer(&op1.0, &op2.0) {
            Ok(_) => match oper1 == oper2 {
                true => Ok(()),
                false => {println!("Different operation |{oper1:?}, {oper2:?}");Err(CompErr)}
            },
            Err(e) => Err(e),
        },
        (Expr::Binary { loperand: lop1, operator: op1, roperand: rop1 },
            Expr::Binary { loperand: lop2, operator: op2, roperand: rop2 }
        ) => match op1 == op2 {
            true => match expr_comparer(&lop1.0, &lop2.0) {
                Ok(_) => match expr_comparer(&rop1.0, &rop2.0) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(e),
                },
                Err(e) => Err(e),
            },
            false => {println!("Different operators |{op1:?}, {op2:?}|");Err(CompErr)},
        },
        (Expr::Ternary { loperand: lop1, moperand: mop1, roperand: rop1 },
            Expr::Ternary { loperand: lop2, moperand: mop2, roperand: rop2 }
        ) => match expr_comparer(&lop1.0, &lop2.0) {
            Ok(_) => match expr_comparer(&mop1.0, &mop2.0) {
                Ok(_) => match expr_comparer(&rop1.0, &rop2.0) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(e),
                },
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        (Expr::Call { func_name: fname1, arguments: (args1, _) },
            Expr::Call { func_name: fname2, arguments: (args2, _) }
        ) => match expr_comparer(&fname1.0, &fname2.0) {
            Ok(_) => {
                for ((a1, _), (a2, _)) in args1.iter().zip(args2.iter()) {
                    match expr_comparer(a1, a2) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                }
                match args1.len() == args2.len() {
                    true => Ok(()),
                    false => {println!("Diferent length call arguments");Err(CompErr)},
                }
            },
            Err(e) => Err(e),
        },
        (Expr::If { test: test1, consequent: cons1 },
            Expr::If { test: test2, consequent: cons2 }
        ) => match expr_comparer(&test1.0, &test2.0) {
            Ok(_) => match expr_comparer(&cons1.0, &cons2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        (Expr::IfBlock { ifs: if1, alternative: alt1 },
            Expr::IfBlock { ifs: if2, alternative: alt2 }
        ) => match expr_comparer(&if1.0, &if2.0) {
            Ok(_) => match expr_comparer(&alt1.0, &alt2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        (
            Expr::Definition { value_type: (
                HexTypeDef { endianness: e1, name: (hn1, _) }, _
            ), name: n1, body: b1 },
            Expr::Definition { value_type: (
                HexTypeDef { endianness: e2, name: (hn2, _) }, _
            ), name: n2, body: b2 }
        ) => match e1 == e2 {
            true => match eq_hextype(hn1, hn2) {
                true => match expr_comparer(&n1.0, &n2.0) {
                    Ok(_) => match expr_comparer(&b1.0, &b2.0) {
                        Ok(_) => Ok(()),
                        Err(e) => Err(e),
                    },
                    Err(e) => Err(e),
                },
                false =>{println!("Different types |{hn1:?}, {hn2:?}|");Err(CompErr)},
            },
            false => {println!("Different endianness");Err(CompErr)},
        }
        (Expr::BitFieldEntry { name: (n1, _), length: l1 },
            Expr::BitFieldEntry { name: (n2, _), length: l2 }
        ) => match n1 == n2 {
            true => match expr_comparer(&l1.0, &l2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            false => {println!("Different bitfield names");Err(CompErr)},
        },
        (Expr::EnumEntry { name: (n1, _), value: v1 },
            Expr::EnumEntry { name: (n2, _), value: v2 }
        ) => match n1 == n2 {
            true => match expr_comparer(&v1.0, &v2.0) {
                Ok(_) => Ok(()),
                Err(e) => {println!("Different enum entry values |{v1:?}, {v2:?}|");Err(e)},
            },
            false => {println!("Different enum entry names");Err(CompErr)},
        },
        (Expr::NamespaceAccess { previous: p1, name: (n1, _) },
            Expr::NamespaceAccess { previous: p2, name: (n2, _) }
        ) => match n1 == n2 {
            true => match expr_comparer(&p1.0, &p2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            false => {println!("Different namespace names");Err(CompErr)},
        },
        (
            Expr::Using { new_name: (n1, _), template_parameters: None, old_name: (
                HexTypeDef { endianness: e1, name: (hn1, _) }, _
            ) },
            Expr::Using { new_name: (n2, _), template_parameters: None, old_name: (
                HexTypeDef { endianness: e2, name: (hn2, _) }, _
            ) }
        ) => match n1 == n2 {
            true => match e1 == e2 {
                true => match eq_hextype(hn1, hn2) {
                    true => Ok(()),
                    false => {println!("Different types |{hn1:?}, {hn2:?}|");Err(CompErr)},
                },
                false => {println!("Different endianness |{e1:?}, {e2:?}|");Err(CompErr)},
            },
            false => {println!("Different names |{n1:?}, {n2:?}|");Err(CompErr)},
        },
        (Expr::Return { value: v1 },
            Expr::Return { value: v2 }
        ) => match expr_comparer(&v1.0, &v2.0) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        },
        (Expr::Continue, Expr::Continue) => Ok(()),
        (Expr::Break, Expr::Break) => Ok(()),
        (Expr::Func { name: (n1, _), args: (ar1, _), body: b1 },
            Expr::Func { name: (n2, _), args: (ar2, _), body: b2 }
        ) => match n1 == n2 {
            true => match expr_comparer(&b1.0, &b2.0) {
                Ok(_) => {
                    for ((arg1, _), (arg2, _)) in ar1.iter().zip(ar2.iter()) {
                        match (arg1, arg2) {
                            (FuncArgument::Parameter(p1), FuncArgument::Parameter(p2)) => match expr_comparer(&p1.0, &p2.0) {
                                Ok(_) => (),
                                Err(e) => return Err(e),
                            },
                            (FuncArgument::ParameterPack((s1, _)), FuncArgument::ParameterPack((s2, _))) => match s1 == s2{
                                true => (),
                                false => {println!("Different parameter pack name |{s1:?}, {s2:?}|");return Err(CompErr)},
                            },
                            _ => {println!("Expected {arg1:?}, found {arg1:?}");return Err(CompErr)}
                        }
                    };
                    if ar1.len() == ar2.len() {
                        Ok(())
                    } else {
                        println!("Diferent length function arguments");
                        Err(CompErr)
                    }
                },
                Err(e) => Err(e),
            },
            false => {println!("Different names |{n1:?}, {n2:?}|");Err(CompErr)},
        },
        (Expr::Struct { name: (n1, _), body: b1, template_parameters: None },
            Expr::Struct { name: (n2, _), body: b2, template_parameters: None }
        ) => match n1 == n2 {
            true => match expr_comparer(&b1.0, &b2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            false => {println!("Different names |{n1:?}, {n2:?}|");Err(CompErr)},
        },
        (Expr::Namespace { name: n1, body: b1 },
            Expr::Namespace { name: n2, body: b2 }
        ) => match expr_comparer(&n1.0, &n2.0) {
            Ok(_) => match expr_comparer(&b1.0, &b2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        (Expr::Enum { name: (n1, _), value_type: (
            HexTypeDef { endianness: e1, name: (hn1, _) }, _
        ), body: b1 },
            Expr::Enum { name: (n2, _), value_type: (
                HexTypeDef { endianness: e2, name: (hn2, _) }, _
            ), body: b2 }
        ) => match n1 == n2 {
            true => match e1 == e2 {
                true => match eq_hextype(hn1, hn2) {
                    true => match expr_comparer(&b1.0, &b2.0) {
                        Ok(_) => Ok(()),
                        Err(e) => {println!("Different enum bodies");Err(e)},
                    },
                    false => {println!("Different types");Err(CompErr)},
                },
                false => {println!("Different endianness");Err(CompErr)},
            },
            false => {println!("Different enum names");Err(CompErr)},
        },
        (Expr::Bitfield { name: (n1, _), body: b1 },
            Expr::Bitfield { name: (n2, _), body: b2 }
        ) => match n1 == n2 {
            true => match expr_comparer(&b1.0, &b2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            false => {println!("Different names |{n1:?}, {n2:?}|");Err(CompErr)},
        },
        (Expr::Access { item: i1, member: m1 },
            Expr::Access { item: i2, member: m2 }
        ) => match expr_comparer(&i1.0, &i2.0) {
            Ok(_) => match expr_comparer(&m1.0, &m2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        (Expr::Attribute { arguments: (a1, _) },
            Expr::Attribute { arguments: (a2, _) }
        ) => {
            for ((arg1, _), (arg2, _)) in a1.iter().zip(a2.iter()) {
                match expr_comparer(arg1, arg2) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            };
            if a1.len() == a2.len() {
                Ok(())
            } else {
                println!("Diferent length attribute arguments");
                Err(CompErr)
            }
        },
        (Expr::AttributeArgument { name: n1, value: v1 },
            Expr::AttributeArgument { name: n2, value: v2 }
        ) => match expr_comparer(&n1.0, &n2.0) {
            Ok(_) => {
                for ((arg1, _), (arg2, _)) in v1.iter().zip(v2.iter()) {
                    match expr_comparer(arg1, arg2) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                };
                if v1.len() == v2.len() {
                    Ok(())
                } else {
                    println!("Diferent length attribute arguments");
                    Err(CompErr)
                }
            },
            Err(e) => {println!("Different names |{n1:?}, {n2:?}|");Err(CompErr)},
        },
        (Expr::WhileLoop { condition: c1, body: b1 },
            Expr::WhileLoop { condition: c2, body: b2 }
        ) => match expr_comparer(&c1.0, &c2.0) {
            Ok(_) => match expr_comparer(&b1.0, &b2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        (Expr::ForLoop { var_init: v_i1, var_test: v_t1, var_change: v_c1, body: b1 },
            Expr::ForLoop { var_init: v_i2, var_test: v_t2, var_change: v_c2, body: b2 }
        ) => match expr_comparer(&v_i1.0, &v_i2.0) {
            Ok(_) => match expr_comparer(&v_t1.0, &v_t2.0) {
                Ok(_) => match expr_comparer(&v_c1.0, &v_c2.0) {
                    Ok(_) => match expr_comparer(&b1.0, &b2.0) {
                        Ok(_) => Ok(()),
                        Err(e) => Err(e),
                    },
                    Err(e) => Err(e),
                },
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        },
        (Expr::Cast { cast_operator: (
            HexTypeDef { endianness: e1, name: (hn1, _) }, _
        ), operand: op1 },
            Expr::Cast { cast_operator: (
                HexTypeDef { endianness: e2, name: (hn2, _) }, _
            ), operand: op2 }
        ) => match e1 == e2 {
            true => match eq_hextype(hn1, hn2) {
                true => match expr_comparer(&op1.0, &op2.0) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(e),
                },
                false => {println!("Different types |{hn1:?}, {hn2:?}|");Err(CompErr)},
            },
            false => {println!("Different endianness |{e1:?}, {e2:?}|");Err(CompErr)},
        },
        (Expr::Union { name: (n1, _), body: b1, template_parameters: None },
            Expr::Union { name: (n2, _), body: b2, template_parameters: None }
        ) => match n1 == n2 {
            true => match expr_comparer(&b1.0, &b2.0) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            false => {println!("Different names |{n1:?}, {n2:?}|");Err(CompErr)},
        },
        (Expr::ArrayDefinition {
            value_type: (HexTypeDef { endianness: e1, name: (hn1, _) }, _),
            array_name: n1, size: s1, body: b1 },
        Expr::ArrayDefinition {
            value_type: (HexTypeDef { endianness: e2, name: (hn2, _) }, _),
            array_name: n2, size: s2, body: b2 }
        ) => match e1 == e2 {
            true => match eq_hextype(hn1, hn2) {
                true => match expr_comparer(&n1.0, &n2.0) {
                    Ok(_) => match expr_comparer(&s1.0, &s2.0) {
                        Ok(_) => match expr_comparer(&b1.0, &b2.0) {
                            Ok(_) => Ok(()),
                            Err(e) => Err(e),
                        },
                        Err(e) => Err(e),
                    },
                    Err(e) => Err(e),
                },
                false => {println!("Different types |{hn1:?}, {hn2:?}|");Err(CompErr)},
            },
            false => {println!("Different endianness |{e1:?}, {e2:?}|");Err(CompErr)},
        },
        (e1, e2) => {println!("Expected {e1:?}.\nGot {e2:?}");Err(CompErr)}
    }
}

#[test]
fn test_pattern_arrays() {
    let test_str = "
        fn end_of_signature() {
            return $ >= 8;
        };

        struct Signature {
            u8 first[4];
            u8 second[while(!end_of_signature())];
        };

        Signature sign @ 0x0;
    ";
    let expected_output = Expr::ExprList {
        list: vec![
            (Expr::Func {
                name: (String::from("end_of_signature"), 0..0),
                args: (vec![], 0..0),
                body: spanbox!(Expr::ExprList {
                    list: vec![
                        (Expr::Return {
                            value: spanbox!(Expr::Binary {
                                loperand: blocal!("$"),
                                operator: BinaryOp::GreaterEqual,
                                roperand: bnum!()
                            })
                        }, 0..0)
                    ]
                })
            }, 0..0),
            (Expr::Struct {
                name: (String::from("Signature"), 0..0),
                body: spanbox!(Expr::ExprList {
                    list: vec![
                        (Expr::ArrayDefinition {
                            value_type: (HexTypeDef {
                                endianness: Endianness::Unkown,
                                name: (HexType::V(ValueType::U8), 0..0)
                            }, 0..0),
                            array_name: blocal!("first"),
                            size: bnum!(),
                            body: bnull!()
                        }, 0..0),
                        (Expr::ArrayDefinition {
                            value_type: (HexTypeDef {
                                endianness: Endianness::Unkown,
                                name: (HexType::V(ValueType::U8), 0..0)
                            }, 0..0),
                            array_name: blocal!("second"),
                            size: spanbox!(Expr::WhileLoop {
                                condition: spanbox!(Expr::Unary {
                                    operation: UnaryOp::LNot,
                                    operand: spanbox!(Expr::Call {
                                        func_name: blocal!("end_of_signature"),
                                        arguments: (vec![], 0..0)
                                    })
                                }),
                                body: bnull!()
                            }),
                            body: bnull!()
                        }, 0..0)
                    ]
                }),
                template_parameters: None
            }, 0..0),
            (Expr::Definition {
                value_type: (HexTypeDef {
                    endianness: Endianness::Unkown,
                    name: (HexType::Custom(String::from("Signature")), 0..0)
                }, 0..0),
                name: blocal!("sign"),
                body: bnum!()
            }, 0..0)
        ]
    };

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_attributes() {
    let test_str = "
        struct FormatTransformTest {
            u32 x, y, z;
        } [[format(\"format_test\"), transform(\"transform_test\")]];

        struct SealedTest {
            float f;
        } [[sealed]];

        struct HiddenTest {
            double f;
        } [[hidden]];

        struct ColorTest {
            char s[5];
        } [[color(\"FF00FF\")]];

        struct NoUniqueAddressTest {
            u32 x;
            u32 y [[no_unique_address]];
        };

        fn format_test(FormatTransformTest value) {
            return \"Hello World\";
        };

        fn transform_test(FormatTransformTest value) {
            return 1337;
        };

        FormatTransformTest formatTransformTest @ 0x00;
        SealedTest sealedTest @ 0x10;
        HiddenTest hiddenTest @ 0x20;
        ColorTest colorTest @ 0x30;
        NoUniqueAddressTest noUniqueAddressTest @ 0x40;
    ";

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Struct {
            name: (String::from("FormatTransformTest"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U32), 0..0)
                    }, 0..0),
                    name: spanbox!(Expr::ExprList { list: vec![
                        local!("x"),
                        local!("y"),
                        local!("z")
                    ] }),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Struct {
            name: (String::from("SealedTest"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::Float), 0..0)
                    }, 0..0),
                    name: blocal!("f"),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Struct {
            name: (String::from("HiddenTest"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::Double), 0..0)
                    }, 0..0),
                    name: blocal!("f"),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Struct {
            name: (String::from("ColorTest"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::ArrayDefinition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::Character), 0..0)
                    }, 0..0),
                    array_name: blocal!("s"),
                    size: bnum!(),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Struct {
            name: (String::from("NoUniqueAddressTest"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U32), 0..0)
                    }, 0..0),
                    name: blocal!("x"),
                    body: bnull!()
                }, 0..0),
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U32), 0..0)
                    }, 0..0),
                    name: blocal!("y"),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Func {
            name: (String::from("format_test"), 0..0),
            args: (vec![(FuncArgument::Parameter(spanbox!(Expr::Definition {
                value_type: (HexTypeDef { endianness: Endianness::Unkown, name: (HexType::Custom(String::from("FormatTransformTest")), 0..0) }, 0..0),
                name: blocal!("value"),
                body: bnull!()
            })), 0..0)], 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Return { value: spanbox!(Expr::Value { val: Value::Str(String::from("Hello World")) }) }, 0..0)
            ] })
        }, 0..0),
        (Expr::Func {
            name: (String::from("transform_test"), 0..0),
            args: (vec![(FuncArgument::Parameter(spanbox!(Expr::Definition {
                value_type: (HexTypeDef { endianness: Endianness::Unkown, name: (HexType::Custom(String::from("FormatTransformTest")), 0..0) }, 0..0),
                name: blocal!("value"),
                body: bnull!()
            })), 0..0)], 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Return { value: bnum!() }, 0..0)
            ] })
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("FormatTransformTest")), 0..0)
            }, 0..0),
            name: blocal!("formatTransformTest"),
            body: bnum!()
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("SealedTest")), 0..0)
            }, 0..0),
            name: blocal!("sealedTest"),
            body: bnum!()
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("HiddenTest")), 0..0)
            }, 0..0),
            name: blocal!("hiddenTest"),
            body: bnum!()
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("ColorTest")), 0..0)
            }, 0..0),
            name: blocal!("colorTest"),
            body: bnum!()
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("NoUniqueAddressTest")), 0..0)
            }, 0..0),
            name: blocal!("noUniqueAddressTest"),
            body: bnum!()
        }, 0..0)
    ] };

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_bitfields() {
    let test_str = "
        bitfield TestBitfield {
            a : 4;
            b : 4;
            c : 4;
            d : 4;
        };

        be TestBitfield testBitfield @ 0x12;
    ";

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Bitfield {
            name: (String::from("TestBitfield"), 0..0),
            body: spanbox!(Expr::ExprList {
                list: vec![
                    (Expr::BitFieldEntry {
                        name: (String::from("a"), 0..0),
                        length: bnum!()
                    }, 0..0),
                    (Expr::BitFieldEntry {
                        name: (String::from("b"), 0..0),
                        length: bnum!()
                    }, 0..0),
                    (Expr::BitFieldEntry {
                        name: (String::from("c"), 0..0),
                        length: bnum!()
                    }, 0..0),
                    (Expr::BitFieldEntry {
                        name: (String::from("d"), 0..0),
                        length: bnum!()
                    }, 0..0)
                ]
            })
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Big,
                name: (HexType::Custom(String::from("TestBitfield")), 0..0)
            }, 0..0),
            name: blocal!("testBitfield"),
            body: bnum!()
        }, 0..0)
    ] };

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_enums() {
    let test_str = "
        enum TestEnum : u32 {
            A,
            B = 0x0C,
            C,
            D
        };

        be TestEnum testEnum @ 0x08;
    ";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Enum {
            name: (String::from("TestEnum"), 0..0),
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::V(ValueType::U32), 0..0)
            }, 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::EnumEntry {
                    name: (String::from("A"), 0..0),
                    value: bnull!()
                }, 0..0),
                (Expr::EnumEntry {
                    name: (String::from("B"), 0..0),
                    value: bnum!()
                }, 0..0),
                (Expr::EnumEntry {
                    name: (String::from("C"), 0..0),
                    value: bnull!()
                }, 0..0),
                (Expr::EnumEntry {
                    name: (String::from("D"), 0..0),
                    value: bnull!()
                }, 0..0)
            ] })
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Big,
                name: (HexType::Custom(String::from("TestEnum")), 0..0)
            }, 0..0),
            name: blocal!("testEnum"),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_example() {
    let test_str = "";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::Value { val: Value::Null };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_extra_semicolon() {
    let test_str = "
        struct Test {
            u32 x;;;
            u8 y;
            float z;;
        };;

        struct Test2 {
            u32 x;
            u32 y;
        };

        Test test @ 0x00;;;
        Test test2 @ 0x10;
    ";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Struct {
            name: (String::from("Test"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U32), 0..0)
                    }, 0..0),
                    name: blocal!("x"),
                    body: bnull!()
                }, 0..0),
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U8), 0..0)
                    }, 0..0),
                    name: blocal!("y"),
                    body: bnull!()
                }, 0..0),
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::Float), 0..0)
                    }, 0..0),
                    name: blocal!("z"),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Struct {
            name: (String::from("Test2"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U32), 0..0)
                    }, 0..0),
                    name: blocal!("x"),
                    body: bnull!()
                }, 0..0),
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U32), 0..0)
                    }, 0..0),
                    name: blocal!("y"),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("Test")), 0..0)
            }, 0..0),
            name: blocal!("test"),
            body: bnum!()
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("Test")), 0..0)
            }, 0..0),
            name: blocal!("test2"),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_namespaces() {
    let test_str = "
        namespace A {
            struct Test {
                u32 x;
            };
        }

        namespace B {
            struct Test {
                u16 x;
            };
        }

        using ATest = A::Test;

        A::Test test1 @ 0x10;
        ATest test2 @ 0x20;
        B::Test test3 @ 0x20;
    ";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Namespace {
            name: blocal!("A"),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Struct { name: (String::from("Test"), 0..0), body: spanbox!(Expr::ExprList { list: vec![
                     (Expr::Definition {
                        value_type: (HexTypeDef {
                            endianness: Endianness::Unkown,
                            name: (HexType::V(ValueType::U32), 0..0)
                        }, 0..0),
                        name: blocal!("x"),
                        body: bnull!()
                    }, 0..0)
                ] }),
                template_parameters: None
                }, 0..0)
            ] })
        }, 0..0),
        (Expr::Namespace {
            name: blocal!("B"),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Struct { name: (String::from("Test"), 0..0), body: spanbox!(Expr::ExprList { list: vec![
                     (Expr::Definition {
                        value_type: (HexTypeDef {
                            endianness: Endianness::Unkown,
                            name: (HexType::V(ValueType::U16), 0..0)
                        }, 0..0),
                        name: blocal!("x"),
                        body: bnull!()
                    }, 0..0)
                ] }),
                template_parameters: None
                }, 0..0)
            ] })
        }, 0..0),
        (Expr::Using {
            new_name: (String::from("ATest"), 0..0),
            template_parameters: None,
            old_name: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Path(vec![
                    String::from("A"),
                    String::from("Test")
                ]), 0..0)
            }, 0..0)
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Path(vec![
                    String::from("A"),
                    String::from("Test")
                ]), 0..0)
            }, 0..0),
            name: blocal!("test1"),
            body: bnum!()
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("ATest")), 0..0)
            }, 0..0),
            name: blocal!("test2"),
            body: bnum!()
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Path(vec![
                    String::from("B"),
                    String::from("Test")
                ]), 0..0)
            }, 0..0),
            name: blocal!("test3"),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_nested_structs() {
    let test_str = "
        fn end_of_body() {
            u32 start = addressof(parent.parent.hdr);
            u32 len = parent.parent.hdr.len;
            u32 end = start + len;

            return $ >= end;
        };

        struct Header {
            u8 len;
        };

        struct Body {
            u8 arr[while(!end_of_body())];
        };

        struct Data {
            Header hdr;
            Body body;
        };

        Data data @ 0x0;
    ";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Func {
            name: (String::from("end_of_body"), 0..0),
            args: (vec![], 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U32), 0..0)
                    }, 0..0),
                    name: blocal!("start"),
                    body: spanbox!(Expr::Call {
                        func_name: blocal!("addressof"),
                        arguments: (vec![
                            (Expr::Access {
                                item: blocal!("parent"),
                                member: spanbox!(Expr::Access {
                                    item: blocal!("parent"),
                                    member: blocal!("hdr"),
                                })
                            }, 0..0)
                        ], 0..0)
                    })
                }, 0..0),
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U32), 0..0)
                    }, 0..0),
                    name: blocal!("len"),
                    body: spanbox!(Expr::Access {
                        item: blocal!("parent"),
                        member: spanbox!(Expr::Access {
                            item: blocal!("parent"),
                            member: spanbox!(Expr::Access {
                                item: blocal!("hdr"),
                                member: blocal!("len"),
                            })
                        })
                    })
                }, 0..0),
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U32), 0..0)
                    }, 0..0),
                    name: blocal!("end"),
                    body: spanbox!(Expr::Binary {
                        loperand: blocal!("start"),
                        operator: BinaryOp::Add,
                        roperand: blocal!("len"),
                    })
                }, 0..0),
                (Expr::Return { value: spanbox!(Expr::Binary {
                    loperand: blocal!("$"),
                    operator: BinaryOp::GreaterEqual,
                    roperand: blocal!("end"),
                }) }, 0..0)
            ] })
        }, 0..0),
        (Expr::Struct {
            name: (String::from("Header"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U8), 0..0)
                    }, 0..0),
                    name: blocal!("len"),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Struct {
            name: (String::from("Body"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::ArrayDefinition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U8), 0..0)
                    }, 0..0),
                    array_name: blocal!("arr"),
                    size: spanbox!(Expr::WhileLoop {
                        condition: spanbox!(Expr::Unary {
                            operation: UnaryOp::LNot,
                            operand: spanbox!(Expr::Call {
                                func_name: blocal!("end_of_body"),
                                arguments: (vec![], 0..0)
                            })
                        }),
                        body: bnull!()
                    }),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Struct {
            name: (String::from("Data"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::Custom(String::from("Header")), 0..0)
                    }, 0..0),
                    name: blocal!("hdr"),
                    body: bnull!()
                }, 0..0),
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::Custom(String::from("Body")), 0..0)
                    }, 0..0),
                    name: blocal!("body"),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("Data")), 0..0)
            }, 0..0),
            name: blocal!("data"),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_padding() {
    let test_str = "
        struct TestStruct {
            s32 variable;
            padding[20];
            u8 array[0x10];
        };

        TestStruct testStruct @ 0x100;
    ";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Struct {
            name: (String::from("TestStruct"), 0..0),
            body: spanbox!(Expr::ExprList {
                list: vec![
                    (Expr::Definition {
                        value_type: (HexTypeDef {
                            endianness: Endianness::Unkown,
                            name: (HexType::V(ValueType::S32), 0..0)
                        }, 0..0),
                        name: blocal!("variable"),
                        body: bnull!()
                    }, 0..0),
                    (Expr::Value { val: Value::Num(0.0) }, 0..0), // padding // TODO: Expr for padding instead of value
                    (Expr::ArrayDefinition {
                        value_type: (HexTypeDef {
                            endianness: Endianness::Unkown,
                            name: (HexType::V(ValueType::U8), 0..0)
                        }, 0..0),
                        array_name: blocal!("array"),
                        size: bnum!(),
                        body: bnull!()
                    }, 0..0)
                ]
            }),
            template_parameters: None
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("TestStruct")), 0..0)
            }, 0..0),
            name: blocal!("testStruct"),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_placement() {
    let test_str = "
        u32 placementVar @ 0x00;
        u8 placementArray[10] @ 0x10;
    ";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::V(ValueType::U32), 0..0)
            }, 0..0),
            name: blocal!("placementVar"),
            body: bnum!()
        }, 0..0),
        (Expr::ArrayDefinition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::V(ValueType::U8), 0..0)
            }, 0..0),
            array_name: blocal!("placementArray"),
            size: bnum!(),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_pointers() {
    let test_str = "
        u32 *placementPointer : u8 @ 0x0C;
        u32 *pointerToArray[10] : u8 @ $;

        fn Rel(u128) { return 0x1D; };
        u32 *pointerRelativeSigned : s8 @ 0x1D [[pointer_base(\"Rel\")]];
    ";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::V(ValueType::U32), 0..0)
            }, 0..0),
            name: blocal!("placementPointer"),
            body: bnum!()
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::V(ValueType::U32), 0..0)
            }, 0..0),
            name: blocal!("pointerToArray"),
            body: blocal!("$"),
        }, 0..0),
        (Expr::Func {
            name: (String::from("Rel"), 0..0),
            args: (vec![(FuncArgument::Parameter(spanbox!(Expr::UnnamedParameter { type_: (HexType::V(ValueType::U128), 0..0) })), 0..0)], 0..0),
            body: spanbox!(Expr::ExprList {
                list: vec![
                    (Expr::Return {
                        value: bnum!()
                    }, 0..0)
                ]
            })
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::V(ValueType::U32), 0..0)
            }, 0..0),
            name: blocal!("pointerRelativeSigned"),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_rvalues() {
    let test_str = "
        union C {
            u8 y;
            u8 array[parent.parent.x];
        };

        struct B {
            C *c : u8;
        };

        struct A {
            u8 x;
            B b;
        };

        A a @ 0x00;
    ";
    
    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Union {
            name: (String::from("C"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U8), 0..0)
                    }, 0..0),
                    name: blocal!("y"),
                    body: bnull!()
                }, 0..0),
                (Expr::ArrayDefinition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U8), 0..0)
                    }, 0..0),
                    array_name: blocal!("array"),
                    size: spanbox!(Expr::Access {
                        item: blocal!("parent"),
                        member: spanbox!(Expr::Access {
                            item: blocal!("parent"),
                            member: blocal!("x"),
                        }),
                    }),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Struct {
            name: (String::from("B"), 0..0),
            body: spanbox!(Expr::ExprList {
                list: vec![
                    (Expr::Definition {
                        value_type: (HexTypeDef {
                            endianness: Endianness::Unkown,
                            name: (HexType::Custom(String::from("C")), 0..0)
                        }, 0..0),
                        name: blocal!("c"),
                        body: bnull!()
                    }, 0..0)
                ]
            }),
            template_parameters: None
        }, 0..0),
        (Expr::Struct {
            name: (String::from("A"), 0..0),
            body: spanbox!(Expr::ExprList {
                list: vec![
                    (Expr::Definition {
                        value_type: (HexTypeDef {
                            endianness: Endianness::Unkown,
                            name: (HexType::V(ValueType::U8), 0..0)
                        }, 0..0),
                        name: blocal!("x"),
                        body: bnull!()
                    }, 0..0),
                    (Expr::Definition {
                        value_type: (HexTypeDef {
                            endianness: Endianness::Unkown,
                            name: (HexType::Custom(String::from("B")), 0..0)
                        }, 0..0),
                        name: blocal!("b"),
                        body: bnull!()
                    }, 0..0)
                ]
            }),
            template_parameters: None
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("A")), 0..0)
            }, 0..0),
            name: blocal!("a"),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_structs() {
    let test_str = "
        struct TestStruct {
            s32 variable;
            u8 array[0x10];
        };

        TestStruct testStruct @ 0x100;
    ";
    
    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Struct {
            name: (String::from("TestStruct"), 0..0),
            body: spanbox!(Expr::ExprList {
                list: vec![
                    (Expr::Definition {
                        value_type: (HexTypeDef {
                            endianness: Endianness::Unkown,
                            name: (HexType::V(ValueType::S32), 0..0)
                        }, 0..0),
                        name: blocal!("variable"),
                        body: bnull!()
                    }, 0..0),
                    (Expr::ArrayDefinition {
                        value_type: (HexTypeDef {
                            endianness: Endianness::Unkown,
                            name: (HexType::V(ValueType::U8), 0..0)
                        }, 0..0),
                        array_name: blocal!("array"),
                        size: bnum!(),
                        body: bnull!()
                    }, 0..0)
                ]
            }),
            template_parameters: None
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("TestStruct")), 0..0)
            }, 0..0),
            name: blocal!("testStruct"),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}

#[test]
fn test_pattern_unions() {
    let test_str = "
        union TestUnion {
            s32 array[2];
            u128 variable;
        };

        TestUnion testUnion @ 0x200;
    ";

    let includeable_folders = vec![
        String::from("~/.local/share/imhex"),
        String::from("/usr/share/imhex"),
        String::from("%localappdata%/imhex"),
        String::from("%programfiles%/imhex")
    ];

    let expected_output = Expr::ExprList { list: vec![
        (Expr::Union {
            name: (String::from("TestUnion"), 0..0),
            body: spanbox!(Expr::ExprList { list: vec![
                (Expr::ArrayDefinition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::S32), 0..0)
                    }, 0..0),
                    array_name: blocal!("array"),
                    size: bnum!(),
                    body: bnull!()
                }, 0..0),
                (Expr::Definition {
                    value_type: (HexTypeDef {
                        endianness: Endianness::Unkown,
                        name: (HexType::V(ValueType::U128), 0..0)
                    }, 0..0),
                    name: blocal!("variable"),
                    body: bnull!()
                }, 0..0)
            ] }),
            template_parameters: None
        }, 0..0),
        (Expr::Definition {
            value_type: (HexTypeDef {
                endianness: Endianness::Unkown,
                name: (HexType::Custom(String::from("TestUnion")), 0..0)
            }, 0..0),
            name: blocal!("testUnion"),
            body: bnum!()
        }, 0..0)
    ] };

    let ((ex, _), _, _) = parse(test_str, &includeable_folders);
    expr_comparer(&ex, &expected_output).unwrap()
}
