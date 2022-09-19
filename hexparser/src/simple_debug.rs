use nom_supreme::error::StackContext;

use crate::{recovery_err::{TokError, TokResult}, token::{Tokens, Spanned, Token, TokSpan}, Expr};


pub trait SimpleDebug {
    fn dbg(&self, indentation: i32);
    fn dbg_ln(&self, indentation: i32);
}

impl<'a, 'b> SimpleDebug for TokSpan<'a, 'b> {
    fn dbg(&self, indentation: i32) {
        self.fragment().dbg(indentation);
    }

    fn dbg_ln(&self, indentation: i32) {
        self.fragment().dbg_ln(indentation);
    }
}

impl<'a> SimpleDebug for Token<'a> {
    fn dbg(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            Token::K(x) => print!("K({x:?})"),
            Token::Num(x) => print!("Num({x:?})"),
            Token::Char(x) => print!("Char{x:?})"),
            Token::Str(x) => print!("Str({x:?})"),
            Token::Op(x) => print!("Op({x:?})"),
            Token::V(x) => print!("V({x:?})"),
            Token::B(x) => print!("B({x:?})"),
            Token::Ident(x) => print!("Ident({x:?})"),
            Token::Separator(x) => print!("Separator({x:?})"),
            Token::Bool(x) => print!("Bool({x:?})"),
            Token::Pre(x) => print!("Pre({x:?})"),
            Token::Comment(_) => (),
            Token::Err => print!("Err"),
        }
    }

    fn dbg_ln(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            Token::K(x) => println!("K({x:?})"),
            Token::Num(x) => println!("Num({x:?})"),
            Token::Char(x) => println!("Char{x:?})"),
            Token::Str(x) => println!("Str({x:?})"),
            Token::Op(x) => println!("Op({x:?})"),
            Token::V(x) => println!("V({x:?})"),
            Token::B(x) => println!("B({x:?})"),
            Token::Ident(x) => println!("Ident({x:?})"),
            Token::Separator(x) => println!("Separator({x:?})"),
            Token::Bool(x) => println!("Bool({x:?})"),
            Token::Pre(x) => println!("Pre({x:?})"),
            Token::Comment(_) => (),
            Token::Err => println!("Err"),
        }
    }
}

impl<T> SimpleDebug for Vec<T>
where
    T: SimpleDebug
{
    fn dbg(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        self.iter().for_each(|t| t.dbg(0))
    }

    fn dbg_ln(&self, indentation: i32) {
        self.iter().for_each(|t| t.dbg_ln(indentation))
    }
}

impl<'a, 'b> SimpleDebug for Tokens<'a, 'b> {
    fn dbg(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        self.tokens.iter().for_each(|t| t.dbg(0))
    }

    fn dbg_ln(&self, indentation: i32) {
        self.tokens.iter().for_each(|t| t.dbg_ln(indentation))
    }
}

impl<'a, 'b> SimpleDebug for TokResult<'a, 'b, Spanned<Expr>> {
    fn dbg(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            Ok(a) => {
                print!("Ok(");
                a.0.dbg(0);
                print!(", ");
                a.1.0.dbg(0);
                print!(")");
            },
            Err(e) => {
                print!("Err(");
                e.dbg(0);
                print!(")");
            },
        }
    }

    fn dbg_ln(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            Ok(a) => {
                println!("Ok(");
                a.0.dbg(indentation+1);
                println!(",");
                a.1.0.dbg_ln(indentation+1);
                println!(")");
            },
            Err(e) => {
                println!("Err(");
                e.dbg_ln(indentation+1);
                println!(")");
            },
        }
    }
}

impl<T> SimpleDebug for nom::Err<T>
where
    T: SimpleDebug
{
    fn dbg(&self, indentation: i32) {
        match self {
            nom::Err::Incomplete(_) => unreachable!(),
            nom::Err::Error(e) => e.dbg(indentation),
            nom::Err::Failure(e) => e.dbg(indentation),
        }
    }

    fn dbg_ln(&self, indentation: i32) {
        match self {
            nom::Err::Incomplete(_) => unreachable!(),
            nom::Err::Error(e) => e.dbg_ln(indentation),
            nom::Err::Failure(e) => e.dbg_ln(indentation),
        }
    }
}

impl<'a, 'b> SimpleDebug for TokError<'a, 'b> {
    fn dbg(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            nom_supreme::error::GenericErrorTree::Base { location, kind } => {
                print!("Base(");
                location.dbg(0);
                print!(", ");
                match kind {
                    nom_supreme::error::BaseErrorKind::Expected(a) => match a {
                        nom_supreme::error::Expectation::Tag(_) => print!("EXPECTED TAG"),
                        nom_supreme::error::Expectation::Char(_) => print!("EXPECTED CHAR"),
                        nom_supreme::error::Expectation::Alpha => print!("EXPECTED ALPHA"),
                        nom_supreme::error::Expectation::Digit => print!("EXPECTED DIGIT"),
                        nom_supreme::error::Expectation::HexDigit => print!("EXPECTED HEXDIGIT"),
                        nom_supreme::error::Expectation::OctDigit => print!("EXPECTED OCTDIGIT"),
                        nom_supreme::error::Expectation::AlphaNumeric => print!("EXPECTED ALPHANUMERIC"),
                        nom_supreme::error::Expectation::Space => print!("EXPECTED SPACE"),
                        nom_supreme::error::Expectation::Multispace => print!("EXPECTED MULTISPACE"),
                        nom_supreme::error::Expectation::CrLf => print!("EXPECTED CRLF"),
                        nom_supreme::error::Expectation::Eof => print!("EXPECTED EOF"),
                        nom_supreme::error::Expectation::Something => print!("EXPECTED SOMETHING"),
                        _ => todo!(),
                    },
                    nom_supreme::error::BaseErrorKind::Kind(a) => match a {
                        nom::error::ErrorKind::Tag => print!("KIND TAG"),
                        nom::error::ErrorKind::MapRes => print!("KIND MAPRES"),
                        nom::error::ErrorKind::MapOpt => print!("KIND MAPOPT"),
                        nom::error::ErrorKind::Alt => print!("KIND ALT"),
                        nom::error::ErrorKind::IsNot => print!("KIND ISNOT"),
                        nom::error::ErrorKind::IsA => print!("KIND ISA"),
                        nom::error::ErrorKind::SeparatedList => print!("KIND SEPARATEDLIST"),
                        nom::error::ErrorKind::SeparatedNonEmptyList => print!("KIND SEPARATEDNONEMPTYLIST"),
                        nom::error::ErrorKind::Many0 => print!("KIND MANY0"),
                        nom::error::ErrorKind::Many1 => print!("KIND MANY1"),
                        nom::error::ErrorKind::ManyTill => print!("KIND MANYTILL"),
                        nom::error::ErrorKind::Count => print!("KIND COUNT"),
                        nom::error::ErrorKind::TakeUntil => print!("KIND TAKEUNTIL"),
                        nom::error::ErrorKind::LengthValue => print!("KIND LENGTHVALUE"),
                        nom::error::ErrorKind::TagClosure => print!("KIND TAGCLOSURE"),
                        nom::error::ErrorKind::Alpha => print!("KIND ALPHA"),
                        nom::error::ErrorKind::Digit => print!("KIND DIGIT"),
                        nom::error::ErrorKind::HexDigit => print!("KIND HEXDIGIT"),
                        nom::error::ErrorKind::OctDigit => print!("KIND OCTDIGIT"),
                        nom::error::ErrorKind::AlphaNumeric => print!("KIND ALPHANUMERIC"),
                        nom::error::ErrorKind::Space => print!("KIND SPACE"),
                        nom::error::ErrorKind::MultiSpace => print!("KIND MULTISPACE"),
                        nom::error::ErrorKind::LengthValueFn => print!("KIND LENGTHVALUEFN"),
                        nom::error::ErrorKind::Eof => print!("KIND OF"),
                        nom::error::ErrorKind::Switch => print!("KIND SWITCH"),
                        nom::error::ErrorKind::TagBits => print!("KIND TAGBITS"),
                        nom::error::ErrorKind::OneOf => print!("KIND ONEOF"),
                        nom::error::ErrorKind::NoneOf => print!("KIND NONEOF"),
                        nom::error::ErrorKind::Char => print!("KIND CHAR"),
                        nom::error::ErrorKind::CrLf => print!("KIND CRLF"),
                        nom::error::ErrorKind::RegexpMatch => print!("KIND REGEXPMATCH"),
                        nom::error::ErrorKind::RegexpMatches => print!("KIND REGEXPMATCHES"),
                        nom::error::ErrorKind::RegexpFind => print!("KIND REGEXPFIND"),
                        nom::error::ErrorKind::RegexpCapture => print!("KIND REGEXPCAPTURE"),
                        nom::error::ErrorKind::RegexpCaptures => print!("KIND REGEXPCAPTURES"),
                        nom::error::ErrorKind::TakeWhile1 => print!("KIND TAKEWHILE1"),
                        nom::error::ErrorKind::Complete => print!("KIND COMPLETE"),
                        nom::error::ErrorKind::Fix => print!("KIND FIX"),
                        nom::error::ErrorKind::Escaped => print!("KIND ESCAPED"),
                        nom::error::ErrorKind::EscapedTransform => print!("KIND ESCAPEDTRANSFORM"),
                        nom::error::ErrorKind::NonEmpty => print!("KIND NONEMPTY"),
                        nom::error::ErrorKind::ManyMN => print!("KIND MANYMN"),
                        nom::error::ErrorKind::Not => print!("KIND NOT"),
                        nom::error::ErrorKind::Permutation => print!("KIND PERMUTATION"),
                        nom::error::ErrorKind::Verify => print!("KIND VERIFY"),
                        nom::error::ErrorKind::TakeTill1 => print!("KIND TAKETILL1"),
                        nom::error::ErrorKind::TakeWhileMN => print!("KIND TAKEWHILEMN"),
                        nom::error::ErrorKind::TooLarge => print!("KIND TOOLARGE"),
                        nom::error::ErrorKind::Many0Count => print!("KIND MANY0COUNT"),
                        nom::error::ErrorKind::Many1Count => print!("KIND MANY1COUNT"),
                        nom::error::ErrorKind::Float => print!("KIND FLOAT"),
                        nom::error::ErrorKind::Satisfy => print!("KIND SATISFY"),
                        nom::error::ErrorKind::Fail => print!("KIND FAIL"),
                    },
                    nom_supreme::error::BaseErrorKind::External(_) => print!("EXTERNAL"),
                };
                print!(")");
            },
            nom_supreme::error::GenericErrorTree::Stack { base, contexts } => {
                print!("Stack(");
                base.dbg(0);
                print!(", ");
                contexts.dbg(0);
                print!(")");
            },
            nom_supreme::error::GenericErrorTree::Alt(v) => {
                print!("Alt(");
                for a in v {
                    a.dbg(0);
                    print!(", ");
                }
                print!(")");
            },
        }
    }

    fn dbg_ln(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            nom_supreme::error::GenericErrorTree::Base { location, kind } => {
                println!("Base(");
                location.dbg(indentation+1);
                println!(",");
                for _ in 0..indentation {
                    print!("  ");
                };
                match kind {
                    nom_supreme::error::BaseErrorKind::Expected(_) => println!("EXPECTED"),
                    nom_supreme::error::BaseErrorKind::Kind(_) => println!("KIND"),
                    nom_supreme::error::BaseErrorKind::External(_) => println!("EXTERNAL"),
                };
                for _ in 0..indentation {
                    print!("  ");
                };
                println!(")");
            },
            nom_supreme::error::GenericErrorTree::Stack { base, contexts } => {
                println!("Stack(");
                base.dbg(indentation+1);
                println!(",");
                contexts.dbg(indentation+1);
                println!();
                for _ in 0..indentation {
                    print!("  ");
                };
                println!(")");
            },
            nom_supreme::error::GenericErrorTree::Alt(v) => {
                println!("Alt(");
                for a in v {
                    a.dbg(indentation+1);
                    println!(",");
                }
                for _ in 0..indentation {
                    print!("  ");
                };
                println!(")");
            },
        }
    }
}

impl SimpleDebug for StackContext<&str> {
    fn dbg(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            StackContext::Kind(a) => match a {
                nom::error::ErrorKind::Tag => print!("ErrorKind"),
                nom::error::ErrorKind::MapRes => print!("ErrorKind"),
                nom::error::ErrorKind::MapOpt => print!("ErrorKind"),
                nom::error::ErrorKind::Alt => print!("ErrorKind"),
                nom::error::ErrorKind::IsNot => print!("ErrorKind"),
                nom::error::ErrorKind::IsA => print!("ErrorKind"),
                nom::error::ErrorKind::SeparatedList => print!("ErrorKind"),
                nom::error::ErrorKind::SeparatedNonEmptyList => print!("ErrorKind"),
                nom::error::ErrorKind::Many0 => print!("ErrorKind"),
                nom::error::ErrorKind::Many1 => print!("ErrorKind"),
                nom::error::ErrorKind::ManyTill => print!("ErrorKind"),
                nom::error::ErrorKind::Count => print!("ErrorKind"),
                nom::error::ErrorKind::TakeUntil => print!("ErrorKind"),
                nom::error::ErrorKind::LengthValue => print!("ErrorKind"),
                nom::error::ErrorKind::TagClosure => print!("ErrorKind"),
                nom::error::ErrorKind::Alpha => print!("ErrorKind"),
                nom::error::ErrorKind::Digit => print!("ErrorKind"),
                nom::error::ErrorKind::HexDigit => print!("ErrorKind"),
                nom::error::ErrorKind::OctDigit => print!("ErrorKind"),
                nom::error::ErrorKind::AlphaNumeric => print!("ErrorKind"),
                nom::error::ErrorKind::Space => print!("ErrorKind"),
                nom::error::ErrorKind::MultiSpace => print!("ErrorKind"),
                nom::error::ErrorKind::LengthValueFn => print!("ErrorKind"),
                nom::error::ErrorKind::Eof => print!("ErrorKind"),
                nom::error::ErrorKind::Switch => print!("ErrorKind"),
                nom::error::ErrorKind::TagBits => print!("ErrorKind"),
                nom::error::ErrorKind::OneOf => print!("ErrorKind"),
                nom::error::ErrorKind::NoneOf => print!("ErrorKind"),
                nom::error::ErrorKind::Char => print!("ErrorKind"),
                nom::error::ErrorKind::CrLf => print!("ErrorKind"),
                nom::error::ErrorKind::RegexpMatch => print!("ErrorKind"),
                nom::error::ErrorKind::RegexpMatches => print!("ErrorKind"),
                nom::error::ErrorKind::RegexpFind => print!("ErrorKind"),
                nom::error::ErrorKind::RegexpCapture => print!("ErrorKind"),
                nom::error::ErrorKind::RegexpCaptures => print!("ErrorKind"),
                nom::error::ErrorKind::TakeWhile1 => print!("ErrorKind"),
                nom::error::ErrorKind::Complete => print!("ErrorKind"),
                nom::error::ErrorKind::Fix => print!("ErrorKind"),
                nom::error::ErrorKind::Escaped => print!("ErrorKind"),
                nom::error::ErrorKind::EscapedTransform => print!("ErrorKind"),
                nom::error::ErrorKind::NonEmpty => print!("ErrorKind"),
                nom::error::ErrorKind::ManyMN => print!("ErrorKind"),
                nom::error::ErrorKind::Not => print!("ErrorKind"),
                nom::error::ErrorKind::Permutation => print!("ErrorKind"),
                nom::error::ErrorKind::Verify => print!("ErrorKind"),
                nom::error::ErrorKind::TakeTill1 => print!("ErrorKind"),
                nom::error::ErrorKind::TakeWhileMN => print!("ErrorKind"),
                nom::error::ErrorKind::TooLarge => print!("ErrorKind"),
                nom::error::ErrorKind::Many0Count => print!("ErrorKind"),
                nom::error::ErrorKind::Many1Count => print!("ErrorKind"),
                nom::error::ErrorKind::Float => print!("ErrorKind"),
                nom::error::ErrorKind::Satisfy => print!("ErrorKind"),
                nom::error::ErrorKind::Fail => print!("ErrorKind"),
            },
            StackContext::Context(a) => print!("{}", a),
        }
    }

    fn dbg_ln(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            StackContext::Kind(a) => match a {
                nom::error::ErrorKind::Tag => println!("ErrorKind"),
                nom::error::ErrorKind::MapRes => println!("ErrorKind"),
                nom::error::ErrorKind::MapOpt => println!("ErrorKind"),
                nom::error::ErrorKind::Alt => println!("ErrorKind"),
                nom::error::ErrorKind::IsNot => println!("ErrorKind"),
                nom::error::ErrorKind::IsA => println!("ErrorKind"),
                nom::error::ErrorKind::SeparatedList => println!("ErrorKind"),
                nom::error::ErrorKind::SeparatedNonEmptyList => println!("ErrorKind"),
                nom::error::ErrorKind::Many0 => println!("ErrorKind"),
                nom::error::ErrorKind::Many1 => println!("ErrorKind"),
                nom::error::ErrorKind::ManyTill => println!("ErrorKind"),
                nom::error::ErrorKind::Count => println!("ErrorKind"),
                nom::error::ErrorKind::TakeUntil => println!("ErrorKind"),
                nom::error::ErrorKind::LengthValue => println!("ErrorKind"),
                nom::error::ErrorKind::TagClosure => println!("ErrorKind"),
                nom::error::ErrorKind::Alpha => println!("ErrorKind"),
                nom::error::ErrorKind::Digit => println!("ErrorKind"),
                nom::error::ErrorKind::HexDigit => println!("ErrorKind"),
                nom::error::ErrorKind::OctDigit => println!("ErrorKind"),
                nom::error::ErrorKind::AlphaNumeric => println!("ErrorKind"),
                nom::error::ErrorKind::Space => println!("ErrorKind"),
                nom::error::ErrorKind::MultiSpace => println!("ErrorKind"),
                nom::error::ErrorKind::LengthValueFn => println!("ErrorKind"),
                nom::error::ErrorKind::Eof => println!("ErrorKind"),
                nom::error::ErrorKind::Switch => println!("ErrorKind"),
                nom::error::ErrorKind::TagBits => println!("ErrorKind"),
                nom::error::ErrorKind::OneOf => println!("ErrorKind"),
                nom::error::ErrorKind::NoneOf => println!("ErrorKind"),
                nom::error::ErrorKind::Char => println!("ErrorKind"),
                nom::error::ErrorKind::CrLf => println!("ErrorKind"),
                nom::error::ErrorKind::RegexpMatch => println!("ErrorKind"),
                nom::error::ErrorKind::RegexpMatches => println!("ErrorKind"),
                nom::error::ErrorKind::RegexpFind => println!("ErrorKind"),
                nom::error::ErrorKind::RegexpCapture => println!("ErrorKind"),
                nom::error::ErrorKind::RegexpCaptures => println!("ErrorKind"),
                nom::error::ErrorKind::TakeWhile1 => println!("ErrorKind"),
                nom::error::ErrorKind::Complete => println!("ErrorKind"),
                nom::error::ErrorKind::Fix => println!("ErrorKind"),
                nom::error::ErrorKind::Escaped => println!("ErrorKind"),
                nom::error::ErrorKind::EscapedTransform => println!("ErrorKind"),
                nom::error::ErrorKind::NonEmpty => println!("ErrorKind"),
                nom::error::ErrorKind::ManyMN => println!("ErrorKind"),
                nom::error::ErrorKind::Not => println!("ErrorKind"),
                nom::error::ErrorKind::Permutation => println!("ErrorKind"),
                nom::error::ErrorKind::Verify => println!("ErrorKind"),
                nom::error::ErrorKind::TakeTill1 => println!("ErrorKind"),
                nom::error::ErrorKind::TakeWhileMN => println!("ErrorKind"),
                nom::error::ErrorKind::TooLarge => println!("ErrorKind"),
                nom::error::ErrorKind::Many0Count => println!("ErrorKind"),
                nom::error::ErrorKind::Many1Count => println!("ErrorKind"),
                nom::error::ErrorKind::Float => println!("ErrorKind"),
                nom::error::ErrorKind::Satisfy => println!("ErrorKind"),
                nom::error::ErrorKind::Fail => println!("ErrorKind"),
            },
            StackContext::Context(a) => println!("{}", a),
        }
    }
}

impl<T,U> SimpleDebug for (T, U)
where
    T: SimpleDebug,
    U: SimpleDebug
{
    fn dbg(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        print!("(");
        self.0.dbg(0);
        print!(", ");
        self.1.dbg(0);
        print!(")");
    }

    fn dbg_ln(&self, indentation: i32) {
        println!("(");
        self.0.dbg(indentation+1);
        println!(",");
        self.1.dbg_ln(indentation+1);
        for _ in 0..indentation {
            print!("  ");
        };
        println!(")");
    }
}

impl SimpleDebug for Expr {
    fn dbg(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            Expr::Error => print!("E::Error"),
            Expr::Value { .. } => print!("E::Value"),
            Expr::ExprList { list } => {
                print!("E::ExprList(");
                for (i, _) in list {
                    i.dbg(0);
                    print!(", ");
                };
                print!(")")
            },
            Expr::UnnamedParameter { .. } => print!("E::UnnamedParameter"),
            Expr::Local { .. } => print!("E::Local"),
            Expr::Unary { .. } => print!("E::Unary"),
            Expr::Binary { .. } => print!("E::Binary"),
            Expr::Ternary { .. } => print!("E::Ternary"),
            Expr::Call { .. } => print!("E::Call"),
            Expr::If { .. } => print!("E::If"),
            Expr::IfBlock { .. } => print!("E:IfBlock"),
            Expr::Definition { .. } => print!("E::Definition"),
            Expr::BitFieldEntry { .. } => print!("E::BitFieldEntry"),
            Expr::EnumEntry { .. } => print!("E::EnumEntry"),
            Expr::NamespaceAccess { .. } => print!("E::NamespaceAccess"),
            Expr::Using { .. } => print!("E::Using"),
            Expr::Return { .. } => print!("E::Return"),
            Expr::Continue => print!("E::Continue"),
            Expr::Break => print!("E::Break"),
            Expr::Func { .. } => print!("E::Func"),
            Expr::Struct { .. } => print!("E::Struct"),
            Expr::Namespace { .. } => print!("E::Namespace"),
            Expr::Enum { .. } => print!("E::Enum"),
            Expr::Bitfield { .. } => print!("E::BitField"),
            Expr::Access { .. } => print!("E::Access"),
            Expr::Attribute { .. } => print!("E::Attribute"),
            Expr::AttributeArgument { .. } => print!("E::AttributeArgument"),
            Expr::WhileLoop { .. } => print!("E::WhileLoop"),
            Expr::ForLoop { .. } => print!("E::ForLoop"),
            Expr::Cast { .. } => print!("E::Cast"),
            Expr::Union { .. } => print!("E::Union"),
        }
    }

    fn dbg_ln(&self, indentation: i32) {
        for _ in 0..indentation {
            print!("  ");
        };
        match self {
            Expr::Error => println!("E::Error"),
            Expr::Value { .. } => println!("E::Value"),
            Expr::ExprList { list } => {
                println!("E::ExprList(");
                for (i, _) in list {
                    i.dbg(indentation+1);
                    println!(",")
                };
                for _ in 0..indentation {
                    print!("  ");
                };
                println!(")")
            },
            Expr::UnnamedParameter { .. } => println!("E::UnnamedParameter"),
            Expr::Local { .. } => println!("E::Local"),
            Expr::Unary { .. } => println!("E::Unary"),
            Expr::Binary { .. } => println!("E::Binary"),
            Expr::Ternary { .. } => println!("E::Ternary"),
            Expr::Call { .. } => println!("E::Call"),
            Expr::If { .. } => println!("E::If"),
            Expr::IfBlock { .. } => println!("E::IfBlock"),
            Expr::Definition { .. } => println!("E::Definition"),
            Expr::BitFieldEntry { .. } => println!("E::BitFieldEntry"),
            Expr::EnumEntry { .. } => println!("E::EnumEntry"),
            Expr::NamespaceAccess { .. } => println!("E::NamespaceAccess"),
            Expr::Using { .. } => println!("E::Using"),
            Expr::Return { .. } => println!("E::Return"),
            Expr::Continue => println!("E::Continue"),
            Expr::Break => println!("E::Break"),
            Expr::Func { .. } => println!("E::Func"),
            Expr::Struct { .. } => println!("E::Struct"),
            Expr::Namespace { .. } => println!("E::Namespace"),
            Expr::Enum { .. } => println!("E::Enum"),
            Expr::Bitfield { .. } => println!("E::BitField"),
            Expr::Access { .. } => println!("E::Access"),
            Expr::Attribute { .. } => println!("E::Attribute"),
            Expr::AttributeArgument { .. } => println!("E::AttributeArgument"),
            Expr::WhileLoop { .. } => println!("E::WhileLoop"),
            Expr::ForLoop { .. } => println!("E::ForLoop"),
            Expr::Cast { .. } => println!("E::Cast"),
            Expr::Union { .. } => println!("E::Union"),
        }
    }
}
