use std::collections::HashMap;

use chumsky::Span;
use im_rc::Vector;
use crate::chumsky::m_parser::{Spanned, func::Func, Expr, NamedASTNode, NormalASTNode};

#[derive(Debug, Clone)]
pub enum ReferenceSymbol {
    Founded(Spanned<String>),
    Founding(usize),
}

use ReferenceSymbol::*;

pub fn get_reference(
    ast: &(HashMap<String, NamedASTNode>, Vec<NormalASTNode>),
    ident_offset: usize,
    include_self: bool,
) -> Vec<Spanned<String>> {
    let mut vector = Vector::new();
    let mut reference_list = vec![];
    // for (_, v) in ast.iter() {
    //     if v.name.1.end < ident_offset {
    //         vector.push_back(v.name.clone());
    //     }
    // }
    let mut kv_list = ast.0.iter().collect::<Vec<_>>();
    kv_list.sort_by(|a, b| a.1.getname().start().cmp(&b.1.getname().start()));
    let mut reference_symbol = ReferenceSymbol::Founding(ident_offset);
    // let mut fn_vector = Vector::new();
    for (_, v) in kv_list {
        match v {
            NamedASTNode::Func(v) => {
                let (_, range) = &v.name;
                if ident_offset >= range.start && ident_offset < range.end {
                    reference_symbol = ReferenceSymbol::Founded(v.name.clone());
                    if include_self {
                        reference_list.push(v.name.clone());
                    }
                };
                vector.push_back(v.name.clone());
                let args = v
                    .args
                    .iter()
                    .map(|arg| {
                        if ident_offset >= arg.1.start && ident_offset < arg.1.end {
                            reference_symbol = ReferenceSymbol::Founded(arg.clone());
                            if include_self {
                                reference_list.push(arg.clone());
                            }
                        }
                        arg.clone()
                    })
                    .collect::<Vector<_>>();
                get_reference_of_expr(
                    &v.body,
                    args + vector.clone(),
                    reference_symbol.clone(),
                    &mut reference_list,
                    include_self,
                );
            },
            NamedASTNode::Struct(_) => todo!(),
            NamedASTNode::Enum(_) => todo!(),
            NamedASTNode::Namespace(_) => todo!(),
            NamedASTNode::Bitfield(_) => todo!(),
        }
    }
    reference_list
}

pub fn get_reference_of_expr(
    expr: &Spanned<Expr>,
    definition_ass_list: Vector<Spanned<String>>,
    reference_symbol: ReferenceSymbol,
    reference_list: &mut Vec<Spanned<String>>,
    include_self: bool,
) {
    match &expr.0 {
        Expr::Error => {}
        Expr::Value(_) => {}
        Expr::Local((name, span)) => {
            if let Founded((symbol_name, symbol_span)) = reference_symbol {
                if &symbol_name == name {
                    let index = definition_ass_list
                        .iter()
                        .position(|decl| decl.0 == symbol_name);
                    if let Some(symbol) = index.map(|i| definition_ass_list.get(i).unwrap().clone())
                    {
                        if symbol == (symbol_name, symbol_span) {
                            reference_list.push((name.clone(), span.clone()));
                        }
                    };
                }
            }
            // if ident_offset >= local.1.start && ident_offset < local.1.end {
            //     let index = definition_ass_list
            //         .iter()
            //         .position(|decl| decl.0 == local.0);
            //     (
            //         false,
            //         index.map(|i| definition_ass_list.get(i).unwrap().clone()),
            //     )
            // } else {
            //     (true, None)
            // }
        }
        Expr::Then(first, second) => {
            get_reference_of_expr(
                first,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                second,
                definition_ass_list.clone(),
                reference_symbol,
                reference_list,
                include_self,
            );
        }
        Expr::Binary(lhs, _op, rhs) => {
            get_reference_of_expr(
                lhs,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                rhs,
                definition_ass_list.clone(),
                reference_symbol,
                reference_list,
                include_self,
            );
        }
        Expr::Call(callee, args) => {
            get_reference_of_expr(
                callee,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            for expr in &args.0 {
                get_reference_of_expr(
                    &expr,
                    definition_ass_list.clone(),
                    reference_symbol.clone(),
                    reference_list,
                    include_self,
                );
            }
        }
        Expr::If(test, consequent, alternative) => {
            get_reference_of_expr(
                test,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                consequent,
                definition_ass_list.clone(),
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
            get_reference_of_expr(
                alternative,
                definition_ass_list,
                reference_symbol.clone(),
                reference_list,
                include_self,
            );
        }
        Expr::Definition(_, _, _, _, _) => todo!(),
    }
}
