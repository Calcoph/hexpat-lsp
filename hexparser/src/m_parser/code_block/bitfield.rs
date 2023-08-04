use crate::{token::{Spanned, Tokens}, Expr, recovery_err::TokResult, m_parser::bitfield_entry};

pub(crate) fn conditional<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    super::conditional(input, bitfield_entry)
}

pub(crate) fn match_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    super::match_statement(input, statement_body)
}

pub(crate) fn try_catch<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    super::try_catch(input, bitfield_entry)
}

pub(crate) fn statement_body<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    super::statement_body(input, bitfield_entry)
}
