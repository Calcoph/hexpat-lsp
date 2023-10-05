use crate::{token::{Spanned, Tokens}, Expr, recovery_err::TokResult, m_parser::{bitfield_entry, Statement}};

pub(crate) fn conditional<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    super::conditional(input, bitfield_entry)
}

pub(crate) fn match_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    super::match_statement(input, statement_body)
}

pub(crate) fn try_catch<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    super::try_catch(input, bitfield_entry)
}

pub(crate) fn statement_body<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Vec<Spanned<Statement>>>> {
    super::statement_body(input, bitfield_entry)
}
