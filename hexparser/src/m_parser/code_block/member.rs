use crate::{m_parser::{member, Statement}, token::{Tokens, Spanned}, recovery_err::TokResult, Expr};

pub(crate) fn conditional<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    super::conditional(input, member)
}

pub(crate) fn match_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    super::match_statement(input, statement_body)
}

pub(crate) fn try_catch<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    super::try_catch(input, member)
}

pub(crate) fn statement_body<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Vec<Spanned<Statement>>>> {
    super::statement_body(input, member)
}
