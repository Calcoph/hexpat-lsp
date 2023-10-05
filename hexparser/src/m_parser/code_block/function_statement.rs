use crate::{m_parser::{function::function_statement, Statement}, token::{Tokens, Spanned}, Expr, recovery_err::TokResult};

pub(crate) fn statement_body<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Vec<Spanned<Statement>>>> {
    super::statement_body(input, function_statement)
}

pub(crate) fn conditional<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    super::conditional(input, function_statement)
}

pub(crate) fn match_statement<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    super::match_statement(input, statement_body)
}

pub(crate) fn try_catch<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Statement>> {
    super::try_catch(input, function_statement)
}
