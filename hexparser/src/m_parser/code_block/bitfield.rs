use crate::{token::{Spanned, Tokens}, Expr, recovery_err::TokResult, m_parser::bitfield_entry};

pub(crate) fn conditional<'a, 'b>(input: Tokens<'a, 'b>) -> TokResult<'a, 'b, Spanned<Expr>> {
    super::conditional(input, bitfield_entry)
}
