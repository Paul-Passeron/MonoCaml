use crate::poly_ir::spanned::Spanned;

pub type Pattern = Spanned<PatternNode>;

#[derive(Debug)]
pub enum PatternNode {
    Wildcard,
}
