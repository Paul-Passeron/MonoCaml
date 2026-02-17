use crate::poly_ir::{VarId, spanned::Spanned};

pub type Pattern = Spanned<PatternNode>;

#[derive(Debug)]
pub enum PatternNode {
    Wildcard,
    Var(VarId),
    Tuple(Vec<Pattern>),
}
