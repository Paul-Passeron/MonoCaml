use crate::poly_ir::{VarId, spanned::Spanned, type_expr::Type};

pub type Pattern = Spanned<PatternNode>;

#[derive(Debug)]
pub enum PatternNode {
    Wildcard,
    Var(VarId),
    Tuple(Vec<Pattern>),
    Constraint(Box<Pattern>, Type),
}
