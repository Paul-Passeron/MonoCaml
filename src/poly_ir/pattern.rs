use crate::poly_ir::{VarId, spanned::TypedNode};

pub type Pattern<T> = TypedNode<PatternNode<T>, T>;

#[derive(Debug, Clone)]
pub enum PatternNode<T> {
    Wildcard,
    Var(VarId),
    Tuple(Vec<Pattern<T>>),
}
