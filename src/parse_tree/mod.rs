use crate::{lexer::interner::Symbol, source_manager::loc::Loc};

pub mod expression;
pub mod pattern;
pub mod type_expr;

pub struct Located<T> {
    pub desc: T,
    pub loc: Loc,
}

pub enum LongIdent {
    Ident(Symbol),                         // x
    Dot(Box<LongIdent>, Symbol),           // M.x
    Apply(Box<LongIdent>, Box<LongIdent>), // F(x) Functor application
}

pub struct RecordField<T> {
    pub name: LongIdent,
    pub pat: T,
}

pub enum ArgLabel<T = ()> {
    NoLabel,             // f x
    Labelled(Symbol, T), // f ~x
    Optional(Symbol, T), // f ?x
}
