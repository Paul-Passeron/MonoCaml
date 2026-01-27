use crate::{lexer::interner::Symbol, source_manager::loc::Loc};

pub mod pattern;
pub mod type_expr;

pub struct Located<T> {
    pub desc: T,
    pub loc: Loc,
}

pub type Ident = Symbol;

pub enum LongIdent {
    Ident(Ident),                          // x
    Dot(Box<LongIdent>, Symbol),           // M.x
    Apply(Box<LongIdent>, Box<LongIdent>), // F(x) Functor application
}

pub enum Constant {}
