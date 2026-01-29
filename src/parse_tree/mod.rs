use crate::{lexer::interner::Symbol, source_manager::loc::Span};

pub mod expression;
pub mod pattern;
pub mod structure;
pub mod type_declaration;
pub mod type_expr;

#[derive(Debug)]
pub struct Located<T> {
    pub desc: T,
    pub span: Span,
}

impl<T> Located<T> {
    pub fn new(desc: T, span: Span) -> Self {
        Self { desc, span }
    }
}

#[derive(Debug)]
pub enum LongIdent {
    Ident(Symbol), // x
    Dot(Box<LongIdent>, Symbol), // M.x
                   // Apply(Box<LongIdent>, Box<LongIdent>), // F(x) Functor application
}

impl LongIdent {
    pub fn last_symbol(&self) -> &Symbol {
        match self {
            LongIdent::Ident(symbol) | LongIdent::Dot(_, symbol) => symbol,
        }
    }
}

#[derive(Debug)]
pub struct RecordField<T> {
    pub name: LongIdent,
    pub pat: T,
}

#[derive(Debug)]
pub enum ArgLabel<T = ()> {
    NoLabel,             // f x
    Labelled(Symbol, T), // f ~x
    Optional(Symbol, T), // f ?x
}
