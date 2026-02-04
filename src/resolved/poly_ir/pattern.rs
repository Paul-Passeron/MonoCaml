use crate::{
    lexer::interner::{StrLit, Symbol},
    resolved::{poly_ir::types::Type, uniqueness},
    source_manager::loc::Span,
};

#[derive(Debug, Clone)]
pub struct VarIdMarker;

pub type VarId = uniqueness::Uniklon<VarIdMarker>;

#[derive(Debug, Clone)]
pub enum PatternKind {
    Wildcard,
    Var {
        name: Symbol,
        id: VarId,
    },
    Literal(Literal),
    Constructor {
        path: Symbol,
        arg: Option<Box<Pattern>>,
    },
    Tuple(Vec<Pattern>),
    Record {
        fields: Vec<(Symbol, Pattern)>,
    },
    Or(Box<Pattern>, Box<Pattern>),
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Char(char),
    String(StrLit),
    Bool(bool),
}
