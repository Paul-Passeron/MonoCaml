use crate::{
    lexer::interner::Symbol,
    resolved::poly_ir::{
        pattern::{Literal, Pattern, VarId},
        types::Type,
    },
    source_manager::loc::Span,
};

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Literal),
    Var(VarId),
    Lambda {
        param: Pattern,
        body: Box<Expr>,
    },
    Apply {
        func: Box<Expr>,
        arg: Box<Expr>,
    },
    Let {
        rec: bool,
        bindings: Vec<LetBinding>,
        body: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_: Box<Expr>,
        else_: Box<Expr>, // If Is meant to not be present, just use unit
    },

    Match {
        matched: Box<Expr>,
        cases: Vec<MatchCase>,
    },
    Tuple(Vec<Expr>),
    Record {
        // TODO: add optional base expr
        fields: Vec<(Symbol, Expr)>,
    },
    Constructor {
        cons: Symbol,
        arg: Option<Box<Expr>>,
    },
    Seq(Box<Expr>, Box<Expr>),
    BinOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    Error,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LetBinding {
    pub pattern: Pattern,
    pub params: Vec<Pattern>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}
