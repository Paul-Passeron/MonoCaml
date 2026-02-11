use crate::{
    lexer::interner::Symbol,
    parse_tree::expression::{BinaryOp, Constant, UnaryOp},
    poly_ir::{ValueRef, VarId, pattern::Pattern, spanned::Spanned, type_expr::Type},
};

pub type Expr = Spanned<ExprNode>;

#[derive(Debug)]
pub enum ExprNode {
    Var(ValueRef),
    Const(Constant),
    Let {
        recursive: bool,
        bindings: Vec<ValueBinding>,
        body: Box<Expr>,
    },

    Function {
        cases: Vec<MatchCase>,
    },
    Apply {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    Match {
        scrutinee: Box<Expr>,
        cases: Vec<MatchCase>,
    },
    Tuple(Vec<Expr>),
    Construct {
        path: ValueRef,
        arg: Option<Box<Expr>>,
    },
    Sequence {
        first: Box<Expr>,
        second: Box<Expr>,
    },
    Constraint {
        expr: Box<Expr>,
        ty: Type,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct ValueBinding {
    pub id: VarId,
    pub name: Symbol,
    pub params: Vec<Pattern>,
    pub ty: Option<Type>,
    pub body: Expr,
}

#[derive(Debug)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: Expr,
}
