use crate::{
    lexer::interner::Symbol,
    parse_tree::expression::{BinaryOp, Constant, UnaryOp},
    poly_ir::{ValueRef, VarId, pattern::Pattern, spanned::TypedNode},
};

pub type Expr<T> = TypedNode<ExprNode<T>, T>;

#[derive(Debug)]
pub enum ExprNode<T> {
    Var(ValueRef),
    Const(Constant),
    Let {
        recursive: bool,
        bindings: Vec<ValueBinding<T>>,
        body: Box<Expr<T>>,
    },

    Function {
        cases: Vec<MatchCase<T>>,
    },
    Apply {
        func: Box<Expr<T>>,
        args: Vec<Expr<T>>,
    },
    Match {
        scrutinee: Box<Expr<T>>,
        cases: Vec<MatchCase<T>>,
    },
    Tuple(Vec<Expr<T>>),
    Construct {
        path: ValueRef,
        arg: Option<Box<Expr<T>>>,
    },
    Sequence {
        first: Box<Expr<T>>,
        second: Box<Expr<T>>,
    },
    Constraint {
        expr: Box<Expr<T>>,
        ty: T,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr<T>>,
        right: Box<Expr<T>>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr<T>>,
    },
}

#[derive(Debug)]
pub struct ValueBinding<T> {
    pub id: VarId,
    pub name: Symbol,
    pub params: Vec<Pattern<T>>,
    pub ty: T,
    pub body: Expr<T>,
}

#[derive(Debug)]
pub struct MatchCase<T> {
    pub pattern: Pattern<T>,
    pub guard: Option<Box<Expr<T>>>,
    pub body: Expr<T>,
}
