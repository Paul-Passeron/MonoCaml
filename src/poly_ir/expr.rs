use crate::{
    parse_tree::expression::{BinaryOp, Constant, UnaryOp},
    poly_ir::{ValueRef, id::Id, pattern::Pattern, spanned::TypedNode},
};

pub type Expr<T> = TypedNode<ExprNode<T>, T>;

#[derive(Debug, Clone)]
pub enum ExprNode<T> {
    Var(ValueRef),
    Const(Constant),
    Let {
        recursive: bool,
        bindings: Vec<ValueBinding<T>>,
        body: Box<Expr<T>>,
    },

    // Function {
    //     cases: Vec<MatchCase<T>>,
    // },
    Apply {
        func: Box<Expr<T>>,
        arg: Box<Expr<T>>,
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
    Unit,
    IfThenElse {
        cond: Box<Expr<T>>,
        then_expr: Box<Expr<T>>,
        else_expr: Box<Expr<T>>,
    },
    // Fun {
    //     arg: Pattern<T>,
    //     body: Box<Expr<T>>,
    // },
}
#[derive(Debug, Default, Clone, Copy)]
pub struct VBMarker;
pub type ValueBindingId = Id<VBMarker>;

#[derive(Debug, Clone)]
pub struct ValueBinding<T> {
    pub id: ValueBindingId,
    pub pat: Pattern<T>,
    pub params: Vec<Pattern<T>>,
    pub ty: T,
    pub body: Expr<T>,
}

#[derive(Debug, Clone)]
pub struct MatchCase<T> {
    pub pattern: Pattern<T>,
    pub guard: Option<Box<Expr<T>>>,
    pub body: Expr<T>,
}
