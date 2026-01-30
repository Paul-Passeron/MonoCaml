use crate::{
    lexer::interner::Symbol,
    parse_tree::{
        Located, LongIdent, RecordField,
        expression::{BinaryOp, Constant},
        type_expr::TypeExpr,
    },
};

pub type Pattern = Located<PatternDesc>;

#[derive(Debug)]
pub enum PatternDesc {
    Any, // _
    // TODO: Var takes ident symbol
    Var(Symbol),                                    // x
    Alias(Box<Pattern>, Symbol),                    // p as x
    Constant(Constant),                             // 1, "a", ...
    Interval { start: Constant, end: Constant },    // 1..5
    Tuple(Vec<Pattern>),                            // (p1, p2, ...)
    Construct(LongIdent, Option<Box<Pattern>>),     // C, C p, C (p1, ...)
    Record(Vec<RecordField<Box<Pattern>>>),         // {l1 = p1; ...}
    Constraint(Box<Pattern>, TypeExpr),             // (p: t)
    Unit,                                           // ()
    Paren(Box<Pattern>),                            // (x)
    BinaryOp(BinaryOp, Box<Pattern>, Box<Pattern>), // a :: b
    List(Vec<Pattern>),
}

impl PatternDesc {
    pub fn paren(pat: Pattern) -> Self {
        Self::Paren(Box::new(pat))
    }

    pub fn tuple(pats: Vec<Pattern>) -> Self {
        Self::Tuple(pats)
    }

    pub fn constraint(pat: Pattern, te: TypeExpr) -> Self {
        Self::Constraint(Box::new(pat), te)
    }

    pub fn construct(cons: LongIdent, args: Option<Pattern>) -> Self {
        Self::Construct(cons, args.map(Box::new))
    }

    pub fn binary_op(op: BinaryOp, lhs: Pattern, rhs: Pattern) -> Self {
        Self::BinaryOp(op, Box::new(lhs), Box::new(rhs))
    }
}
