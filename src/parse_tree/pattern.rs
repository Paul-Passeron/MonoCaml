use crate::{
    lexer::interner::Symbol,
    parse_tree::{Located, LongIdent, expression::Constant, type_expr::TypeExpr},
};

pub type Pattern = Located<PatternDesc>;

pub struct RecordFieldPattern {
    pub name: LongIdent,
    pub pat: Box<Pattern>,
}

pub enum PatternDesc {
    Any,                                         // _
    Var(Symbol),                                 // x
    Alias(Box<Pattern>, Symbol),                 // p as x
    Constant(Constant),                          // 1, "a", ...
    Interval { start: Constant, end: Constant }, // 1..5
    Tuple(Vec<Pattern>),                         // (p1, p2, ...)
    Construct(LongIdent, Option<Box<Pattern>>),  // C, C p, C (p1, ...)
    Record(Vec<RecordFieldPattern>),             // {l1 = p1; ...}
    Constraint(Box<Pattern>, TypeExpr),          // (p: t)
}
