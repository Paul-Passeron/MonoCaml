use crate::{
    lexer::interner::Symbol,
    parse_tree::{Located, LongIdent, RecordField, expression::Constant, type_expr::TypeExpr},
};

pub type Pattern = Located<PatternDesc>;

#[derive(Debug)]
pub enum PatternDesc {
    Any,                                         // _
    Var(Symbol),                                 // x
    Alias(Box<Pattern>, Symbol),                 // p as x
    Constant(Constant),                          // 1, "a", ...
    Interval { start: Constant, end: Constant }, // 1..5
    Tuple(Vec<Pattern>),                         // (p1, p2, ...)
    Construct(LongIdent, Option<Box<Pattern>>),  // C, C p, C (p1, ...)
    Record(Vec<RecordField<Box<Pattern>>>),      // {l1 = p1; ...}
    Constraint(Box<Pattern>, TypeExpr),          // (p: t)
}
