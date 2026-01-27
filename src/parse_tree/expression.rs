use crate::{
    lexer::interner::{StrLit, Symbol},
    parse_tree::{Located, LongIdent, pattern::Pattern, type_expr::TypeExpr},
    source_manager::loc::Loc,
};

pub enum Constant {
    Int(i64),
    Char(char),
    String(StrLit),
    Float(f64),
}

pub type Expression = Located<ExpressionDesc>;

pub enum ExpressionDesc {
    Ident(LongIdent),
    Constant(Constant),
}

pub struct ValueBinding {
    pub pat: Pattern,                        // The pattern being bound
    pub expr: Box<Expression>,               // The expression
    pub constraint: Option<ValueConstraint>, // Optional type constraint
    pub loc: Loc,
}

pub struct ValueConstraint {
    pub locally_abstract_types: Vec<Symbol>,
    pub typ: TypeExpr,
}
