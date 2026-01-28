use crate::{
    lexer::interner::{StrLit, Symbol},
    parse_tree::{
        ArgLabel, Located, LongIdent, RecordField, pattern::Pattern, type_expr::TypeExpr,
    },
    source_manager::loc::Loc,
};

pub enum Constant {
    Int(i64),
    Char(char),
    String(StrLit),
    Float(f64),
}

pub type Expression = Located<ExpressionDesc>;

pub enum RecFlag {
    Recursive,
    NonRecursive,
}

pub enum ExpressionDesc {
    Ident(LongIdent),
    Constant(Constant),
    Let {
        rec: RecFlag,
        bindings: Vec<ValueBinding>,
        expression: Box<Expression>,
    },
    Function(Vec<Case>),
    Match {
        expr: Box<Expression>,
        with: Vec<Case>,
    },
    Fun {
        arg: ArgLabel<Option<Box<Expression>>>,
        pat: Pattern,
        body: Box<Expression>,
    },
    Tuple(Vec<Expression>),
    Construct(LongIdent, Option<Box<Expression>>),
    Record(Vec<RecordField<Box<Expression>>>),
    Field(Box<Expression>, LongIdent),

    // t.f <- v
    SetField {
        target: Box<Expression>,
        field: LongIdent,
        value: Box<Expression>,
    },
    IfThenElse {
        cond: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Option<Box<Expression>>,
    },
    Sequence(Box<Expression>, Box<Expression>),
    While {
        cond: Box<Expression>,
        body: Box<Expression>,
    },
    For {
        pat: Pattern,
        start: Box<Expression>,
        end: Box<Expression>,
        dir: DirectionFlag,
        body: Box<Expression>,
    },
    Constraint(Box<Expression>, TypeExpr),
}

pub enum DirectionFlag {
    Upto,
    Downto,
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

pub struct Case {
    pub lhs: Pattern,
    pub guard: Option<Expression>,
    pub expr: Expression,
}

impl ExpressionDesc {
    pub fn if_then_else(
        cond: Expression,
        then_expr: Expression,
        else_expr: Option<Expression>,
    ) -> Self {
        Self::IfThenElse {
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: else_expr.map(Box::new),
        }
    }
}
