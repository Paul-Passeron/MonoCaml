use crate::{
    lexer::{
        interner::{StrLit, Symbol},
        token::TokenKind,
    },
    parse_tree::{
        ArgLabel, Located, LongIdent, RecordField, pattern::Pattern, type_expr::TypeExpr,
    },
    source_manager::loc::Span,
};

#[derive(Debug)]
pub enum Constant {
    Int(i64),
    Char(char),
    String(StrLit),
    Float(f64),
}

pub type Expression = Located<ExpressionDesc>;

#[derive(Debug)]
pub enum RecFlag {
    Recursive,
    NonRecursive,
}

#[derive(Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Div,
    Eq,
    NEq,
    LT,
    GT,
    GEq,
    LEq,
    LOr,
    LAnd,
    Cons,
    Custom(Symbol),
}

impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Star => Ok(Self::Star),
            TokenKind::Div => Ok(Self::Div),
            TokenKind::Eq => Ok(Self::Eq),
            TokenKind::NEq => Ok(Self::NEq),
            TokenKind::LT => Ok(Self::LT),
            TokenKind::GT => Ok(Self::GT),
            TokenKind::GEq => Ok(Self::GEq),
            TokenKind::LEq => Ok(Self::LEq),
            TokenKind::LOr => Ok(Self::LOr),
            TokenKind::LAnd => Ok(Self::LAnd),
            TokenKind::Cons => Ok(Self::Cons),
            TokenKind::Op(x) => Ok(Self::Custom(x)),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
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
    Construct(LongIdent, Option<Box<Expression>>),
    Record(Vec<RecordField<Box<Expression>>>),
    Field(Box<Expression>, LongIdent),
    Application(Box<Expression>, Box<Expression>),
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
    Product(Box<Expression>, Box<Expression>),
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
    Unit,
    Paren(Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
    List(Vec<Expression>),
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

    pub fn let_in(rec: RecFlag, bindings: Vec<ValueBinding>, expr: Expression) -> Self {
        Self::Let {
            rec,
            bindings,
            expression: Box::new(expr),
        }
    }

    pub fn application(fun: Expression, arg: Expression) -> Self {
        Self::Application(Box::new(fun), Box::new(arg))
    }

    pub fn construct(c: LongIdent, args: Option<Expression>) -> Self {
        Self::Construct(c, args.map(Box::new))
    }

    pub fn paren(e: Expression) -> Self {
        Self::Paren(Box::new(e))
    }

    // pub fn tuple(v: Vec<Expression>) -> Self {
    //     Self::Tuple(v)
    // }

    pub fn binary_op(op: TokenKind, left: Expression, right: Expression) -> Self {
        match op {
            TokenKind::Comma => Self::Product(Box::new(left), Box::new(right)),
            TokenKind::Semi => Self::Sequence(Box::new(left), Box::new(right)),
            _ => {
                let op = op.try_into().unwrap();
                Self::BinaryOp(op, Box::new(left), Box::new(right))
            }
        }
    }

    pub fn seq(fst: Expression, snd: Expression) -> Self {
        Self::Sequence(Box::new(fst), Box::new(snd))
    }

    pub fn match_with(expr: Expression, with: Vec<Case>) -> Self {
        Self::Match {
            expr: Box::new(expr),
            with,
        }
    }
}

#[derive(Debug)]
pub enum DirectionFlag {
    Upto,
    Downto,
}

#[derive(Debug)]
pub struct ValueBinding {
    pub pat: Pattern,                        // The pattern being bound
    pub args: Vec<Pattern>,                  // The arguments (can be 0) ex: let x y = 1 + y
    pub expr: Box<Expression>,               // The expression
    pub constraint: Option<ValueConstraint>, // Optional type constraint
    pub span: Span,
}

impl ValueBinding {
    pub fn new(
        pat: Pattern,
        args: Vec<Pattern>,
        expr: Expression,
        constraint: Option<ValueConstraint>,
        span: Span,
    ) -> Self {
        Self {
            pat,
            args,
            expr: Box::new(expr),
            constraint,
            span,
        }
    }
}

#[derive(Debug)]
pub struct ValueConstraint {
    pub locally_abstract_types: Vec<Symbol>,
    pub typ: TypeExpr,
}

#[derive(Debug)]
pub struct Case {
    pub lhs: Pattern,
    pub guard: Option<Expression>,
    pub expr: Expression,
}

impl Case {
    pub fn new(lhs: Pattern, guard: Option<Expression>, expr: Expression) -> Self {
        Self { lhs, guard, expr }
    }
}
