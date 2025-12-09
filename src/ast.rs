#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Symbol(String),
    Dict(Vec<(String, Self)>),
    Tuple(Vec<Self>),
    Literal(Literal),
    List(Vec<Self>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(u64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub pat: Pattern,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    And,
    Or,
    Cons,
}

#[derive(Debug, Clone)]
pub enum Expression {
    // string literal
    StrLit(String),
    // integer literal
    IntLit(u64),
    // float literal
    FloatLit(f64),
    // char literal
    CharLit(char),
    // bool literal
    BoolLit(bool),
    // variable reference
    Var(String),
    // a, b, c, d
    Tuple(Vec<Self>),
    // [a; b; c; d]
    List(Vec<Self>),
    // expr expr
    Call {
        fun: Box<Self>,
        arg: Box<Self>,
    },
    // let pattern = expr in expr
    Let {
        recursive: bool,
        binding: Binding,
        in_expr: Box<Self>,
    },
    // expr; expr
    Seq {
        fst: Box<Self>,
        snd: Box<Self>,
    },
    // if expr then expr else expr
    If {
        cond: Box<Self>,
        then_branch: Box<Self>,
        else_branch: Box<Self>,
    },
    // fun pattern => expr
    Lambda {
        param: Pattern,
        body: Box<Self>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Self>,
        right: Box<Self>,
    },
}
