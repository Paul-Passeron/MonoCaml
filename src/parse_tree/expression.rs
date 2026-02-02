use std::fmt;

use crate::{
    lexer::{
        interner::{StrLit, Symbol},
        token::TokenKind,
    },
    parse_tree::{Located, LongIdent, RecordField, pattern::Pattern, type_expr::TypeExpr},
    session::Session,
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
        arg: Pattern,
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

    pub fn fun(pat: Pattern, body: Expression) -> Self {
        Self::Fun {
            arg: pat,
            body: Box::new(body),
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

const INDENT: &'static str = "    ";

pub struct ExpressionDescDisplay<'a, 'b> {
    pub desc: &'a ExpressionDesc,
    pub session: &'b Session,
    pub indent: usize,
}

impl ExpressionDesc {
    pub fn display<'a, 'b>(
        &'a self,
        session: &'b Session,
        indent: usize,
    ) -> ExpressionDescDisplay<'a, 'b> {
        ExpressionDescDisplay {
            desc: self,
            session,
            indent,
        }
    }
}

pub struct LongIdentDisplay<'a, 'b> {
    pub ident: &'a LongIdent,
    pub session: &'b Session,
    pub indent: usize,
}

impl LongIdent {
    pub fn display<'a, 'b>(
        &'a self,
        session: &'b Session,
        indent: usize,
    ) -> LongIdentDisplay<'a, 'b> {
        LongIdentDisplay {
            ident: self,
            session,
            indent,
        }
    }
}

impl fmt::Display for LongIdentDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ident {
            LongIdent::Ident(symbol) => {
                write!(
                    f,
                    "{}{}",
                    INDENT.repeat(self.indent),
                    symbol.display(&self.session.symbol_interner)
                )
            }
            LongIdent::Dot(long_ident, symbol) => {
                write!(
                    f,
                    "{}{}.{}",
                    INDENT.repeat(self.indent),
                    long_ident.display(&self.session, 0),
                    symbol.display(&self.session.symbol_interner)
                )
            }
        }
    }
}

impl fmt::Display for ExpressionDescDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.desc {
            ExpressionDesc::Ident(long_ident) => {
                write!(f, "{}", long_ident.display(&self.session, self.indent))
            }
            ExpressionDesc::Constant(constant) => {
                write!(f, "{}", INDENT.repeat(self.indent))?;
                match constant {
                    Constant::Int(x) => write!(f, "{}", x),
                    Constant::Char(c) => write!(f, "'{}'", c),
                    Constant::String(s) => {
                        write!(f, "\"{}\"", self.session.resolve_strlit(*s))
                    }
                    Constant::Float(fl) => write!(f, "{}", fl),
                }
            }
            ExpressionDesc::Let {
                rec,
                bindings,
                expression,
            } => {
                write!(f, "{}", INDENT.repeat(self.indent))?;
                let mut iterator = bindings.iter();
                let first = iterator.next().expect("Malformed let expression desc");
                write!(
                    f,
                    "let{} {}",
                    match rec {
                        RecFlag::Recursive => " rec",
                        RecFlag::NonRecursive => "",
                    },
                    first.pat.desc.display(self.session, 0)
                )?;

                for arg in &first.args {
                    write!(f, " {}", arg.desc.display(self.session, 0))?;
                }

                if let Some(constraint) = &first.constraint {
                    write!(f, " : ")?;
                    if !constraint.locally_abstract_types.is_empty() {
                        write!(f, "type ")?;
                        for (i, ty) in constraint.locally_abstract_types.iter().enumerate() {
                            if i > 0 {
                                write!(f, " ")?;
                            }
                            write!(f, "'{}", ty.display(&self.session.symbol_interner))?;
                        }
                        write!(f, ". ")?;
                    }
                    write!(f, "{}", constraint.typ.desc.display(self.session, 0))?;
                }

                write!(f, " =\n")?;
                write!(
                    f,
                    "{}",
                    first.expr.desc.display(self.session, self.indent + 1)
                )?;

                for binding in iterator {
                    write!(
                        f,
                        "\n{}and {}",
                        INDENT.repeat(self.indent),
                        binding.pat.desc.display(self.session, 0)
                    )?;
                    for arg in &binding.args {
                        write!(f, " {}", arg.desc.display(self.session, 0))?;
                    }
                    if let Some(constraint) = &binding.constraint {
                        write!(f, " : ")?;
                        if !constraint.locally_abstract_types.is_empty() {
                            write!(f, "type ")?;
                            for (i, ty) in constraint.locally_abstract_types.iter().enumerate() {
                                if i > 0 {
                                    write!(f, " ")?;
                                }
                                write!(f, "'{}", ty.display(&self.session.symbol_interner))?;
                            }
                            write!(f, ". ")?;
                        }
                        write!(f, "{}", constraint.typ.desc.display(self.session, 0))?;
                    }
                    write!(f, " =\n")?;
                    write!(
                        f,
                        "{}",
                        binding.expr.desc.display(self.session, self.indent + 1)
                    )?;
                }

                write!(f, "\n{}in\n", INDENT.repeat(self.indent))?;
                write!(f, "{}", expression.desc.display(self.session, self.indent))
            }
            ExpressionDesc::Function(cases) => {
                write!(f, "{}function", INDENT.repeat(self.indent))?;
                for (i, case) in cases.iter().enumerate() {
                    if i == 0 {
                        write!(f, "\n")?;
                    } else {
                        write!(f, "\n")?;
                    }
                    write!(
                        f,
                        "{}| {}",
                        INDENT.repeat(self.indent),
                        case.lhs.desc.display(self.session, 0)
                    )?;
                    if let Some(guard) = &case.guard {
                        write!(f, " when {}", guard.desc.display(self.session, 0))?;
                    }
                    write!(f, " ->\n")?;
                    write!(
                        f,
                        "{}",
                        case.expr.desc.display(self.session, self.indent + 1)
                    )?;
                }
                Ok(())
            }
            ExpressionDesc::Match { expr, with } => {
                write!(f, "{}match ", INDENT.repeat(self.indent))?;
                write!(f, "{}", expr.desc.display(self.session, 0))?;
                write!(f, " with")?;
                for (i, case) in with.iter().enumerate() {
                    if i == 0 {
                        write!(f, "\n")?;
                    } else {
                        write!(f, "\n")?;
                    }
                    write!(
                        f,
                        "{}| {}",
                        INDENT.repeat(self.indent),
                        case.lhs.desc.display(self.session, 0)
                    )?;
                    if let Some(guard) = &case.guard {
                        write!(f, " when {}", guard.desc.display(self.session, 0))?;
                    }
                    write!(f, " ->\n")?;
                    write!(
                        f,
                        "{}",
                        case.expr.desc.display(self.session, self.indent + 1)
                    )?;
                }
                Ok(())
            }
            ExpressionDesc::Fun { arg, body } => {
                write!(f, "{}fun ", INDENT.repeat(self.indent))?;
                write!(f, "{} ->\n", arg.desc.display(self.session, 0))?;
                write!(f, "{}", body.desc.display(self.session, self.indent + 1))
            }
            ExpressionDesc::Construct(long_ident, None) => {
                write!(
                    f,
                    "{}{}",
                    INDENT.repeat(self.indent),
                    long_ident.display(self.session, 0)
                )
            }
            ExpressionDesc::Construct(long_ident, Some(expr)) => {
                write!(
                    f,
                    "{}{} ",
                    INDENT.repeat(self.indent),
                    long_ident.display(self.session, 0)
                )?;
                write!(f, "{}", expr.desc.display(self.session, 0))
            }
            ExpressionDesc::Record(record_fields) => {
                write!(f, "{}{{ ", INDENT.repeat(self.indent))?;
                for (i, field) in record_fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(
                        f,
                        "{} = {}",
                        field.name.display(self.session, 0),
                        field.pat.desc.display(self.session, 0)
                    )?;
                }
                write!(f, " }}")
            }
            ExpressionDesc::Field(expr, long_ident) => {
                write!(f, "{}", INDENT.repeat(self.indent))?;
                write!(
                    f,
                    "{}.{}",
                    expr.desc.display(self.session, 0),
                    long_ident.display(self.session, 0)
                )
            }
            ExpressionDesc::Application(fun, arg) => {
                write!(f, "{}", INDENT.repeat(self.indent))?;
                write!(
                    f,
                    "{} {}",
                    fun.desc.display(self.session, 0),
                    arg.desc.display(self.session, 0)
                )
            }
            ExpressionDesc::SetField {
                target,
                field,
                value,
            } => {
                write!(f, "{}", INDENT.repeat(self.indent))?;
                write!(
                    f,
                    "{}.{} <- {}",
                    target.desc.display(self.session, 0),
                    field.display(self.session, 0),
                    value.desc.display(self.session, 0)
                )
            }
            ExpressionDesc::IfThenElse {
                cond,
                then_expr,
                else_expr,
            } => {
                write!(f, "{}if ", INDENT.repeat(self.indent))?;
                write!(f, "{}", cond.desc.display(self.session, 0))?;
                write!(f, " then\n")?;
                write!(
                    f,
                    "{}",
                    then_expr.desc.display(self.session, self.indent + 1)
                )?;
                if let Some(else_expr) = else_expr {
                    write!(f, "\n{}else\n", INDENT.repeat(self.indent))?;
                    write!(
                        f,
                        "{}",
                        else_expr.desc.display(self.session, self.indent + 1)
                    )?;
                }
                Ok(())
            }
            ExpressionDesc::Product(e1, e2) => {
                write!(f, "{}", INDENT.repeat(self.indent))?;
                write!(
                    f,
                    "{}, {}",
                    e1.desc.display(self.session, 0),
                    e2.desc.display(self.session, 0)
                )
            }
            ExpressionDesc::Sequence(e1, e2) => {
                write!(f, "{}", e1.desc.display(self.session, self.indent))?;
                write!(f, ";\n")?;
                write!(f, "{}", e2.desc.display(self.session, self.indent))
            }
            ExpressionDesc::While { cond, body } => {
                write!(f, "{}while ", INDENT.repeat(self.indent))?;
                write!(f, "{}", cond.desc.display(self.session, 0))?;
                write!(f, " do\n")?;
                write!(f, "{}", body.desc.display(self.session, self.indent + 1))?;
                write!(f, "\n{}done", INDENT.repeat(self.indent))
            }
            ExpressionDesc::For {
                pat,
                start,
                end,
                dir,
                body,
            } => {
                write!(
                    f,
                    "{}for {} = ",
                    INDENT.repeat(self.indent),
                    pat.desc.display(self.session, 0)
                )?;
                write!(f, "{}", start.desc.display(self.session, 0))?;
                write!(
                    f,
                    " {} ",
                    match dir {
                        DirectionFlag::Upto => "to",
                        DirectionFlag::Downto => "downto",
                    }
                )?;
                write!(f, "{}", end.desc.display(self.session, 0))?;
                write!(f, " do\n")?;
                write!(f, "{}", body.desc.display(self.session, self.indent + 1))?;
                write!(f, "\n{}done", INDENT.repeat(self.indent))
            }
            ExpressionDesc::Constraint(e, t) => {
                write!(f, "{}", INDENT.repeat(self.indent))?;
                write!(
                    f,
                    "({} : {})",
                    e.desc.display(self.session, 0),
                    t.desc.display(self.session, 0)
                )
            }
            ExpressionDesc::Unit => write!(f, "{}()", INDENT.repeat(self.indent)),
            ExpressionDesc::Paren(expr) => {
                write!(
                    f,
                    "{}({})",
                    INDENT.repeat(self.indent),
                    expr.desc.display(self.session, 0)
                )
            }
            ExpressionDesc::BinaryOp(binary_op, left, right) => {
                write!(f, "{}", INDENT.repeat(self.indent))?;
                let op_str = match binary_op {
                    BinaryOp::Plus => "+",
                    BinaryOp::Minus => "-",
                    BinaryOp::Star => "*",
                    BinaryOp::Div => "/",
                    BinaryOp::Eq => "=",
                    BinaryOp::NEq => "<>",
                    BinaryOp::LT => "<",
                    BinaryOp::GT => ">",
                    BinaryOp::GEq => ">=",
                    BinaryOp::LEq => "<=",
                    BinaryOp::LOr => "||",
                    BinaryOp::LAnd => "&&",
                    BinaryOp::Cons => "::",
                    BinaryOp::Custom(sym) => {
                        return write!(
                            f,
                            "{} {} {}",
                            left.desc.display(self.session, 0),
                            sym.display(&self.session.symbol_interner),
                            right.desc.display(self.session, 0)
                        );
                    }
                };
                write!(
                    f,
                    "{} {} {}",
                    left.desc.display(self.session, 0),
                    op_str,
                    right.desc.display(self.session, 0)
                )
            }
            ExpressionDesc::List(exprs) => {
                write!(f, "{}[", INDENT.repeat(self.indent))?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", expr.desc.display(self.session, 0))?;
                }
                write!(f, "]")
            }
        }
    }
}
