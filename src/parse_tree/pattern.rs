use std::fmt;

use crate::{
    lexer::interner::Symbol,
    parse_tree::{
        Located, LongIdent, RecordField,
        expression::{BinaryOp, Constant},
        type_expr::TypeExpr,
    },
    resolve_strlit, resolve_symbol,
};

pub type Pattern = Located<PatternDesc>;

#[derive(Debug)]
pub enum PatternDesc {
    Any, // _
    // TODO: Var takes ident symbol
    Var(Symbol), // x
    // Alias(Box<Pattern>, Symbol),                    // p as x
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

pub struct PatternDescDisplay<'a> {
    pub desc: &'a PatternDesc,
    pub indent: usize,
}

impl PatternDesc {
    pub fn display<'a>(&'a self, indent: usize) -> PatternDescDisplay<'a> {
        PatternDescDisplay { desc: self, indent }
    }
}

const INDENT: &str = "    ";

impl fmt::Display for PatternDescDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", INDENT.repeat(self.indent))?;
        match &self.desc {
            PatternDesc::Any => write!(f, "_"),
            PatternDesc::Var(symbol) => {
                write!(f, "{}", resolve_symbol(*symbol))
            }
            // PatternDesc::Alias(pat, symbol) => {
            //     write!(f, "{} as {}", pat.desc.display(0), resolve_symbol(*symbol))
            // }
            PatternDesc::Constant(constant) => match constant {
                Constant::Int(i) => write!(f, "{}", i),
                Constant::Char(c) => write!(f, "'{}'", c),
                Constant::String(s) => {
                    write!(f, "\"{}\"", resolve_strlit(*s))
                }
                Constant::Float(fl) => write!(f, "{}", fl),
            },
            PatternDesc::Interval { start, end } => {
                match start {
                    Constant::Int(i) => write!(f, "{}", i)?,
                    Constant::Char(c) => write!(f, "'{}'", c)?,
                    _ => write!(f, "?")?,
                }
                write!(f, "..")?;
                match end {
                    Constant::Int(i) => write!(f, "{}", i),
                    Constant::Char(c) => write!(f, "'{}'", c),
                    _ => write!(f, "?"),
                }
            }
            PatternDesc::Tuple(pats) => {
                write!(f, "(")?;
                for (i, pat) in pats.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", pat.desc.display(0))?;
                }
                write!(f, ")")
            }
            PatternDesc::Construct(long_ident, None) => {
                write!(f, "{}", long_ident.display(0))
            }
            PatternDesc::Construct(long_ident, Some(pat)) => {
                write!(f, "{} {}", long_ident.display(0), pat.desc.display(0))
            }
            PatternDesc::Record(fields) => {
                write!(f, "{{ ")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(
                        f,
                        "{} = {}",
                        field.name.display(0),
                        field.pat.desc.display(0)
                    )?;
                }
                write!(f, " }}")
            }
            PatternDesc::Constraint(pat, typ) => {
                write!(f, "({} : {})", pat.desc.display(0), typ.desc.display(0))
            }
            PatternDesc::Unit => write!(f, "()"),
            PatternDesc::Paren(pat) => {
                write!(f, "({})", pat.desc.display(0))
            }
            PatternDesc::BinaryOp(op, lhs, rhs) => {
                let op_str = match op {
                    BinaryOp::Cons => "::",
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
                    BinaryOp::Custom(sym) => {
                        return write!(
                            f,
                            "{} {} {}",
                            lhs.desc.display(0),
                            resolve_symbol(*sym),
                            rhs.desc.display(0)
                        );
                    }
                };
                write!(
                    f,
                    "{} {} {}",
                    lhs.desc.display(0),
                    op_str,
                    rhs.desc.display(0)
                )
            }
            PatternDesc::List(pats) => {
                write!(f, "[")?;
                for (i, pat) in pats.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", pat.desc.display(0))?;
                }
                write!(f, "]")
            }
        }
    }
}
