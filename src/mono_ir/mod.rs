use std::{collections::HashSet, fmt};

use crate::{
    helpers::unique::Unique,
    mono_ir::{pattern::Pattern, types::AstTy},
};

pub mod display;
pub mod pattern;
pub mod types;
mod vars;

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(usize);

impl Var {
    pub fn fresh() -> Self {
        Unique::<Self>::fresh()
    }

    pub fn extract(&self) -> usize {
        self.0
    }

    pub fn reset() {
        Unique::<Self>::reset();
    }
}

#[derive(Clone)]
pub enum Ast {
    Str(String),
    Int(i32),
    Var(Var),
    Lambda {
        arg: Var,
        arg_ty: AstTy,
        body: Box<Ast>,
    },
    App {
        fun: Box<Ast>,
        arg: Box<Ast>,
    },
    Seq {
        fst: Box<Ast>,
        snd: Box<Ast>,
    },
    Tuple(Vec<Ast>),
    Native(String),
    LetBinding {
        bound: Var,
        bound_ty: AstTy,
        value: Box<Ast>,
        in_expr: Box<Ast>,
    },
    If {
        cond: Box<Ast>,
        then_e: Box<Ast>,
        else_e: Box<Ast>,
    },
    Cons {
        enum_name: String,
        case: String,
        arg: Option<Box<Ast>>,
    },
    Match {
        expr: Box<Ast>,
        cases: Vec<MatchCase>,
    },
}

#[derive(Clone)]
pub struct MatchCase {
    pub pat: Pattern,
    pub expr: Ast,
}

impl Ast {
    fn free_vars_aux(&self, s: &mut HashSet<Var>) {
        match self {
            Ast::Str(_) | Ast::Int(_) | Ast::Native(_) => (),
            Ast::Var(var) => {
                s.insert(*var);
            }
            Ast::Lambda { arg, body, .. } => {
                body.free_vars_aux(s);
                s.remove(arg);
            }
            Ast::App { fun, arg } => {
                fun.free_vars_aux(s);
                arg.free_vars_aux(s);
            }
            Ast::Seq { fst, snd } => {
                fst.free_vars_aux(s);
                snd.free_vars_aux(s);
            }
            Ast::Tuple(asts) => asts.iter().for_each(|x| x.free_vars_aux(s)),
            Ast::LetBinding {
                bound,
                value,
                in_expr,
                ..
            } => {
                value.free_vars_aux(s);
                in_expr.free_vars_aux(s);
                s.remove(bound);
            }
            Ast::If {
                cond,
                then_e,
                else_e,
            } => {
                cond.free_vars_aux(s);
                then_e.free_vars_aux(s);
                else_e.free_vars_aux(s);
            }
            Ast::Cons { arg, .. } => arg.iter().for_each(|x| x.free_vars_aux(s)),
            Ast::Match { expr, cases } => {
                expr.free_vars_aux(s);
                cases.iter().for_each(|case| {
                    case.expr.free_vars_aux(s);
                    case.pat.vars().iter().for_each(|x| {
                        s.remove(x);
                    })
                });
            }
        }
    }

    pub fn free_vars(&self) -> HashSet<Var> {
        let mut s = HashSet::new();
        self.free_vars_aux(&mut s);
        s
    }

    pub fn string<S: ToString>(s: S) -> Self {
        Ast::Str(s.to_string())
    }

    pub fn int(i: i32) -> Self {
        Ast::Int(i)
    }

    pub fn var(v: Var) -> Self {
        Ast::Var(v)
    }

    pub fn lambda(arg: Var, arg_ty: AstTy, body: Self) -> Self {
        Ast::Lambda {
            arg,
            arg_ty,
            body: Box::new(body),
        }
    }

    pub fn app(fun: Self, arg: Self) -> Self {
        Ast::App {
            fun: Box::new(fun),
            arg: Box::new(arg),
        }
    }

    pub fn seq(fst: Self, snd: Self) -> Self {
        Ast::Seq {
            fst: Box::new(fst),
            snd: Box::new(snd),
        }
    }

    pub fn tuple(arg: Vec<Self>) -> Self {
        Ast::Tuple(arg)
    }

    pub fn native<S: ToString>(s: S) -> Self {
        Ast::Native(s.to_string())
    }

    pub fn let_in(v: Var, ty: AstTy, e: Ast, in_e: Ast) -> Self {
        Ast::LetBinding {
            bound: v,
            bound_ty: ty,
            value: Box::new(e),
            in_expr: Box::new(in_e),
        }
    }

    pub fn ifte(cond: Self, t: Self, e: Self) -> Self {
        Ast::If {
            cond: Box::new(cond),
            then_e: Box::new(t),
            else_e: Box::new(e),
        }
    }

    pub fn cons<S1: ToString, S2: ToString>(ty_name: S1, case: S2, arg: Option<Self>) -> Self {
        Ast::Cons {
            enum_name: ty_name.to_string(),
            case: case.to_string(),
            arg: arg.map(Box::new),
        }
    }

    pub fn match_with(expr: Self, cases: Vec<MatchCase>) -> Self {
        Ast::Match {
            expr: Box::new(expr),
            cases,
        }
    }

    pub fn display<'a>(&'a self) -> impl fmt::Display + 'a {
        DisplayAst(self, 0)
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display())
    }
}

pub struct DisplayAst<'a>(&'a Ast, usize);

impl<'a> fmt::Display for DisplayAst<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", " ".repeat(TAB_SIZE * self.1))?;
        match &self.0 {
            Ast::Str(x) => write!(f, "\"{}\"", x.escape_debug()),
            Ast::Int(x) => write!(f, "{x}"),
            Ast::Var(var) => write!(f, "{var}"),
            Ast::Lambda { arg, arg_ty, body } => {
                writeln!(f, "fun {arg} : {arg_ty} =>")?;
                write!(f, "{}", DisplayAst(body, self.1 + 1))
            }
            Ast::App { fun, arg } => {
                let fu = fun.display().to_string();
                let x = arg.display().to_string();
                if fu.contains("\n") || x.contains("\n") {
                    if fu.contains("\n") {
                        write!(f, "{}", fu.lines().next().unwrap_or(""))?;
                        for l in fu.lines().skip(1) {
                            write!(f, "\n{}{}", " ".repeat(TAB_SIZE * self.1), l)?;
                        }
                        write!(f, " ")?;
                    } else {
                        write!(f, "{fu} ")?;
                    }
                    if x.contains("\n") {
                        write!(f, "{}", x.lines().next().unwrap_or(""))?;
                        for l in x.lines().skip(1) {
                            write!(f, "\n{}{}", " ".repeat(TAB_SIZE * self.1), l)?;
                        }
                        Ok(())
                    } else {
                        write!(f, "{x}")
                    }
                } else {
                    write!(f, "({} {})", fu, x)
                }
            }
            Ast::Seq { fst, snd } => {
                writeln!(f, "{};", fst.display())?;
                write!(f, "{}", DisplayAst(snd, self.1))
            }
            Ast::Tuple(asts) => {
                let elems = asts.iter().any(|x| x.display().to_string().contains("\n"));
                if elems {
                    writeln!(f, "(")?;
                    asts.iter().for_each(|x| {
                        writeln!(f, "{},", DisplayAst(x, self.1 + 1)).unwrap();
                    });
                    write!(f, "{})", " ".repeat(TAB_SIZE * self.1))
                } else {
                    write!(
                        f,
                        "({})",
                        asts.iter()
                            .map(|x| x.display().to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Ast::Native(s) => write!(f, "{s}"),
            Ast::LetBinding {
                bound,
                bound_ty,
                value,
                in_expr,
            } => {
                write!(f, "let {bound}: {bound_ty} =")?;
                let val = value.display().to_string();
                if val.len() > 15 || val.contains("\n") {
                    writeln!(f, "\n{}", DisplayAst(value, self.1 + 1))?;
                    writeln!(f, "{}in", " ".repeat(TAB_SIZE * self.1))?;
                } else {
                    writeln!(f, " {} in", val)?;
                }

                write!(f, "{}", DisplayAst(in_expr, self.1))
            }
            Ast::If {
                cond,
                then_e,
                else_e,
            } => {
                let str_cond = cond.display().to_string();
                let str_then_e = then_e.display().to_string();
                let str_else_e = else_e.display().to_string();

                if str_cond.contains("\n") {
                    writeln!(f, "\n{}", DisplayAst(cond, self.1 + 1))?;
                    writeln!(f, "{}then", " ".repeat(TAB_SIZE * self.1))?;
                    writeln!(f, "{}", DisplayAst(then_e, self.1 + 1))?;
                    writeln!(f, "{}else", " ".repeat(TAB_SIZE * self.1))?;
                    writeln!(f, "{}", DisplayAst(else_e, self.1 + 1))?;
                } else if str_then_e.contains("\n") || str_else_e.contains("\n") {
                    writeln!(f, "\n{}", DisplayAst(then_e, self.1 + 1))?;
                    writeln!(f, "{}else", " ".repeat(TAB_SIZE * self.1))?;
                    writeln!(f, "{}", DisplayAst(else_e, self.1 + 1))?;
                } else {
                    writeln!(f, " if {str_cond} then {str_then_e} else {str_else_e}")?;
                }

                Ok(())
            }
            Ast::Cons {
                case: cons_name,
                arg,
                ..
            } => {
                write!(f, "{cons_name}")?;
                let str_arg = match arg {
                    Some(arg) => arg.display().to_string(),
                    None => String::new(),
                };
                if str_arg.contains("\n") {
                    writeln!(f, "(")?;
                    for l in str_arg.lines() {
                        writeln!(f, "{}{}", " ".repeat(TAB_SIZE * (self.1 + 1)), l)?;
                    }
                    writeln!(f, "{})", " ".repeat(TAB_SIZE * self.1))?;
                } else if !str_arg.is_empty() {
                    write!(f, "({str_arg})")?;
                }
                Ok(())
            }
            Ast::Match { expr, cases } => {
                write!(f, "match ")?;
                let str_e = expr.display().to_string();
                if str_e.contains("\n") {
                    writeln!(f)?;
                    for l in str_e.lines() {
                        writeln!(f, "{}{}", " ".repeat(TAB_SIZE * (self.1 + 1)), l)?;
                    }
                    writeln!(f, "{}with", " ".repeat(TAB_SIZE * self.1))?;
                } else {
                    write!(f, "{str_e} with")?;
                }
                for case in cases {
                    write!(
                        f,
                        "{}| {} ->",
                        " ".repeat(TAB_SIZE * (self.1 + 1)),
                        case.pat
                    )?;
                    let e = case.expr.display().to_string();
                    if e.contains("\n") {
                        writeln!(f)?;
                        for l in e.lines() {
                            writeln!(f, "{}{}", " ".repeat(TAB_SIZE * (self.1 + 1)), l)?;
                        }
                    } else {
                        write!(f, "{e}")?;
                    }
                }
                Ok(())
            }
        }
    }
}

const TAB_SIZE: usize = 4;
