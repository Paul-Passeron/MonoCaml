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
pub struct Ast<T: Clone> {
    expr: AstKind<T>,
    metadata: T,
}

#[derive(Clone)]
pub enum AstKind<T: Clone> {
    Str(String),
    Int(i32),
    Var(Var),
    Lambda {
        arg: Var,
        arg_ty: AstTy,
        body: Box<Ast<T>>,
    },
    App {
        fun: Box<Ast<T>>,
        arg: Box<Ast<T>>,
    },
    Seq {
        fst: Box<Ast<T>>,
        snd: Box<Ast<T>>,
    },
    Tuple(Vec<Ast<T>>),
    Native(String),
    LetBinding {
        bound: Var,
        bound_ty: AstTy,
        value: Box<Ast<T>>,
        in_expr: Box<Ast<T>>,
    },
    If {
        cond: Box<Ast<T>>,
        then_e: Box<Ast<T>>,
        else_e: Box<Ast<T>>,
    },
    Cons {
        enum_name: String,
        case: String,
        arg: Option<Box<Ast<T>>>,
    },
    Match {
        expr: Box<Ast<T>>,
        cases: Vec<MatchCase<T>>,
    },
}

#[derive(Clone)]
pub struct MatchCase<T: Clone> {
    pub pat: Pattern,
    pub expr: Ast<T>,
}

impl<MetaData: Clone> Ast<MetaData> {
    pub fn expr(&self) -> &AstKind<MetaData> {
        &self.expr
    }

    pub fn metadata(&self) -> &MetaData {
        &self.metadata
    }
    pub fn extract(self) -> AstKind<MetaData> {
        self.expr
    }

    fn free_vars_aux(&self, s: &mut HashSet<Var>) {
        match self.expr() {
            AstKind::Str(_) | AstKind::Int(_) | AstKind::Native(_) => (),
            AstKind::Var(var) => {
                s.insert(*var);
            }
            AstKind::Lambda { arg, body, .. } => {
                body.free_vars_aux(s);
                s.remove(arg);
            }
            AstKind::App { fun, arg } => {
                fun.free_vars_aux(s);
                arg.free_vars_aux(s);
            }
            AstKind::Seq { fst, snd } => {
                fst.free_vars_aux(s);
                snd.free_vars_aux(s);
            }
            AstKind::Tuple(asts) => asts.iter().for_each(|x| x.free_vars_aux(s)),
            AstKind::LetBinding {
                bound,
                value,
                in_expr,
                ..
            } => {
                value.free_vars_aux(s);
                in_expr.free_vars_aux(s);
                s.remove(bound);
            }
            AstKind::If {
                cond,
                then_e,
                else_e,
            } => {
                cond.free_vars_aux(s);
                then_e.free_vars_aux(s);
                else_e.free_vars_aux(s);
            }
            AstKind::Cons { arg, .. } => arg.iter().for_each(|x| x.free_vars_aux(s)),
            AstKind::Match { expr, cases } => {
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
}

impl Ast<()> {
    pub fn string<S: ToString>(s: S) -> Self {
        Self {
            expr: AstKind::Str(s.to_string()),
            metadata: (),
        }
    }

    pub fn int(i: i32) -> Self {
        Self {
            expr: AstKind::Int(i),
            metadata: (),
        }
    }

    pub fn var(v: Var) -> Self {
        Self {
            expr: AstKind::Var(v),
            metadata: (),
        }
    }

    pub fn lambda(arg: Var, arg_ty: AstTy, body: Self) -> Self {
        Self {
            expr: AstKind::Lambda {
                arg,
                arg_ty,
                body: Box::new(body),
            },
            metadata: (),
        }
    }

    pub fn app(fun: Self, arg: Self) -> Self {
        Self {
            expr: AstKind::App {
                fun: Box::new(fun),
                arg: Box::new(arg),
            },
            metadata: (),
        }
    }

    pub fn seq(fst: Self, snd: Self) -> Self {
        Self {
            expr: AstKind::Seq {
                fst: Box::new(fst),
                snd: Box::new(snd),
            },
            metadata: (),
        }
    }

    pub fn tuple(arg: Vec<Self>) -> Self {
        Self {
            expr: AstKind::Tuple(arg),
            metadata: (),
        }
    }

    pub fn native<S: ToString>(s: S) -> Self {
        Self {
            expr: AstKind::Native(s.to_string()),
            metadata: (),
        }
    }

    pub fn let_in(v: Var, ty: AstTy, e: Ast<()>, in_e: Ast<()>) -> Self {
        Self {
            expr: AstKind::LetBinding {
                bound: v,
                bound_ty: ty,
                value: Box::new(e),
                in_expr: Box::new(in_e),
            },
            metadata: (),
        }
    }

    pub fn ifte(cond: Self, t: Self, e: Self) -> Self {
        Self {
            expr: AstKind::If {
                cond: Box::new(cond),
                then_e: Box::new(t),
                else_e: Box::new(e),
            },
            metadata: (),
        }
    }

    pub fn cons<S1: ToString, S2: ToString>(ty_name: S1, case: S2, arg: Option<Self>) -> Self {
        Self {
            expr: AstKind::Cons {
                enum_name: ty_name.to_string(),
                case: case.to_string(),
                arg: arg.map(Box::new),
            },
            metadata: (),
        }
    }

    pub fn match_with(expr: Self, cases: Vec<MatchCase<()>>) -> Self {
        Self {
            expr: AstKind::Match {
                expr: Box::new(expr),
                cases,
            },
            metadata: (),
        }
    }
}

impl<A: Clone> AstKind<A> {
    pub fn visit<B, F>(self, f: &mut F) -> AstKind<B>
    where
        B: Clone,
        F: FnMut(A, &mut Self) -> B,
    {
        match self {
            AstKind::Str(x) => AstKind::Str(x),
            AstKind::Int(x) => AstKind::Int(x),
            AstKind::Native(x) => AstKind::Native(x),
            AstKind::Var(x) => AstKind::Var(x),
            AstKind::Lambda { arg, arg_ty, body } => AstKind::Lambda {
                arg,
                arg_ty,
                body: Box::new(body.map(f)),
            },
            AstKind::App { fun, arg } => AstKind::App {
                fun: Box::new(fun.map(f)),
                arg: Box::new(arg.map(f)),
            },
            AstKind::Seq { fst, snd } => AstKind::Seq {
                fst: Box::new(fst.map(f)),
                snd: Box::new(snd.map(f)),
            },
            AstKind::Tuple(asts) => AstKind::Tuple(asts.into_iter().map(|x| x.map(f)).collect()),
            AstKind::LetBinding {
                bound,
                bound_ty,
                value,
                in_expr,
            } => AstKind::LetBinding {
                bound,
                bound_ty,
                value: Box::new(value.map(f)),
                in_expr: Box::new(in_expr.map(f)),
            },
            AstKind::If {
                cond,
                then_e,
                else_e,
            } => AstKind::If {
                cond: Box::new(cond.map(f)),
                then_e: Box::new(then_e.map(f)),
                else_e: Box::new(else_e.map(f)),
            },
            AstKind::Cons {
                enum_name,
                case,
                arg,
            } => AstKind::Cons {
                enum_name,
                case,
                arg: arg.map(|x| Box::new(x.map(f))),
            },
            AstKind::Match { expr, cases } => AstKind::Match {
                expr: Box::new(expr.map(f)),
                cases: cases
                    .into_iter()
                    .map(|MatchCase { pat, expr }| MatchCase {
                        pat,
                        expr: expr.map(f),
                    })
                    .collect(),
            },
        }
    }
}

impl<A: Clone> Ast<A> {
    pub fn map<B, F>(self, f: &mut F) -> Ast<B>
    where
        B: Clone,
        F: FnMut(A, &mut AstKind<A>) -> B,
    {
        let mut this = self;
        let metadata = f(this.metadata, &mut this.expr);
        let expr = this.expr.visit(f);
        Ast { expr, metadata }
    }
}

pub struct DisplayAstKind<'a, M: Clone>(&'a AstKind<M>, usize);

impl<'a, M: Clone> fmt::Display for DisplayAstKind<'a, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", " ".repeat(TAB_SIZE * self.1))?;
        match &self.0 {
            AstKind::Str(x) => write!(f, "\"{}\"", x.escape_debug()),
            AstKind::Int(x) => write!(f, "{x}"),
            AstKind::Var(var) => write!(f, "{var}"),
            AstKind::Lambda { arg, arg_ty, body } => {
                writeln!(f, "fun {arg} : {arg_ty} =>")?;
                write!(f, "{}", DisplayAst(body, self.1 + 1))
            }
            AstKind::App { fun, arg } => {
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
            AstKind::Seq { fst, snd } => {
                writeln!(f, "{};", fst.display())?;
                write!(f, "{}", DisplayAst(snd, self.1))
            }
            AstKind::Tuple(asts) => {
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
            AstKind::Native(s) => write!(f, "{s}"),
            AstKind::LetBinding {
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
            AstKind::If {
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
            AstKind::Cons {
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
            AstKind::Match { expr, cases } => {
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

impl<A: Clone> AstKind<A> {
    pub fn display<'a>(&'a self) -> impl fmt::Display + 'a {
        DisplayAstKind(self, 0)
    }
}

pub struct DisplayAst<'a, M: Clone>(&'a Ast<M>, usize);

impl<'a, M: Clone> fmt::Display for DisplayAst<'a, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = self.0.expr.display().to_string();
        if val.contains("\n") {
            for l in val.lines() {
                writeln!(f, "{}{}", " ".repeat(TAB_SIZE * self.1), l)?;
            }
            Ok(())
        } else {
            write!(f, "{val}")
        }
    }
}

impl<A: Clone> Ast<A> {
    pub fn display<'a>(&'a self) -> impl fmt::Display + 'a {
        DisplayAst(self, 0)
    }
}

pub struct MetaDisplayAstKind<'a, M: Clone>(&'a AstKind<M>, usize);

const TAB_SIZE: usize = 4;

impl<'a, M: fmt::Display + Clone> fmt::Display for MetaDisplayAstKind<'a, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", " ".repeat(TAB_SIZE * self.1))?;
        match &self.0 {
            AstKind::Str(x) => write!(f, "\"{}\"", x.escape_debug()),
            AstKind::Int(x) => write!(f, "{x}"),
            AstKind::Var(var) => write!(f, "{var}"),
            AstKind::Lambda { arg, arg_ty, body } => {
                writeln!(f, "fun {arg} : {arg_ty} =>")?;
                write!(f, "{}", MetaDisplayAst(body, self.1 + 1))
            }
            AstKind::App { fun, arg } => {
                let fu = fun.display_meta().to_string();
                let x = arg.display_meta().to_string();
                if fu.contains("\n") || x.contains("\n") {
                    if fu.contains("\n") {
                        write!(f, "{}", fu.lines().next().unwrap_or(""))?;
                        for l in fu.lines().skip(1) {
                            writeln!(f, "\n{}{}", " ".repeat(TAB_SIZE * self.1), l)?;
                        }
                        writeln!(f)?;
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
            AstKind::Seq { fst, snd } => {
                writeln!(f, "{};", fst.display_meta())?;
                write!(f, "{}", MetaDisplayAst(snd, self.1))
            }
            AstKind::Tuple(asts) => {
                let elems = asts.iter().any(|x| x.display().to_string().contains("\n"));
                if elems {
                    writeln!(f, "(")?;
                    asts.iter().for_each(|x| {
                        writeln!(f, "{},", MetaDisplayAst(x, self.1 + 1)).unwrap();
                    });
                    write!(f, "{})", " ".repeat(TAB_SIZE * self.1))
                } else {
                    write!(
                        f,
                        "({})",
                        asts.iter()
                            .map(|x| x.display_meta().to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            AstKind::Native(s) => write!(f, "{s}"),
            AstKind::LetBinding {
                bound,
                bound_ty,
                value,
                in_expr,
            } => {
                write!(f, "let {bound}: {bound_ty} =")?;
                let val = value.display_meta().to_string();
                if val.len() > 15 || val.contains("\n") {
                    writeln!(f, "\n{}", MetaDisplayAst(value, self.1 + 1))?;
                    writeln!(f, "{}in", " ".repeat(TAB_SIZE * self.1))?;
                } else {
                    writeln!(f, " {} in", val)?;
                }

                write!(f, "{}", MetaDisplayAst(in_expr, self.1))
            }
            AstKind::If {
                cond,
                then_e,
                else_e,
            } => {
                let str_cond = cond.display_meta().to_string();
                let str_then_e = then_e.display_meta().to_string();
                let str_else_e = else_e.display_meta().to_string();

                if str_cond.contains("\n") {
                    writeln!(f, "\n{}", MetaDisplayAst(cond, self.1 + 1))?;
                    writeln!(f, "{}then", " ".repeat(TAB_SIZE * self.1))?;
                    writeln!(f, "{}", MetaDisplayAst(then_e, self.1 + 1))?;
                    writeln!(f, "{}else", " ".repeat(TAB_SIZE * self.1))?;
                    writeln!(f, "{}", MetaDisplayAst(else_e, self.1 + 1))?;
                } else if str_then_e.contains("\n") || str_else_e.contains("\n") {
                    writeln!(f, "\n{}", MetaDisplayAst(then_e, self.1 + 1))?;
                    writeln!(f, "{}else", " ".repeat(TAB_SIZE * self.1))?;
                    writeln!(f, "{}", MetaDisplayAst(else_e, self.1 + 1))?;
                } else {
                    writeln!(f, " if {str_cond} then {str_then_e} else {str_else_e}")?;
                }

                Ok(())
            }
            AstKind::Cons {
                case: cons_name,
                arg,
                ..
            } => {
                write!(f, "{cons_name}")?;
                let str_arg = match arg {
                    Some(arg) => arg.display_meta().to_string(),
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
            AstKind::Match { expr, cases } => {
                write!(f, "match ")?;
                let str_e = expr.display_meta().to_string();
                if str_e.contains("\n") {
                    writeln!(f,)?;
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
                    let e = case.expr.display_meta().to_string();
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

impl<M: fmt::Display + Clone> AstKind<M> {
    pub fn display_meta<'a>(&'a self) -> impl fmt::Display + 'a {
        MetaDisplayAstKind(self, 0)
    }
}

pub struct MetaDisplayAst<'a, M: fmt::Display + Clone>(&'a Ast<M>, usize);

impl<'a, M: fmt::Display + Clone> fmt::Display for MetaDisplayAst<'a, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let disp = self.0.expr().display_meta().to_string();
        if disp.contains("\n") {
            writeln!(f, "{{")?;
            for l in disp.lines() {
                writeln!(f, "{}{}", " ".repeat(TAB_SIZE * (self.1 + 1)), l)?;
            }
            writeln!(
                f,
                "{}: {}}}",
                " ".repeat(TAB_SIZE * self.1),
                self.0.metadata()
            )?;
            Ok(())
        } else {
            write!(
                f,
                "{{{} : {}}}",
                self.0.expr().display_meta(),
                self.0.metadata()
            )
        }
    }
}

impl<A: Clone> Ast<A>
where
    A: fmt::Display,
{
    pub fn display_meta<'a>(&'a self) -> impl fmt::Display + 'a {
        MetaDisplayAst(self, 0)
    }
}
