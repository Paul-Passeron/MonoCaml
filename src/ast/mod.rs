pub mod display;
pub mod types;
mod vars;
use std::collections::HashSet;

use crate::{
    ast::types::{AstTy, AstTyped},
    helpers::unique::Unique,
};

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
        arg: Box<AstTyped<Var>>,
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
    Get {
        from: Box<Ast>,
        index: usize,
    },
    Native(String),
    LetBinding {
        bound: AstTyped<Var>,
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
        arg: Option<Box<Ast>>,
    },
}

impl Ast {
    pub fn string<S: Into<String>>(s: S) -> Self {
        Self::Str(s.into())
    }

    pub fn int(i: i32) -> Self {
        Self::Int(i)
    }

    pub fn var(v: Var) -> Self {
        Self::Var(v)
    }

    pub fn lambda(arg: AstTyped<Var>, body: Self) -> Self {
        Self::Lambda {
            arg: Box::new(arg),
            body: Box::new(body),
        }
    }

    pub fn app(fun: Self, arg: Self) -> Self {
        Self::App {
            fun: Box::new(fun),
            arg: Box::new(arg),
        }
    }

    pub fn seq(fst: Self, snd: Self) -> Self {
        Self::Seq {
            fst: Box::new(fst),
            snd: Box::new(snd),
        }
    }

    pub fn tuple(arg: Vec<Self>) -> Self {
        Self::Tuple(arg)
    }

    pub fn get(from: Self, index: usize) -> Self {
        Self::Get {
            from: Box::new(from),
            index,
        }
    }

    pub fn native<S: Into<String>>(s: S) -> Self {
        Self::Native(s.into())
    }

    pub fn let_in(v: Var, ty: AstTy, e: Ast, in_e: Ast) -> Self {
        Self::LetBinding {
            bound: AstTyped::new(v, ty),
            value: Box::new(e),
            in_expr: Box::new(in_e),
        }
    }

    pub fn ifte(cond: Self, t: Self, e: Self) -> Self {
        Self::If {
            cond: Box::new(cond),
            then_e: Box::new(t),
            else_e: Box::new(e),
        }
    }

    pub fn free_vars(&self) -> HashSet<Var> {
        let mut s = HashSet::new();
        self.free_vars_aux(&mut s);
        s
    }

    fn free_vars_aux(&self, s: &mut HashSet<Var>) {
        match self {
            Ast::Str(_) | Ast::Int(_) | Ast::Native(_) => (),
            Ast::Var(var) => {
                s.insert(var.clone());
            }
            Ast::Lambda { arg, body } => {
                body.free_vars_aux(s);
                s.remove(&arg.expr());
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
            Ast::Get { from, .. } => from.free_vars_aux(s),
            Ast::LetBinding {
                bound,
                value,
                in_expr,
                ..
            } => {
                value.free_vars_aux(s);
                in_expr.free_vars_aux(s);
                s.remove(bound.expr());
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
        }
    }
}
