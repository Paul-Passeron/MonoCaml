pub mod display;
mod vars;

use crate::helpers::unique::Unique;

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
pub enum AstTy {
    Int,
    String,
    Tuple(Vec<AstTy>),
    Fun { arg: Box<AstTy>, ret: Box<AstTy> },
}

impl AstTy {
    pub fn int() -> Self {
        Self::Int
    }

    pub fn string() -> Self {
        Self::String
    }

    pub fn fun(arg: Self, ret: Self) -> Self {
        Self::Fun {
            arg: Box::new(arg),
            ret: Box::new(ret),
        }
    }
}

pub struct AstTyped<T> {
    expr: T,
    ty: AstTy,
}

impl<T> AstTyped<T> {
    pub fn new(expr: T, ty: AstTy) -> Self {
        Self { expr, ty }
    }

    pub fn ty<'a>(&'a self) -> &'a AstTy {
        &self.ty
    }

    pub fn expr<'a>(&'a self) -> &'a T {
        &self.expr
    }
}

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
}
