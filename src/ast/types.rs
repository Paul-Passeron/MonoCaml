use std::collections::HashMap;

#[derive(Clone)]
pub enum AstTy {
    Int,
    String,
    Tuple(Vec<AstTy>),
    Fun { arg: Box<AstTy>, ret: Box<AstTy> },
    Named(String),
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
#[derive(Clone)]
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

pub struct EnumCase {
    pub cons_name: String,
    pub args: Vec<AstTy>,
}

pub struct EnumDef {
    pub cases: Vec<EnumCase>,
}

pub enum TypeDef {
    Alias(AstTy),
    Enum(EnumDef),
}

#[derive(Default)]
pub struct AstCtx {
    pub types: HashMap<String, TypeDef>,
}
