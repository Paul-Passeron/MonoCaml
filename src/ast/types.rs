use std::{
    collections::{HashMap, HashSet},
    iter::once,
};

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

    pub fn is_rec_aux(&self, ctx: &AstCtx, names: &mut HashSet<&str>) -> bool {
        match self {
            AstTy::Int => false,
            AstTy::String => false,
            AstTy::Tuple(items) => items.iter().any(|x| x.is_rec_aux(ctx, names)),
            AstTy::Fun { arg, ret } => arg.is_rec_aux(ctx, names) || ret.is_rec_aux(ctx, names),
            AstTy::Named(this) => {
                if names.contains(&this[..]) {
                    true
                } else {
                    let def = &ctx.types[this];
                    match def {
                        TypeDef::Alias(ast_ty) => ast_ty.is_rec_aux(ctx, names),
                        TypeDef::Enum(enum_def) => enum_def
                            .cases
                            .iter()
                            .map(|x| x.arg.iter())
                            .flatten()
                            .any(|x| x.is_rec_aux(ctx, names)),
                    }
                }
            }
        }
    }

    pub fn is_recursive(&self, ctx: &AstCtx) -> bool {
        match self {
            AstTy::Named(this) => {
                let def = &ctx.types[this];
                let names = &mut HashSet::from_iter(once(&this[..]));
                match def {
                    TypeDef::Alias(ast_ty) => ast_ty.is_rec_aux(ctx, names),
                    TypeDef::Enum(enum_def) => enum_def
                        .cases
                        .iter()
                        .map(|x| x.arg.iter())
                        .flatten()
                        .any(|x| x.is_rec_aux(ctx, names)),
                }
            }
            _ => false,
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
    pub arg: Option<AstTy>,
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

#[cfg(test)]
mod tests {
    use crate::ast::types::{AstCtx, AstTy, EnumCase, EnumDef, TypeDef};

    #[test]
    pub fn test_list_is_rec() {
        let mut ctx = AstCtx::default();
        let elem_ty = AstTy::Int;
        ctx.types
            .insert("elem".into(), TypeDef::Alias(elem_ty.clone()));
        let list_ty = EnumDef {
            cases: vec![
                EnumCase {
                    cons_name: "Nil".into(),
                    arg: None,
                },
                EnumCase {
                    cons_name: "Cons".into(),
                    arg: Some(AstTy::Tuple(vec![
                        AstTy::Named("elem".into()),
                        AstTy::Named("list".into()),
                    ])),
                },
            ],
        };

        ctx.types.insert("list".into(), TypeDef::Enum(list_ty));

        assert!(AstTy::Named("list".into()).is_recursive(&ctx));
        assert!(!AstTy::Named("elem".into()).is_recursive(&ctx));
    }
}
