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

    pub fn named<S: ToString>(name: S) -> Self {
        AstTy::Named(name.to_string())
    }

    pub fn rec_aux(&self, this: &str, ctx: &AstCtx) -> bool {
        match self {
            AstTy::Int => false,
            AstTy::String => false,
            AstTy::Tuple(items) => items.iter().any(|x| x.rec_aux(this, ctx)),
            AstTy::Fun { arg, ret } => arg.rec_aux(this, ctx) || ret.rec_aux(this, ctx),
            AstTy::Named(x) => {
                if x == this {
                    true
                } else {
                    let enum_def = &ctx.types[&x[..]];
                    enum_def
                        .cases
                        .iter()
                        .any(|x| x.arg.iter().any(|x| x.rec_aux(this, ctx)))
                }
            }
        }
    }

    // TODO: Maybe use another version of this when compiling to CFG because:
    // - Here, Types are marked as recursive even if the type containing them is later turned into a pointer
    //      Example: the type enum T { Nil | AFunction(T -> T) } is marked as recursive even though a T -> T is
    //      turned into a closure which sizes are invariant.
    pub fn is_recursive(&self, ctx: &AstCtx) -> bool {
        match self {
            AstTy::Named(this) => {
                let enum_def = &ctx.types[&this[..]];
                enum_def
                    .cases
                    .iter()
                    .any(|x| x.arg.iter().any(|x| x.rec_aux(this, ctx)))
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

#[derive(Clone)]
pub struct EnumCase {
    pub cons_name: String,
    pub arg: Option<AstTy>,
}

#[derive(Clone)]
pub struct EnumDef {
    pub name: String,
    pub cases: Vec<EnumCase>,
}

#[derive(Default)]
pub struct AstCtx {
    pub types: HashMap<String, EnumDef>,
    pub natives: HashMap<String, AstTy>,
}

#[cfg(test)]
mod tests {
    use crate::ast::types::{AstCtx, AstTy, EnumCase, EnumDef};

    #[test]
    pub fn test_list_is_rec() {
        let mut ctx = AstCtx::default();
        let elem_ty = AstTy::Int;
        let list_ty = EnumDef {
            name: "list".into(),
            cases: vec![
                EnumCase {
                    cons_name: "Nil".into(),
                    arg: None,
                },
                EnumCase {
                    cons_name: "Cons".into(),
                    arg: Some(AstTy::Tuple(vec![
                        elem_ty.clone(),
                        AstTy::Named("list".into()),
                    ])),
                },
            ],
        };

        println!("{list_ty}");
        ctx.types.insert("list".into(), list_ty);
        assert!(AstTy::Named("list".into()).is_recursive(&ctx));
        assert!(!elem_ty.is_recursive(&ctx));
    }

    #[test]
    pub fn test_mutually_is_rec() {
        let mut ctx = AstCtx::default();
        let a_ty = EnumDef {
            name: "a".into(),
            cases: vec![EnumCase {
                cons_name: "B".into(),
                arg: Some(AstTy::Named("b".into())),
            }],
        };
        let b_ty = EnumDef {
            name: "b".into(),
            cases: vec![EnumCase {
                cons_name: "A".into(),
                arg: Some(AstTy::Named("a".into())),
            }],
        };

        ctx.types.insert("a".into(), a_ty);
        ctx.types.insert("b".into(), b_ty);

        assert!(AstTy::Named("a".into()).is_recursive(&ctx));
        assert!(AstTy::Named("b".into()).is_recursive(&ctx));
    }
}
