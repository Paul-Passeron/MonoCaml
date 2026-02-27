use std::collections::HashSet;

use crate::{
    inference::{InferVarId, InferenceCtx},
    poly_ir::{TypeId, id::Id},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SolvedTy {
    Var(TyVar),
    Con(SolvedCon),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SolvedCon {
    pub id: TypeId,
    pub args: Vec<SolvedTy>,
}

impl SolvedTy {
    pub fn int_ty(ctx: &InferenceCtx) -> Self {
        Self::Con(SolvedCon {
            id: Id::from_name("int", ctx.decls).unwrap(),
            args: vec![],
        })
    }

    pub fn bool_ty(ctx: &InferenceCtx) -> Self {
        Self::Con(SolvedCon {
            id: Id::from_name("bool", ctx.decls).unwrap(),
            args: vec![],
        })
    }

    pub fn unit_ty(ctx: &InferenceCtx) -> Self {
        Self::Con(SolvedCon {
            id: Id::from_name("unit", ctx.decls).unwrap(),
            args: vec![],
        })
    }

    pub fn string_ty(ctx: &InferenceCtx) -> Self {
        Self::Con(SolvedCon {
            id: Id::from_name("string", ctx.decls).unwrap(),
            args: vec![],
        })
    }

    pub fn list_ty(ty: SolvedTy, ctx: &InferenceCtx) -> Self {
        Self::Con(SolvedCon {
            id: Id::from_name("list", ctx.decls).unwrap(),
            args: vec![ty],
        })
    }

    pub fn tuple_ty(tys: Vec<SolvedTy>, ctx: &InferenceCtx) -> Self {
        Self::Con(SolvedCon {
            id: Id::from_name("*", ctx.decls).unwrap(),
            args: tys,
        })
    }

    pub fn func_ty(arg: SolvedTy, ret: SolvedTy, ctx: &InferenceCtx) -> Self {
        Self::Con(SolvedCon {
            id: Id::from_name("->", ctx.decls).unwrap(),
            args: vec![arg, ret],
        })
    }

    pub fn free_vars(&self) -> HashSet<InferVarId> {
        fn aux(ty: &SolvedTy, s: &mut HashSet<InferVarId>) {
            match ty {
                SolvedTy::Var(ty_var) => {
                    s.insert(ty_var.id);
                }
                SolvedTy::Con(solved_con) => solved_con.args.iter().for_each(|t| aux(t, s)),
            }
        }
        let mut s = HashSet::new();
        aux(self, &mut s);
        s
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MonoTy {
    Var(TyVar),
    Con(TyCon),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyForall {
    pub tyvars: Vec<TyVar>,
    pub ty: Box<MonoTy>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar {
    pub id: InferVarId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyCon {
    pub id: TypeId,
    pub args: Vec<Id<MonoTy>>,
}

impl MonoTy {
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }

    pub fn is_con(&self) -> bool {
        matches!(self, Self::Con(_))
    }

    pub fn as_var(&self) -> Option<&TyVar> {
        match self {
            MonoTy::Var(ty_var) => Some(ty_var),
            _ => None,
        }
    }

    pub fn as_con(&self) -> Option<&TyCon> {
        match self {
            MonoTy::Con(ty_con) => Some(ty_con),
            _ => None,
        }
    }

    pub fn into_var(self) -> Option<TyVar> {
        match self {
            MonoTy::Var(ty_var) => Some(ty_var),
            _ => None,
        }
    }

    pub fn into_con(self) -> Option<TyCon> {
        match self {
            MonoTy::Con(ty_con) => Some(ty_con),
            _ => None,
        }
    }

    pub fn tuple_ty(args: Vec<Id<Self>>, ctx: &InferenceCtx) -> Self {
        if args.is_empty() {
            Self::unit_ty(ctx)
        } else {
            MonoTy::Con(TyCon {
                id: Id::from_name("*", ctx.decls).unwrap(),
                args,
            })
        }
    }

    pub fn list_ty(ty: Id<Self>, ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("list", ctx.decls).unwrap(),
            args: vec![ty],
        })
    }

    pub fn func_ty(arg: Id<Self>, ret: Id<Self>, ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("->", ctx.decls).unwrap(),
            args: vec![arg, ret],
        })
    }

    pub fn option_ty(ty: Id<Self>, ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("option", ctx.decls).unwrap(),
            args: vec![ty],
        })
    }

    pub fn result_ty(ok: Id<Self>, err: Id<Self>, ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("result", ctx.decls).unwrap(),
            args: vec![ok, err],
        })
    }

    pub fn unit_ty(ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("unit", ctx.decls).unwrap(),
            args: vec![],
        })
    }

    pub fn int_ty(ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("int", ctx.decls).unwrap(),
            args: vec![],
        })
    }

    pub fn char_ty(ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("char", ctx.decls).unwrap(),
            args: vec![],
        })
    }

    pub fn string_ty(ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("string", ctx.decls).unwrap(),
            args: vec![],
        })
    }

    pub fn float_ty(ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("float", ctx.decls).unwrap(),
            args: vec![],
        })
    }

    pub fn bool_ty(ctx: &InferenceCtx) -> Self {
        MonoTy::Con(TyCon {
            id: Id::from_name("bool", ctx.decls).unwrap(),
            args: vec![],
        })
    }
}

impl From<TyCon> for MonoTy {
    fn from(val: TyCon) -> Self {
        MonoTy::Con(val)
    }
}

impl From<TyVar> for MonoTy {
    fn from(val: TyVar) -> Self {
        MonoTy::Var(val)
    }
}

impl TyVar {
    pub fn new(id: InferVarId) -> Self {
        Self { id }
    }
}

impl TyForall {
    pub fn new(vars: Vec<TyVar>, ty: MonoTy) -> Self {
        Self {
            tyvars: vars,
            ty: Box::new(ty),
        }
    }

    pub fn mono(ty: MonoTy) -> Self {
        Self::new(vec![], ty)
    }
}

impl TyCon {
    pub fn new(id: TypeId, args: Vec<Id<MonoTy>>) -> Self {
        Self { id, args }
    }
}
