use std::{collections::HashMap, sync::atomic::AtomicU32};

use crate::{
    inference::{
        error::Res,
        solved_ty::{MonoTy, TyForall},
    },
    poly_ir::{
        VarId,
        id::{Arena, Id},
    },
};

use lazy_static::lazy_static;

pub mod error;
pub mod solved_ty;

// Implementing Algorithm J for the moment

#[derive(Debug)]
pub struct InfMarker;

pub type InferVarId = Id<InfMarker>;

#[derive(Debug)]
pub struct InferenceCtx {
    pub map: HashMap<VarId, TyForall>,
    pub tys: Arena<MonoTy>,
    pub forwards: HashMap<Id<MonoTy>, Id<MonoTy>>,
    pub inf: Arena<InfMarker>,
}

impl Id<MonoTy> {
    pub fn get<'a>(&self, ctx: &'a InferenceCtx) -> &'a MonoTy {
        &ctx.tys[*self]
    }

    pub fn find(&self, ctx: &InferenceCtx) -> Id<MonoTy> {
        if self.get(ctx).is_con() {
            *self
        } else if let Some(val) = ctx.forwards.get(self)
            && val.get(ctx).is_var()
        {
            val.find(ctx)
        } else {
            *self
        }
    }

    pub fn make_equal_to(&self, other: Id<MonoTy>, ctx: &mut InferenceCtx) {
        let chain_end = self.find(ctx);
        assert!(chain_end.get(ctx).is_var(), "Already resolved");
        ctx.forwards.insert(chain_end, other);
    }
}

impl MonoTy {
    pub fn occurs_in(&self, host: &Self, ctx: &InferenceCtx) -> bool {
        self == host
            || match host {
                MonoTy::Var(_) => false,
                MonoTy::Con(ty_con) => ty_con
                    .args
                    .iter()
                    .map(|arg| arg.get(ctx))
                    .any(|arg| self.occurs_in(arg, ctx)),
            }
    }
}

impl InferenceCtx {
    pub fn occurence_unify(&mut self, ty1: Id<MonoTy>, ty2: Id<MonoTy>) -> Res<()> {
        let mono1 = ty1.get(self);
        let mono2 = ty2.get(self);
        if mono1.occurs_in(mono2, self) {
            Err("Occurs check failed".into())
        } else {
            ty1.make_equal_to(ty2, self);
            Ok(())
        }
    }

    pub fn unify_j(&mut self, ty1: Id<MonoTy>, ty2: Id<MonoTy>) -> Res<()> {
        let ty1 = ty1.find(self);
        let ty2 = ty2.find(self);

        let mono1 = ty1.get(self);
        let mono2 = ty2.get(self);
        if mono1.is_var() {
            self.occurence_unify(ty1, ty2)?;
            return Ok(());
        }
        if mono2.is_var() {
            self.occurence_unify(ty2, ty1)?;
            return Ok(());
        }
        match (mono1, mono2) {
            (MonoTy::Con(con1), MonoTy::Con(con2)) => {
                if con1.name != con2.name {
                    return Err(format!("Con type mismatch: {} != {}", con1.name, con2.name));
                }
                if con1.args.len() != con2.args.len() {
                    return Err(format!(
                        "Con type {} arg len mismatch : {} != {}",
                        con1.name,
                        con1.args.len(),
                        con2.args.len()
                    ));
                }

                con1.args
                    .iter()
                    .copied()
                    .zip(con2.args.iter().copied())
                    .collect::<Vec<_>>()
                    .into_iter()
                    .map(|(l, r)| self.unify_j(l, r))
                    .collect()
            }
            _ => Err("Unexpected type".into()),
        }
    }

    #[inline(always)]
    pub fn fresh(&mut self) -> InferVarId {
        self.inf.alloc(InfMarker)
    }
}
