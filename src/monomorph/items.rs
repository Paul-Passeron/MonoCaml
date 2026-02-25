use std::collections::HashSet;

use crate::{
    inference::{InferVarId, solved_ty::SolvedTy},
    poly_ir::{
        expr::ValueBinding,
        item::{GenItem, ItemNode},
    },
};

impl<T> GenItem<SolvedTy, T> {
    pub fn collect_free_vars(&self) -> HashSet<InferVarId> {
        let mut s = HashSet::new();
        match &self.node {
            ItemNode::Value { bindings, .. } => {
                for binding in bindings {
                    binding.map_mut(&mut s, &mut |ty, s| s.extend(ty.free_vars()));
                }
            }
            _ => (),
        }
        s
    }

    pub fn into_collected(self) -> GenItem<SolvedTy, HashSet<InferVarId>> {
        GenItem {
            ty: self.collect_free_vars(),
            node: self.node,
            span: self.span,
        }
    }
}

impl<T> ItemNode<T> {
    pub(super) fn get_bindings(&self) -> &[ValueBinding<T>] {
        match self {
            ItemNode::Value { bindings, .. } => bindings,
            _ => &[],
        }
    }
}
