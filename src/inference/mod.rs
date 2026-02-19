use crate::{
    inference::solved_ty::SolvedTy,
    poly_ir::item::Item,
    resolution::{ResItem, error::Res},
};

pub mod solved_ty;

#[derive(Debug)]
pub struct InferenceCtx {}

impl InferenceCtx {
    pub fn new() -> Self {
        Self {}
    }

    pub fn infer(&mut self, items: &[ResItem]) -> Res<Vec<Item<SolvedTy>>> {
        todo!("{items:?}")
    }
}

impl Default for InferenceCtx {
    fn default() -> Self {
        Self::new()
    }
}
