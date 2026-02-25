use std::collections::HashMap;

use crate::{
    inference::solved_ty::{SolvedCon, SolvedTy},
    poly_ir::{
        TypeId,
        expr::{VBMarker, ValueBinding},
        id::{Arena, Id},
        item::{Item, ItemNode},
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConcrTy {
    pub id: TypeId,
    pub args: Vec<ConcrTy>,
}

impl SolvedTy {
    pub fn as_concrete(&self) -> Option<ConcrTy> {
        match self {
            SolvedTy::Var(_) => None,
            SolvedTy::Con(SolvedCon { id, args }) => Some(ConcrTy {
                id: *id,
                args: args
                    .iter()
                    .map(|x| x.as_concrete())
                    .collect::<Option<Vec<_>>>()?,
            }),
        }
    }
}

impl TryInto<ConcrTy> for SolvedTy {
    type Error = ();

    fn try_into(self) -> Result<ConcrTy, Self::Error> {
        self.as_concrete().ok_or(())
    }
}

pub type MonoErr = String;
pub type Res<T> = Result<T, MonoErr>;

pub struct Subst {}

pub struct MonoCtx<'a> {
    pub items: Vec<&'a Item<SolvedTy>>,
    pub vb_to_items: HashMap<Id<VBMarker>, usize>, // og id to index in item_infos

    pub vbs: Arena<VBMarker>,
    pub specs: HashMap<(Id<VBMarker>, Subst), Id<VBMarker>>,
    pub bindings: Vec<ValueBinding<ConcrTy>>,
}

impl<'a> MonoCtx<'a> {
    pub fn new(program: &'a [Item<SolvedTy>]) -> Self {
        MonoCtx {
            items: Vec::from_iter(program),
            vb_to_items: HashMap::new(),
            vbs: Arena::new(),
            specs: HashMap::new(),
            bindings: Vec::new(),
        }
    }

    fn render(&mut self) -> Vec<Item<ConcrTy>> {
        // from (og id, subst) -> new id
        let map = std::mem::take(&mut self.specs);

        // from new id -> og id
        let rev = map
            .iter()
            .map(|((a, _), b)| (*b, *a))
            .collect::<HashMap<_, _>>();

        // vec of specialized bindings (news)
        let bindings = std::mem::take(&mut self.bindings);
        let mut mbs: HashMap<Id<_>, Vec<_>> = HashMap::new();

        for binding in bindings {
            let new_id = binding.id;
            let og_id = rev[&new_id];
            mbs.entry(og_id).or_default().push(binding)
        }

        let mut res: Vec<Item<ConcrTy>> = Vec::with_capacity(self.items.len());

        for item in &self.items {
            let node = match item.as_ref().node {
                ItemNode::Value {
                    recursive,
                    bindings,
                } => {
                    let mut res_bindings = Vec::new();
                    for og_id in bindings.iter().map(|b| b.id) {
                        let new_ids = mbs.remove(&og_id).unwrap_or_default();
                        res_bindings.extend(new_ids);
                    }
                    ItemNode::Value {
                        recursive: *recursive,
                        bindings: res_bindings,
                    }
                }
                ItemNode::Type { decls } => ItemNode::Type {
                    decls: decls.clone(),
                },
            };
            res.push(Item::new(node, item.span, ()))
        }
        res
    }

    fn specialize(&mut self) {
        todo!()
    }

    pub fn mono_program(&mut self) -> Res<Vec<Item<ConcrTy>>> {
        // We assume that bindings with free variables are side-effects free so they are fine to declare in any order. Bindings without free variables are already resolved and we just map them to concrete types.
        self.specialize();
        Ok(self.render())
    }
}
