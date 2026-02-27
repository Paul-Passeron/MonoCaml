use std::collections::HashMap;

use crate::{
    cfg::{Value, builder::Builder},
    lower::mono_to_cfg::MonoToCfg,
    monomorph::ConcrTy,
    poly_ir::{
        VarId,
        expr::{VBMarker, ValueBinding},
        id::Id,
        pattern::{Pattern, PatternNode},
    },
};

#[allow(unused)]
impl<'a> MonoToCfg<'a> {
    fn compute_rec_env(
        &mut self,
        bindings: &[ValueBinding<ConcrTy>],
    ) -> HashMap<Id<VBMarker>, Value> {
        let mut res = HashMap::new();
        for binding in bindings {
            let val = todo!();
            res.insert(binding.id, val);
        }
        res
    }

    fn compile_rec_bindings(
        &mut self,
        bindings: &[ValueBinding<ConcrTy>],
        subst: &mut HashMap<VarId, Value>,
        b: &mut Builder,
    ) {
        let rec_env = self.compute_rec_env(bindings);
        for binding in bindings {
            // allocate memory location
            if !binding.params.is_empty() {
                todo!()
            } else {
                todo!()
            }
        }
        todo!()
    }

    fn bind_pattern_to_value(
        &mut self,
        pat: &Pattern<ConcrTy>,
        value: Value,
        map: &mut HashMap<VarId, Value>,
        b: &mut Builder,
    ) {
        match &pat.node {
            PatternNode::Wildcard => (),
            PatternNode::Var(id) => {
                map.insert(*id, value);
            }
            PatternNode::Tuple(typed_nodes) => todo!(),
            PatternNode::Cons { ty, idx, arg } => todo!(),
        }
    }

    fn compile_binding(
        &mut self,
        binding: &ValueBinding<ConcrTy>,
        subst: &mut HashMap<VarId, Value>,
        b: &mut Builder,
    ) {
        if !binding.params.is_empty() {
            todo!()
        } else {
            let val = self.compile_expr(&binding.body, subst, b);
            self.bind_pattern_to_value(&binding.pat, val, subst, b)
        }
    }

    pub(super) fn compile_bindings(
        &mut self,
        rec: bool,
        bindings: &[ValueBinding<ConcrTy>],
        subst: &mut HashMap<VarId, Value>,
        b: &mut Builder,
    ) {
        if rec {
            self.compile_rec_bindings(bindings, subst, b)
        } else {
            bindings
                .iter()
                .map(|binding| self.compile_binding(binding, subst, b))
                .collect()
        }
    }
}
