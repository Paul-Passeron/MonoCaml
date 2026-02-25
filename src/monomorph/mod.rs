use std::collections::{HashMap, HashSet};

use crate::{
    inference::{
        InferVarId,
        solved_ty::{SolvedCon, SolvedTy, TyVar},
    },
    poly_ir::{
        TypeId, ValueRef, VarId,
        expr::{Expr, ExprNode, VBMarker, ValueBinding},
        id::{Arena, Id},
        item::{Item, ItemNode},
        pattern::{Pattern, PatternNode},
    },
    resolution::VarInfo,
};

pub mod items;

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

    pub fn subst(&self, subst: &Subst) -> ConcrTy {
        match self {
            SolvedTy::Var(TyVar { id }) => subst.get(*id).clone(),
            SolvedTy::Con(SolvedCon { id, args }) => ConcrTy {
                id: *id,
                args: args.iter().map(|x| x.subst(subst)).collect(),
            },
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Subst {
    pub map: Vec<(InferVarId, ConcrTy)>,
}

impl Subst {
    pub fn get(&self, id: InferVarId) -> &ConcrTy {
        &self.map[self.map.binary_search_by_key(&id, |x| x.0).unwrap()].1
    }

    pub fn new() -> Self {
        Self { map: Vec::new() }
    }
}

impl Default for Subst {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct MonoCtx<'a> {
    pub items: Vec<&'a Item<SolvedTy>>,
    pub vars: &'a mut Arena<VarInfo>, // og vars
    pub id_to_bindings: HashMap<Id<VBMarker>, &'a ValueBinding<SolvedTy>>,

    pub builtins: HashMap<VarId, SolvedTy>,

    pub vbs: Arena<VBMarker>,

    pub vb_forwards: HashMap<Id<VBMarker>, Id<VBMarker>>,
    pub og_var_to_og_vb: HashMap<VarId, Id<VBMarker>>,
    pub specs: HashMap<(Id<VBMarker>, Subst), Id<VBMarker>>,
    pub bindings: Vec<ValueBinding<ConcrTy>>,
}

// fn extract_expr_vars_decl<T>(expr: Expr<T>)

#[allow(unused_variables)]
fn walk_expr_vars<'a>(
    e: &'a Expr<SolvedTy>,
    id: Id<VBMarker>,
    id_m: &mut HashMap<VarId, Id<VBMarker>>,
    forwards: &mut HashMap<Id<VBMarker>, Id<VBMarker>>,
    id_to_bindings: &mut HashMap<Id<VBMarker>, &'a ValueBinding<SolvedTy>>,
) {
    match &e.node {
        ExprNode::Var(_) => (),
        ExprNode::Const(_) => (),
        ExprNode::Unit => (),
        ExprNode::Let { bindings, body, .. } => {
            for b in bindings {
                let mut s = HashSet::new();
                MonoCtx::extract_pattern_vars(&b.pat, &mut s);
                for p in &b.params {
                    MonoCtx::extract_pattern_vars(p, &mut s);
                }
                for v in s {
                    id_m.insert(v, b.id);
                }
                forwards.insert(b.id, id);
                id_to_bindings.insert(b.id, b);
                walk_expr_vars(&b.body, b.id, id_m, forwards, id_to_bindings);
            }
            walk_expr_vars(body, id, id_m, forwards, id_to_bindings);
        }
        ExprNode::Apply { func, arg } => {
            walk_expr_vars(func, id, id_m, forwards, id_to_bindings);
            walk_expr_vars(arg, id, id_m, forwards, id_to_bindings);
        }
        ExprNode::Match { scrutinee, cases } => {
            walk_expr_vars(scrutinee, id, id_m, forwards, id_to_bindings);
            cases.iter().for_each(|case| {
                case.guard
                    .iter()
                    .for_each(|e| walk_expr_vars(e, id, id_m, forwards, id_to_bindings));
                walk_expr_vars(&case.body, id, id_m, forwards, id_to_bindings);
            })
        }
        ExprNode::Construct { path, arg } => {
            arg.iter()
                .for_each(|e| walk_expr_vars(e, id, id_m, forwards, id_to_bindings));
        }
        ExprNode::Sequence { first, second } => {
            walk_expr_vars(first, id, id_m, forwards, id_to_bindings);
            walk_expr_vars(second, id, id_m, forwards, id_to_bindings);
        }
        ExprNode::Constraint { expr, ty } => {
            walk_expr_vars(expr, id, id_m, forwards, id_to_bindings);
        }
        ExprNode::BinaryOp { op, left, right } => {
            walk_expr_vars(left, id, id_m, forwards, id_to_bindings);
            walk_expr_vars(right, id, id_m, forwards, id_to_bindings);
        }
        ExprNode::UnaryOp { op, expr } => {
            walk_expr_vars(expr, id, id_m, forwards, id_to_bindings);
        }
        ExprNode::IfThenElse {
            cond,
            then_expr,
            else_expr,
        } => {
            walk_expr_vars(cond, id, id_m, forwards, id_to_bindings);
            walk_expr_vars(then_expr, id, id_m, forwards, id_to_bindings);
            walk_expr_vars(else_expr, id, id_m, forwards, id_to_bindings);
        }
        ExprNode::Tuple(typed_nodes) => todo!(),
    }
}

impl<'a> MonoCtx<'a> {
    fn init(&mut self) {
        for item in &self.items {
            if let ItemNode::Value { bindings, .. } = &item.node {
                for b in bindings {
                    let mut s = HashSet::new();
                    Self::extract_pattern_vars(&b.pat, &mut s);
                    for p in &b.params {
                        Self::extract_pattern_vars(p, &mut s);
                    }
                    self.id_to_bindings.insert(b.id, b);
                    for id in dbg!(s) {
                        self.og_var_to_og_vb.insert(id, b.id);
                    }
                    walk_expr_vars(
                        &b.body,
                        b.id,
                        &mut self.og_var_to_og_vb,
                        &mut self.vb_forwards,
                        &mut self.id_to_bindings,
                    );
                }
            }
        }
    }

    fn extract_pattern_vars<T>(pat: &Pattern<T>, s: &mut HashSet<VarId>) {
        match pat.as_ref().node {
            PatternNode::Wildcard => (),
            PatternNode::Var(id) => {
                s.insert(*id);
            }
            PatternNode::Tuple(pats) => pats.iter().for_each(|p| Self::extract_pattern_vars(p, s)),
        }
    }

    pub fn new(
        program: &'a [Item<SolvedTy>],
        vars: &'a mut Arena<VarInfo>,
        builtins: HashMap<VarId, SolvedTy>,
    ) -> Self {
        let mut res = Self {
            items: Vec::from_iter(program),
            // vb_to_items: HashMap::new(),
            vars,
            vbs: Arena::new(),
            og_var_to_og_vb: HashMap::new(),
            specs: HashMap::new(),
            bindings: Vec::new(),
            vb_forwards: HashMap::new(),
            id_to_bindings: HashMap::new(),
            builtins,
        };
        res.init();
        res
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

    #[allow(unused)]
    fn instantiate_expr(
        &self,
        expr: &Expr<SolvedTy>,
        subst: &Subst,
        var_rename: &HashMap<VarId, VarId>,
    ) -> Expr<ConcrTy> {
        let node = match &expr.node {
            ExprNode::Const(constant) => ExprNode::Const(*constant),
            ExprNode::Var(ValueRef::Local(id)) => {
                if let Some(v) = var_rename.get(id) {
                    ExprNode::Var(ValueRef::Local(*v))
                } else {
                    // reference to other top level binding !
                    if let Some(vb_id) = self.og_var_to_og_vb.get(id) {
                        todo!()
                    } else if self.builtins.contains_key(id) {
                        // Builtin
                        ExprNode::Var(ValueRef::Local(*id))
                    } else {
                        unreachable!()
                    }
                }
            }
            ExprNode::Var(_) => todo!(),
            ExprNode::Apply { func, arg } => {
                let i_func = self.instantiate_expr(func, subst, var_rename);
                let i_arg = self.instantiate_expr(arg, subst, var_rename);
                ExprNode::Apply {
                    func: Box::new(i_func),
                    arg: Box::new(i_arg),
                }
            }
            ExprNode::IfThenElse {
                cond,
                then_expr,
                else_expr,
            } => {
                let i_cond = self.instantiate_expr(cond, subst, var_rename);
                let i_then_expr = self.instantiate_expr(then_expr, subst, var_rename);
                let i_else_expr = self.instantiate_expr(else_expr, subst, var_rename);
                ExprNode::IfThenElse {
                    cond: Box::new(i_cond),
                    then_expr: Box::new(i_then_expr),
                    else_expr: Box::new(i_else_expr),
                }
            }
            ExprNode::Let {
                recursive,
                bindings,
                body,
            } => todo!(),
            ExprNode::Match { scrutinee, cases } => todo!(),
            ExprNode::Tuple(typed_nodes) => todo!(),
            ExprNode::Construct { path, arg } => todo!(),
            ExprNode::Sequence { first, second } => todo!(),
            ExprNode::Constraint { expr, ty } => todo!(),
            ExprNode::BinaryOp { op, left, right } => {
                let i_left = self.instantiate_expr(left, subst, var_rename);
                let i_right = self.instantiate_expr(right, subst, var_rename);
                ExprNode::BinaryOp {
                    op: *op,
                    left: Box::new(i_left),
                    right: Box::new(i_right),
                }
            }
            ExprNode::UnaryOp { op, expr } => todo!(),
            ExprNode::Unit => todo!(),
        };
        let ty = expr.ty.subst(subst);
        Expr::new(node, expr.span, ty)
    }

    #[allow(unused)]
    fn instantiate_pattern(
        &self,
        pattern: &Pattern<SolvedTy>,
        subst: &Subst,
        var_rename: &HashMap<VarId, VarId>,
    ) -> Pattern<ConcrTy> {
        let node = match pattern.as_ref().node {
            PatternNode::Wildcard => PatternNode::Wildcard,
            PatternNode::Var(id) => {
                if var_rename.contains_key(&id) {
                    PatternNode::Var(*var_rename.get(&id).unwrap())
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        };
        let ty = pattern.ty.subst(subst);
        Pattern::new(node, pattern.span, ty)
    }

    fn find_original_binding(&self, id: Id<VBMarker>) -> &ValueBinding<SolvedTy> {
        self.id_to_bindings[&id]
    }

    fn new_var_of_id(&mut self, id: VarId) -> VarId {
        let name = self.vars[id].name;
        self.vars.alloc(VarInfo { name })
    }

    fn build_fresh_rename(&mut self, og_binding: &ValueBinding<SolvedTy>) -> HashMap<VarId, VarId> {
        let mut ids = HashSet::new();
        Self::extract_pattern_vars(&og_binding.pat, &mut ids);
        og_binding
            .params
            .iter()
            .for_each(|pat| Self::extract_pattern_vars(pat, &mut ids));
        ids.into_iter()
            .map(|id| (id, self.new_var_of_id(id)))
            .collect()
    }

    fn specialize_binding(&mut self, og_id: Id<VBMarker>, subst: Subst) -> Id<VBMarker> {
        if let Some(new_id) = self.specs.get(&(og_id, subst.clone())) {
            return *new_id;
        }
        let new_id = self.vbs.alloc(VBMarker);
        self.specs.insert((og_id, subst.clone()), new_id);

        let og_binding = self.find_original_binding(og_id).clone();

        let var_rename = self.build_fresh_rename(&og_binding);

        let new_binding = ValueBinding {
            id: new_id,
            pat: self.instantiate_pattern(&og_binding.pat, &subst, &var_rename),
            params: og_binding
                .params
                .iter()
                .map(|pat| self.instantiate_pattern(pat, &subst, &var_rename))
                .collect(),
            ty: og_binding.ty.subst(&subst),
            body: self.instantiate_expr(&og_binding.body, &subst, &var_rename),
        };

        self.bindings.push(new_binding);

        new_id
    }

    fn specialize(&mut self) {
        // collect all bindings that are already specialized
        let mut ids = vec![];
        for item in &self.items {
            if let ItemNode::Value { bindings, .. } = &item.node {
                for b in bindings {
                    let mut s = HashSet::new();
                    b.map_mut(&mut s, &mut |ty, s| match ty {
                        SolvedTy::Var(ty_var) => {
                            s.insert(ty_var.id);
                        }
                        SolvedTy::Con(SolvedCon { args, .. }) => {
                            args.iter().for_each(|x| s.extend(x.free_vars()))
                        }
                    });
                    if s.is_empty() {
                        ids.push(b.id);
                    }
                }
            }
        }

        ids.into_iter().for_each(|id| {
            self.specialize_binding(id, Subst::new());
        });
    }

    pub fn mono_program(&mut self) -> Res<Vec<Item<ConcrTy>>> {
        self.specialize();
        Ok(self.render())
    }
}
