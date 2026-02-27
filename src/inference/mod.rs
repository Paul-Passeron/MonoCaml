use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    inference::{
        error::Res,
        solved_ty::{MonoTy, SolvedCon, SolvedTy, TyCon, TyForall, TyVar},
    },
    lexer::interner::Symbol,
    parse_tree::expression::{BinaryOp, Constant},
    poly_ir::{
        TypeId, TypeParamId, ValueRef, VarId,
        expr::{Expr, ExprNode, MatchCase, ValueBinding},
        id::{Arena, Id},
        item::{Item, ItemNode, TypeDecl, TypeDeclInfo},
        pattern::{Pattern, PatternNode},
        type_expr::Type,
    },
    resolution::VarInfo,
};

pub mod builtins;
pub mod error;
pub mod solved_ty;

// Implementing Algorithm J for the moment

#[derive(Debug, Copy, Clone)]
pub struct InfMarker;

pub type InferVarId = Id<InfMarker>;

#[derive(Debug)]
pub struct InferenceCtx<'a> {
    pub map: HashMap<VarId, TyForall>,
    pub tys: Arena<MonoTy>,
    pub forwards: HashMap<Id<MonoTy>, Id<MonoTy>>,
    pub inf: Arena<InfMarker>,
    pub decls: &'a Arena<TypeDeclInfo>,
    pub names: &'a Arena<VarInfo>,
    pub builtins: &'a HashMap<Symbol, VarId>,
    pub type_decls: HashMap<TypeId, TypeDecl>,
    pub constructors: &'a HashMap<TypeId, Vec<Option<Type>>>,
    pub type_params: HashMap<TypeParamId, Id<MonoTy>>,
}

impl Id<MonoTy> {
    pub fn get<'a>(&self, ctx: &'a InferenceCtx) -> &'a MonoTy {
        &ctx.tys[*self]
    }

    pub fn find(&self, ctx: &InferenceCtx) -> Id<MonoTy> {
        if self.get(ctx).is_con() {
            *self
        } else if let Some(val) = ctx.forwards.get(self) {
            val.find(ctx)
        } else {
            *self
        }
    }

    pub fn make_equal_to(&self, other: Id<MonoTy>, ctx: &mut InferenceCtx) {
        let chain_end = self.find(ctx);

        assert!(
            chain_end.get(ctx).is_var() || chain_end == other,
            "Already resolved"
        );
        ctx.forwards.insert(chain_end, other);
    }
}

pub struct InferenceResult {
    pub items: Vec<Item<SolvedTy>>,
    pub builtins: HashMap<VarId, SolvedTy>,
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

impl<'a> InferenceCtx<'a> {
    pub fn new(
        decls: &'a Arena<TypeDeclInfo>,
        names: &'a Arena<VarInfo>,
        builtins: &'a HashMap<Symbol, VarId>,
        constructors: &'a HashMap<TypeId, Vec<Option<Type>>>,
    ) -> Self {
        let mut res = Self {
            map: HashMap::new(),
            tys: Arena::new(),
            forwards: HashMap::new(),
            inf: Arena::new(),
            type_decls: HashMap::new(),
            type_params: HashMap::new(),
            decls,
            names,
            builtins,
            constructors,
        };
        res.init_builtins();
        res
    }

    pub fn occurence_unify(&mut self, ty1: Id<MonoTy>, ty2: Id<MonoTy>) -> Res<()> {
        if ty1 == ty2 {
            return Ok(());
        }
        let mono1 = ty1.get(self);
        let mono2 = ty2.get(self);
        if mono1.occurs_in(mono2, self) {
            println!("{} vs {}", ty1.display(self), ty2.display(self));
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
                if con1.id != con2.id {
                    let s1 = con1.id.to_string(self);
                    let s2 = con2.id.to_string(self);
                    if ((s1 == "*" && s2 == "unit") || (s2 == "*" && s1 == "unit"))
                        && con1.args.is_empty()
                        && con2.args.is_empty()
                    {
                        // no op
                    } else {
                        return Err(format!(
                            "Con type mismatch: {} != {}",
                            ty1.display(self),
                            ty2.display(self)
                        ));
                    }
                }
                if con1.args.len() != con2.args.len() {
                    return Err(format!(
                        "Con type {:?} arg len mismatch : {} != {}",
                        con1.id,
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
                    .try_for_each(|(l, r)| self.unify_j(l, r))
            }
            _ => Err("Unexpected type".into()),
        }
    }

    #[inline(always)]
    pub fn fresh(&mut self) -> InferVarId {
        self.inf.alloc(InfMarker)
    }

    fn get_ty(&mut self, ty: MonoTy) -> Id<MonoTy> {
        for (id, t) in self.tys.iter() {
            if t == &ty {
                return id;
            }
        }
        self.tys.alloc(ty)
    }

    #[allow(unused)]
    fn type_to_mono(&mut self, ty: &Type) -> Id<MonoTy> {
        match ty {
            Type::Infer => {
                let fresh = self.fresh();
                self.tys.alloc(MonoTy::Var(TyVar { id: fresh }))
            }
            Type::Arrow { param, result } => {
                let mono_arg = self.type_to_mono(param);
                let mono_ret = self.type_to_mono(result);
                self.get_ty(MonoTy::func_ty(mono_arg, mono_ret, self))
            }
            Type::Tuple(items) => {
                let tys = items.iter().map(|x| self.type_to_mono(x)).collect();
                self.get_ty(MonoTy::tuple_ty(tys, self))
            }
            Type::Constr { id, args } => {
                let tys = args.iter().map(|x| self.type_to_mono(x)).collect();
                let mono = MonoTy::Con(TyCon { id: *id, args: tys });
                self.get_ty(mono)
            }
            Type::Param(i) => self.type_params[i],
            _ => todo!(),
        }
    }

    pub fn get_ty_of_const(&mut self, constant: &Constant) -> Id<MonoTy> {
        let mono = match constant {
            Constant::Int(_) => MonoTy::int_ty(self),
            Constant::Char(_) => MonoTy::char_ty(self),
            Constant::String(_) => MonoTy::string_ty(self),
            Constant::Float(_) => MonoTy::float_ty(self),
            Constant::Bool(_) => MonoTy::bool_ty(self),
        };
        self.get_ty(mono)
    }

    pub fn infer_expr(&mut self, expr: &Expr<Type>) -> Res<Expr<Id<MonoTy>>> {
        let span = expr.span;
        match expr.as_ref().node {
            ExprNode::Var(id) => {
                let node = ExprNode::Var(*id);

                let ty = match id {
                    ValueRef::Local(id) => {
                        let scheme = self.map[id].clone();
                        self.instantiate(&scheme)
                    }
                    ValueRef::Constructor { .. } => todo!(),
                };

                Ok(Expr::new(node, span, ty))
            }
            ExprNode::Const(constant) => {
                let node = ExprNode::Const(*constant);
                let ty = self.get_ty_of_const(constant);
                Ok(Expr::new(node, span, ty))
            }
            ExprNode::Let {
                recursive,
                bindings,
                body,
            } => {
                let bindings = self.infer_bindings(*recursive, bindings)?;
                let body = self.infer_expr(body)?;
                let ty = body.ty;
                Ok(Expr::new(
                    ExprNode::Let {
                        recursive: *recursive,
                        bindings,
                        body: Box::new(body),
                    },
                    span,
                    ty,
                ))
            }
            ExprNode::Apply { func, arg } => {
                let ifunc = self.infer_expr(func)?;
                let iarg = self.infer_expr(arg)?;
                let arg_ty = self.fresh_ty();
                let res_ty = self.fresh_ty();
                let func_ty = self.get_ty(MonoTy::func_ty(arg_ty, res_ty, self));
                self.unify_j(ifunc.ty, func_ty)?;
                self.unify_j(iarg.ty, arg_ty)?;
                Ok(Expr::new(
                    ExprNode::Apply {
                        func: Box::new(ifunc),
                        arg: Box::new(iarg),
                    },
                    span,
                    res_ty,
                ))
            }
            ExprNode::Match { scrutinee, cases } => {
                assert!(!cases.is_empty());
                let i_scrutinee = self.infer_expr(scrutinee)?;
                let body_ty = self.fresh_ty();
                let mut new_cases = vec![];
                for case in cases {
                    let i_pat = self.infer_pattern(&case.pattern)?;
                    self.bind_pattern_to_context(
                        &i_pat,
                        TyForall::mono(i_pat.ty.get(self).clone()),
                    )?;
                    self.unify_j(i_scrutinee.ty, i_pat.ty)?;
                    let i_guard = case
                        .guard
                        .as_ref()
                        .map(|x| self.infer_expr(x))
                        .transpose()?;
                    if let Some(g) = &i_guard {
                        let bool_ty = MonoTy::bool_ty(self);
                        let bool_ty = self.get_ty(bool_ty);
                        self.unify_j(g.ty, bool_ty)?;
                    }
                    let i_body = self.infer_expr(&case.body)?;
                    self.unify_j(i_body.ty, body_ty)?;
                    new_cases.push(MatchCase {
                        pattern: i_pat,
                        guard: i_guard.map(Box::new),
                        body: i_body,
                    });
                }
                Ok(Expr::new(
                    ExprNode::Match {
                        scrutinee: Box::new(i_scrutinee),
                        cases: new_cases,
                    },
                    span,
                    body_ty,
                ))
            }
            ExprNode::Tuple(exprs) => {
                let inf: Vec<_> = exprs
                    .iter()
                    .map(|e| self.infer_expr(e))
                    .collect::<Res<_>>()?;
                let t = self.get_ty(MonoTy::tuple_ty(inf.iter().map(|x| x.ty).collect(), self));
                Ok(Expr::new(ExprNode::Tuple(inf), span, t))
            }
            ExprNode::Construct { ty, idx, arg } => {
                let args: HashMap<_, _> = self.decls[*ty]
                    .params
                    .iter()
                    .map(|p| (*p, self.fresh_ty()))
                    .collect();

                let res_args = args.values().copied().collect::<Vec<_>>();
                let current_map = self.type_params.clone();
                self.type_params.extend(args);

                let current_cons = self.constructors[ty][*idx].as_ref();
                let arg_ty = current_cons.map(|t| self.type_to_mono(t));

                assert!(
                    arg_ty.is_some() == arg.is_some(),
                    "idx = {idx}\n{:?}\n\n***************\n\n{:?}",
                    arg_ty,
                    arg
                );

                let i_arg = arg.as_ref().map(|a| self.infer_expr(a)).transpose()?;

                for (p, ty) in i_arg.iter().zip(arg_ty) {
                    self.unify_j(p.ty, ty)?;
                }

                let res_ty = MonoTy::Con(TyCon {
                    id: *ty,
                    args: res_args,
                });

                self.type_params = current_map;
                Ok(Expr::new(
                    ExprNode::Construct {
                        ty: *ty,
                        idx: *idx,
                        arg: i_arg.map(Box::new),
                    },
                    span,
                    self.get_ty(res_ty),
                ))
            }
            ExprNode::Sequence { first, second } => {
                let inferred_first = self.infer_expr(first)?;
                let inferred_second = self.infer_expr(second)?;
                let ty = inferred_second.ty;
                Ok(Expr::new(
                    ExprNode::Sequence {
                        first: Box::new(inferred_first),
                        second: Box::new(inferred_second),
                    },
                    span,
                    ty,
                ))
            }
            ExprNode::Constraint { .. } => todo!(),
            ExprNode::BinaryOp { op, left, right } => {
                let (node, ty) = self.infer_binary_op(*op, left, right)?;
                Ok(Expr::new(node, span, ty))
            }
            ExprNode::UnaryOp { .. } => todo!(),
            ExprNode::Unit => Ok(Expr::new(
                ExprNode::Unit,
                span,
                self.get_ty(MonoTy::unit_ty(self)),
            )),
            ExprNode::IfThenElse {
                cond,
                then_expr,
                else_expr,
            } => {
                let i_cond = self.infer_expr(cond)?;
                let i_then_expr = self.infer_expr(then_expr)?;
                let i_else_expr = self.infer_expr(else_expr)?;
                let bool_ty = self.get_ty(MonoTy::bool_ty(self));
                self.unify_j(i_cond.ty, bool_ty)?;
                let arms_ty = self.fresh_ty();
                self.unify_j(i_then_expr.ty, arms_ty)?;
                self.unify_j(i_else_expr.ty, arms_ty)?;
                Ok(Expr::new(
                    ExprNode::IfThenElse {
                        cond: Box::new(i_cond),
                        then_expr: Box::new(i_then_expr),
                        else_expr: Box::new(i_else_expr),
                    },
                    span,
                    arms_ty,
                ))
            }
        }
    }

    pub fn infer_pattern(&mut self, pattern: &Pattern<Type>) -> Res<Pattern<Id<MonoTy>>> {
        let (node, ty) = match &pattern.node {
            PatternNode::Wildcard => (PatternNode::Wildcard, self.fresh_ty()),
            PatternNode::Var(id) => (PatternNode::Var(*id), self.fresh_ty()),
            PatternNode::Tuple(typed_nodes) => {
                if !typed_nodes.is_empty() {
                    let nodes = typed_nodes
                        .iter()
                        .map(|node| self.infer_pattern(node))
                        .collect::<Res<Vec<_>>>()?;

                    let mono = MonoTy::tuple_ty(nodes.iter().map(|x| x.ty).collect(), self);
                    let ty = self.get_ty(mono);
                    (PatternNode::Tuple(nodes), ty)
                } else {
                    (
                        PatternNode::Tuple(vec![]),
                        self.get_ty(MonoTy::unit_ty(self)),
                    )
                }
            }
            PatternNode::Cons { ty, idx, arg } => {
                let args: HashMap<_, _> = self.decls[*ty]
                    .params
                    .iter()
                    .map(|p| (*p, self.fresh_ty()))
                    .collect();

                let res_args = args.values().copied().collect::<Vec<_>>();
                let current_map = self.type_params.clone();
                self.type_params.extend(args);

                let current_cons = self.constructors[ty][*idx].as_ref();

                let arg_ty = current_cons.map(|t| self.type_to_mono(t));

                assert!(arg_ty.iter().len() == arg.iter().len());

                let i_arg = arg.as_ref().map(|a| self.infer_pattern(a)).transpose()?;

                for (p, ty) in i_arg.iter().zip(arg_ty) {
                    self.unify_j(p.ty, ty)?;
                }

                let res_ty = MonoTy::Con(TyCon {
                    id: *ty,
                    args: res_args,
                });

                self.type_params = current_map;
                (
                    PatternNode::Cons {
                        ty: *ty,
                        idx: *idx,
                        arg: i_arg.map(Box::new),
                    },
                    self.get_ty(res_ty),
                )
            }
        };
        if !pattern.ty.is_infer() {
            let constr = self.type_to_mono(&pattern.ty);
            self.unify_j(constr, ty)?;
        }
        Ok(Pattern::new(node, pattern.span, ty))
    }

    pub fn infer_bindings(
        &mut self,
        rec: bool,
        bindings: &[ValueBinding<Type>],
    ) -> Res<Vec<ValueBinding<Id<MonoTy>>>> {
        let mut new_bindings = vec![];
        let pats: Vec<_> = bindings
            .iter()
            .map(|binding| self.infer_pattern(&binding.pat))
            .collect::<Res<_>>()?;
        let params = bindings
            .iter()
            .map(|binding| {
                binding
                    .params
                    .iter()
                    .map(|param| self.infer_pattern(param))
                    .collect::<Res<Vec<_>>>()
            })
            .collect::<Res<Vec<_>>>()?;
        for ((pat, params), (expr, constr, id)) in pats
            .into_iter()
            .zip(params)
            .zip(bindings.iter().map(|b| (&b.body, &b.ty, b.id)))
        {
            let opt_body_ty = if rec {
                let body = self.fresh_ty();
                let full_ty = params.iter().rev().fold(body, |acc, param_ty| {
                    self.get_ty(MonoTy::func_ty(param_ty.ty, acc, self))
                });
                let scheme = TyForall {
                    tyvars: vec![],
                    ty: Box::new(full_ty.get(self).clone()),
                };
                self.bind_pattern_to_context(&pat, scheme)?;
                Some(body)
            } else {
                None
            };

            for param in params.iter() {
                self.bind_pattern_to_context(
                    param,
                    TyForall {
                        tyvars: vec![],
                        ty: Box::new(param.ty.get(self).clone()),
                    },
                )?;
            }

            let body = self.infer_expr(expr)?;
            let full_ty = params.iter().rev().fold(body.ty, |acc, param_ty| {
                self.get_ty(MonoTy::func_ty(param_ty.ty, acc, self))
            });
            if let Some(body_ty) = opt_body_ty {
                self.unify_j(body.ty, body_ty)?;
            }
            if !constr.is_infer() {
                let mono = self.type_to_mono(constr);
                self.unify_j(body.ty, mono)?;
            }
            self.unify_j(pat.ty, full_ty)?;
            let scheme = self.generalize(full_ty);
            self.bind_pattern_to_context(&pat, scheme)?;
            new_bindings.push(ValueBinding {
                id,
                pat,
                params,
                ty: full_ty,
                body,
            })
        }
        Ok(new_bindings)
    }

    pub fn infer_item(&mut self, item: &Item<Type>) -> Res<Item<Id<MonoTy>>> {
        let node = item.as_ref().node;
        let span = item.span;
        match node {
            ItemNode::Type { .. } => todo!(),
            ItemNode::Value {
                bindings,
                recursive,
            } => {
                let new_bindings = self.infer_bindings(*recursive, bindings)?;
                Ok(Item::new(
                    ItemNode::Value {
                        recursive: *recursive,
                        bindings: new_bindings,
                    },
                    span,
                    (),
                ))
            }
        }
    }

    fn collect_vars(&mut self, ty: Id<MonoTy>) -> Vec<InferVarId> {
        fn aux(ty: Id<MonoTy>, ctx: &InferenceCtx, set: &mut HashSet<InferVarId>) {
            let ty = ty.find(ctx);
            let ty = ty.get(ctx);
            match ty {
                MonoTy::Var(ty_var) => {
                    set.insert(ty_var.id);
                }
                MonoTy::Con(ty_con) => ty_con.args.iter().for_each(|x| aux(*x, ctx, set)),
            }
        }
        let mut s = HashSet::new();
        aux(ty, self, &mut s);
        let mut v: Vec<_> = s.into_iter().collect();
        v.sort();
        v
    }

    fn subst(&mut self, ty: Id<MonoTy>, vars: &HashMap<InferVarId, InferVarId>) -> Id<MonoTy> {
        let ty = ty.find(self);
        let ty = ty.get(self);
        let t = match ty {
            MonoTy::Var(ty_var) => MonoTy::Var(TyVar {
                id: if let Some(id) = vars.get(&ty_var.id) {
                    *id
                } else {
                    ty_var.id
                },
            }),
            MonoTy::Con(TyCon { id, args }) => MonoTy::Con(TyCon {
                id: *id,
                args: args
                    .iter()
                    .copied()
                    .collect::<Box<[_]>>()
                    .into_iter()
                    .map(|t| self.subst(t, vars))
                    .collect(),
            }),
        };
        self.get_ty(t)
    }

    pub fn generalize(&mut self, ty: Id<MonoTy>) -> TyForall {
        let ty = ty.find(self);
        let vars = self.collect_vars(ty);
        let new_vars: Vec<_> = vars.iter().map(|_| self.fresh()).collect();
        let vars: HashMap<InferVarId, InferVarId> =
            HashMap::from_iter(vars.into_iter().zip(new_vars.iter().copied()));

        let res = self.subst(ty, &vars);

        TyForall {
            tyvars: new_vars.into_iter().map(|x| TyVar { id: x }).collect(),
            ty: Box::new(res.get(self).clone()),
        }
    }

    pub fn bind_pattern_to_context<T>(&mut self, pat: &Pattern<T>, scheme: TyForall) -> Res<()> {
        match &pat.node {
            PatternNode::Wildcard => (),
            PatternNode::Var(id) => {
                self.map.insert(*id, scheme);
            }
            PatternNode::Tuple(pats) => {
                if !pats.is_empty() {
                    match &*scheme.ty {
                        MonoTy::Con(TyCon { id, args }) if &id.to_string(self) == "*" => {
                            if args.len() != pats.len() {
                                return Err("Tuple pattern length mismatch".to_string());
                            }
                            for (arg, pat) in args.iter().zip(pats.iter()) {
                                self.bind_pattern_to_context(
                                    pat,
                                    TyForall::mono(arg.get(self).clone()),
                                )?;
                            }
                        }
                        _ => return Err("Expected tuple type".to_string()),
                    }
                } else if let Some(ty_con) = scheme.ty.as_con()
                    && &ty_con.id.to_string(self) == "unit"
                {
                    // pass
                } else {
                    return Err("Expected unit type.".to_string());
                }
            }
            PatternNode::Cons { ty, idx, arg } => {
                let infos = &self.decls[*ty];

                let current_map = self.type_params.clone();
                self.type_params.extend(
                    infos
                        .params
                        .iter()
                        .copied()
                        .zip(scheme.ty.as_con().unwrap().args.iter().copied()),
                );

                let cons_ty = self.constructors[ty][*idx].as_ref();
                let arg_ty = cons_ty.map(|t| self.type_to_mono(t));
                assert!(arg_ty.is_some() == arg.is_some());

                for (arg_pat, arg_ty) in arg.iter().zip(arg_ty) {
                    let forall = TyForall::mono(arg_ty.get(self).clone());
                    self.bind_pattern_to_context(arg_pat, forall)?;
                }

                self.type_params = current_map;
            }
        }
        Ok(())
    }

    pub fn instantiate(&mut self, forall: &TyForall) -> Id<MonoTy> {
        let replacement = HashMap::from_iter(forall.tyvars.iter().map(|id| (id.id, self.fresh())));
        let id = self.get_ty(forall.ty.as_ref().clone());
        self.subst(id, &replacement)
    }

    pub fn into_solved(&self, ty: Id<MonoTy>) -> SolvedTy {
        let end = ty.find(self);
        match end.get(self).clone() {
            MonoTy::Var(ty_var) => SolvedTy::Var(ty_var),
            MonoTy::Con(TyCon { id, args }) => SolvedTy::Con(SolvedCon {
                id,
                args: args.iter().map(|x| self.into_solved(*x)).collect(),
            }),
        }
    }

    pub fn infer_program(&mut self, items: &[Item<Type>]) -> Res<InferenceResult> {
        fn replace_infer(
            ty: &SolvedTy,
            ctx: &mut (HashMap<InferVarId, InferVarId>, Arena<InfMarker>),
        ) -> SolvedTy {
            match ty {
                SolvedTy::Var(ty_var) => {
                    let id = if let Some(id) = ctx.0.get(&ty_var.id) {
                        *id
                    } else {
                        let id = ctx.1.alloc(InfMarker);
                        ctx.0.insert(ty_var.id, id);
                        id
                    };
                    SolvedTy::Var(TyVar { id })
                }
                SolvedTy::Con(SolvedCon { id, args }) => SolvedTy::Con(SolvedCon {
                    id: *id,
                    args: args.iter().map(|x| replace_infer(x, ctx)).collect(),
                }),
            }
        }

        // inferring items
        let inferred = items
            .iter()
            .map(|item| self.infer_item(item))
            .collect::<Res<Vec<_>>>()?;

        // Cleaning up the free variables
        let replace_ctx = &mut (HashMap::new(), Arena::new());

        Ok(InferenceResult {
            items: inferred
                .into_iter()
                .map(|id| {
                    id
                        // From MonoTy to SolvedTy
                        .map(self, &mut |id, ctx| ctx.into_solved(*id))
                        // Cleaning up the unnused `InferVarId`s
                        .map_mut(replace_ctx, &mut |ty, ctx| replace_infer(ty, ctx))
                })
                .collect(),
            builtins: self
                .builtins
                .values()
                .map(|id| {
                    let instantiated = self.instantiate(&self.map[id].clone());
                    let solved_ty = self.into_solved(instantiated);
                    (*id, replace_infer(&solved_ty, replace_ctx))
                })
                .collect(),
        })
    }

    pub fn fresh_ty(&mut self) -> Id<MonoTy> {
        let id = self.fresh();
        let mono = MonoTy::Var(TyVar { id });
        self.tys.alloc(mono)
    }

    fn infer_binary_op(
        &mut self,
        op: BinaryOp,
        left: &Expr<Type>,
        right: &Expr<Type>,
    ) -> Res<(ExprNode<Id<MonoTy>>, Id<MonoTy>)> {
        match &op {
            BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Star | BinaryOp::Div => {
                let left = self.infer_expr(left)?;
                let right = self.infer_expr(right)?;
                let int_ty = self.get_ty(MonoTy::int_ty(self));
                self.unify_j(left.ty, int_ty)?;
                self.unify_j(right.ty, int_ty)?;
                Ok((
                    ExprNode::BinaryOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    int_ty,
                ))
            }
            BinaryOp::GEq | BinaryOp::GT | BinaryOp::LEq | BinaryOp::LT => {
                let left = self.infer_expr(left)?;
                let right = self.infer_expr(right)?;
                let bool_ty = self.get_ty(MonoTy::bool_ty(self));
                let int_ty = self.get_ty(MonoTy::int_ty(self));
                self.unify_j(left.ty, int_ty)?;
                self.unify_j(right.ty, int_ty)?;
                Ok((
                    ExprNode::BinaryOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    bool_ty,
                ))
            }
            BinaryOp::Eq | BinaryOp::NEq => {
                let left = self.infer_expr(left)?;
                let right = self.infer_expr(right)?;
                let bool_ty = self.get_ty(MonoTy::bool_ty(self));
                self.unify_j(left.ty, right.ty)?;
                Ok((
                    ExprNode::BinaryOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    bool_ty,
                ))
            }
            x => todo!("{x:#?}"),
        }
    }
}

impl Id<MonoTy> {
    pub fn display(&self, ctx: &InferenceCtx) -> String {
        let end = self.find(ctx);
        let mono = end.get(ctx);
        match mono {
            MonoTy::Var(ty_var) => format!("{{TyVar({})}}", ty_var.id.raw()),
            MonoTy::Con(ty_con) => {
                let s = ty_con.id.to_string(ctx);
                if &s == "*" || &s == "->" {
                    format!(
                        "({})",
                        ty_con
                            .args
                            .iter()
                            .map(|x| x.display(ctx))
                            .collect::<Vec<_>>()
                            .join(format!(" {s} ").as_str()),
                    )
                } else {
                    format!(
                        "{}{}{}",
                        ty_con
                            .args
                            .iter()
                            .map(|x| x.display(ctx))
                            .collect::<Vec<_>>()
                            .join(" "),
                        if ty_con.args.is_empty() { "" } else { " " },
                        ty_con.id.to_string(ctx)
                    )
                }
            }
        }
    }
}

impl TypeId {
    pub fn to_string(self, ctx: &InferenceCtx) -> String {
        ctx.decls[self].name.to_string()
    }

    pub fn from_name(s: &str, decls: &Arena<TypeDeclInfo>) -> Option<Self> {
        for (id, TypeDeclInfo { name, .. }) in decls.iter() {
            if name.to_string() == s {
                return Some(id);
            }
        }
        None
    }
}
