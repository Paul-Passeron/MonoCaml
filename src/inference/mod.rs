use std::collections::{HashMap, HashSet};

use crate::{
    inference::{
        error::Res,
        solved_ty::{MonoTy, TyCon, TyForall, TyVar},
    },
    lexer::interner::Symbol,
    parse_tree::expression::{BinaryOp, Constant},
    poly_ir::{
        ValueRef, VarId,
        expr::{Expr, ExprNode, ValueBinding},
        id::{Arena, Id},
        item::{Item, ItemNode, TypeDeclInfo},
        pattern::{Pattern, PatternNode},
        type_expr::Type,
    },
    resolution::VarInfo,
};

pub mod builtins;
pub mod error;
pub mod solved_ty;

// Implementing Algorithm J for the moment

#[derive(Debug)]
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
    ) -> Self {
        let mut res = Self {
            map: HashMap::new(),
            tys: Arena::new(),
            forwards: HashMap::new(),
            inf: Arena::new(),
            decls,
            names,
            builtins,
        };
        res.init_builtins();
        res
    }

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
                self.get_ty(MonoTy::func_ty(mono_arg, mono_ret))
            }
            Type::Tuple(items) => {
                let tys = items.iter().map(|x| self.type_to_mono(x)).collect();
                self.get_ty(MonoTy::tuple_ty(tys))
            }
            Type::Constr { id, args } => {
                let name = self.decls[*id].name;
                let tys = args.iter().map(|x| self.type_to_mono(x)).collect();
                let mono = MonoTy::Con(TyCon { name, args: tys });
                self.get_ty(mono)
            }
            _ => todo!(),
        }
    }

    pub fn get_ty_of_const(&mut self, constant: &Constant) -> Id<MonoTy> {
        let mono = match constant {
            Constant::Int(_) => MonoTy::int_ty(),
            Constant::Char(_) => MonoTy::char_ty(),
            Constant::String(_) => MonoTy::string_ty(),
            Constant::Float(_) => MonoTy::float_ty(),
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
                        println!("Looking for the scheme of {}", self.names[*id].name);
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
            ExprNode::Function { .. } => todo!(),
            ExprNode::Apply { func, arg } => {
                let ifunc = self.infer_expr(func)?;
                let iarg = self.infer_expr(arg)?;
                let arg_ty = self.fresh_ty();
                let res_ty = self.fresh_ty();
                let func_ty = self.get_ty(MonoTy::func_ty(arg_ty, res_ty));
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
            ExprNode::Match { .. } => todo!(),
            ExprNode::Tuple(exprs) => {
                let inf: Vec<_> = exprs
                    .iter()
                    .map(|e| self.infer_expr(e))
                    .collect::<Res<_>>()?;
                let t = self.get_ty(MonoTy::tuple_ty(inf.iter().map(|x| x.ty).collect()));
                Ok(Expr::new(ExprNode::Tuple(inf), span, t))
            }
            ExprNode::Construct { .. } => todo!(),
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
                self.get_ty(MonoTy::unit_ty()),
            )),
            ExprNode::IfThenElse {
                cond,
                then_expr,
                else_expr,
            } => {
                let i_cond = self.infer_expr(cond)?;
                let i_then_expr = self.infer_expr(then_expr)?;
                let i_else_expr = self.infer_expr(else_expr)?;
                let bool_ty = self.get_ty(MonoTy::bool_ty());
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
            ExprNode::Fun { arg, body } => {
                let i_arg = self.infer_pattern(arg)?;
                let i_body = self.infer_expr(body)?;
                let arg_ty = self.fresh_ty();
                let body_ty = self.fresh_ty();
                let func_ty = self.get_ty(MonoTy::func_ty(arg_ty, body_ty));
                self.unify_j(i_arg.ty, arg_ty)?;
                self.unify_j(i_body.ty, body_ty)?;
                Ok(Expr::new(
                    ExprNode::Fun {
                        arg: i_arg,
                        body: Box::new(i_body),
                    },
                    span,
                    func_ty,
                ))
            }
        }
    }

    pub fn infer_pattern(&mut self, pattern: &Pattern<Type>) -> Res<Pattern<Id<MonoTy>>> {
        let (node, ty) = match pattern.as_ref().node {
            PatternNode::Wildcard => (PatternNode::Wildcard, self.fresh_ty()),
            PatternNode::Var(id) => (PatternNode::Var(*id), self.fresh_ty()),
            PatternNode::Tuple(typed_nodes) => {
                if typed_nodes.is_empty() {
                    let nodes = typed_nodes
                        .iter()
                        .map(|node| self.infer_pattern(node))
                        .collect::<Res<Vec<_>>>()?;

                    let mono = MonoTy::tuple_ty(nodes.iter().map(|x| x.ty).collect());
                    let ty = self.get_ty(mono);
                    (PatternNode::Tuple(nodes), ty)
                } else {
                    (PatternNode::Tuple(vec![]), self.get_ty(MonoTy::unit_ty()))
                }
            }
        };
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
        for ((pat, params), expr) in pats
            .into_iter()
            .zip(params)
            .zip(bindings.iter().map(|b| &b.body))
        {
            let opt_body_ty = if rec {
                let body = self.fresh_ty();
                let full_ty = params.iter().rev().fold(body, |acc, param_ty| {
                    self.get_ty(MonoTy::func_ty(param_ty.ty, acc))
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
                self.get_ty(MonoTy::func_ty(param_ty.ty, acc))
            });
            if let Some(body_ty) = opt_body_ty {
                self.unify_j(body.ty, body_ty)?;
            }
            self.unify_j(pat.ty, full_ty)?;
            let scheme = self.generalize(full_ty);
            self.bind_pattern_to_context(&pat, scheme)?;

            new_bindings.push(ValueBinding {
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
        fn aux(ty: &MonoTy, ctx: &InferenceCtx, set: &mut HashSet<InferVarId>) {
            match ty {
                MonoTy::Var(ty_var) => {
                    set.insert(ty_var.id);
                }
                MonoTy::Con(ty_con) => ty_con.args.iter().for_each(|x| aux(x.get(ctx), ctx, set)),
            }
        }
        let mut s = HashSet::new();
        aux(ty.get(self), self, &mut s);
        let mut v: Vec<_> = s.into_iter().collect();
        v.sort();
        v
    }

    fn subst(&mut self, ty: &MonoTy, vars: &HashMap<InferVarId, InferVarId>) -> Id<MonoTy> {
        let t = match ty {
            MonoTy::Var(ty_var) => MonoTy::Var(TyVar {
                id: if let Some(id) = vars.get(&ty_var.id) {
                    *id
                } else {
                    ty_var.id
                },
            }),
            MonoTy::Con(TyCon { name, args }) => MonoTy::Con(TyCon {
                name: *name,
                args: args
                    .iter()
                    .map(|t| t.get(self).clone())
                    .collect::<Vec<_>>()
                    .iter()
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

        let res = self.subst(&ty.get(self).clone(), &vars);

        TyForall {
            tyvars: new_vars.into_iter().map(|x| TyVar { id: x }).collect(),
            ty: Box::new(res.get(self).clone()),
        }
    }

    pub fn bind_pattern_to_context<T>(&mut self, pat: &Pattern<T>, scheme: TyForall) -> Res<()> {
        match pat.as_ref().node {
            PatternNode::Wildcard => (),
            PatternNode::Var(id) => {
                self.map.insert(*id, scheme);
            }
            PatternNode::Tuple(pats) => {
                if !pats.is_empty() {
                    match &*scheme.ty {
                        MonoTy::Con(TyCon { name, args }) if &name.to_string() == "*" => {
                            if args.len() != pats.len() {
                                return Err("Tuple pattern length mismatch".to_string());
                            }
                            for (arg, pat) in args.iter().zip(pats.iter()) {
                                self.bind_pattern_to_context(
                                    pat,
                                    TyForall {
                                        tyvars: vec![],
                                        ty: Box::new(arg.get(self).clone()),
                                    },
                                )?;
                            }
                        }
                        _ => return Err("Expected tuple type".to_string()),
                    }
                } else {
                    if let Some(ty_con) = scheme.ty.as_con()
                        && &ty_con.name.to_string() == "unit"
                    {
                    } else {
                        return Err("Expected unit type.".to_string());
                    }
                }
            }
        }
        Ok(())
    }

    pub fn instantiate(&mut self, forall: &TyForall) -> Id<MonoTy> {
        let replacement = HashMap::from_iter(forall.tyvars.iter().map(|id| (id.id, self.fresh())));
        self.subst(&forall.ty, &replacement)
    }

    pub fn infer_program(&mut self, items: &[Item<Type>]) -> Res<Vec<Item<Id<MonoTy>>>> {
        items.iter().map(|item| self.infer_item(item)).collect()
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
                let int_ty = self.get_ty(MonoTy::int_ty());
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
                let bool_ty = self.get_ty(MonoTy::bool_ty());
                let int_ty = self.get_ty(MonoTy::int_ty());
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
                let s = ty_con.name.to_string();
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
                        ty_con.name
                    )
                }
            }
        }
    }
}
