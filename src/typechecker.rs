use std::{
    collections::HashMap,
    fmt::{self, Display},
    hash::Hash,
    rc::Rc,
};

use crate::ast::{BinaryOp, Expression, Literal, Pattern};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum MonoType {
    TyVar(TyVar),
    TyCon(TyCon),
    Forall(Forall),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyVar {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum TCError {
    UnboundVariable(String),
    TypeMismatch(MonoType, MonoType),
    CyclicType(MonoType),
}

impl TyVar {
    pub fn as_mono(self) -> MonoType {
        MonoType::TyVar(self)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyCon {
    name: String,
    args: Vec<MonoType>,
    infix: bool,
}

impl TyCon {
    pub fn as_mono(self) -> MonoType {
        MonoType::TyCon(self)
    }

    pub fn name_ref(&self) -> &String {
        &self.name
    }

    pub fn args_ref(&self) -> &Vec<MonoType> {
        &self.args
    }

    pub fn name(self) -> String {
        self.name
    }

    pub fn args(self) -> Vec<MonoType> {
        self.args
    }

    pub fn infix(&self) -> bool {
        self.infix
    }

    pub fn new(name: String, args: Vec<MonoType>) -> Self {
        Self::new_pro(name, args, false)
    }

    pub fn new_infix(name: String, args: Vec<MonoType>) -> Self {
        Self::new_pro(name, args, true)
    }

    pub fn new_pro(name: String, args: Vec<MonoType>, infix: bool) -> Self {
        Self { name, args, infix }
    }
}

impl Display for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            MonoType::TyVar(x) => write!(f, "{x}"),
            MonoType::TyCon(x) => write!(f, "{x}"),
            MonoType::Forall(x) => write!(f, "{x}"),
        }
    }
}

impl Display for Forall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty)
    }
}

impl Display for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Display for TyCon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.infix() {
            assert!(self.args_ref().len() == 2);
            write!(
                f,
                "({} {} {})",
                self.args_ref()[0],
                self.name_ref(),
                self.args_ref()[1]
            )
        } else if self.args_ref().len() == 0 {
            write!(f, "{}", self.name)
        } else {
            write!(
                f,
                "({}{})",
                self.args_ref()
                    .iter()
                    .rev()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
                    + " ",
                self.name_ref()
            )
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Forall {
    pub tyvars: Vec<TyVar>,
    pub ty: Box<MonoType>,
}

impl Forall {
    pub fn as_mono(self) -> MonoType {
        MonoType::Forall(self)
    }
}

impl MonoType {
    pub fn list_type(ty: MonoType) -> MonoType {
        Self::TyCon(TyCon::new("list".to_string(), vec![ty]))
    }

    pub fn func_type(arg: MonoType, ret: MonoType) -> MonoType {
        Self::TyCon(TyCon::new_infix("->".to_string(), vec![arg, ret]))
    }

    pub fn tuple_type(left: MonoType, right: MonoType) -> MonoType {
        Self::TyCon(TyCon::new_infix("*".to_string(), vec![left, right]))
    }

    pub fn int_type() -> MonoType {
        Self::TyCon(TyCon::new("int".to_string(), vec![]))
    }

    pub fn unit_type() -> MonoType {
        Self::TyCon(TyCon::new("unit".to_string(), vec![]))
    }

    pub fn bool_type() -> MonoType {
        Self::TyCon(TyCon::new("bool".to_string(), vec![]))
    }

    pub fn tuple_type_from_vec(tys: Vec<MonoType>) -> MonoType {
        let mut i = tys.into_iter();
        if let Some(first) = i.next() {
            i.fold(first, |acc, ty| Self::tuple_type(acc, ty))
        } else {
            Self::unit_type()
        }
    }

    pub fn apply_subst(self, subst: &Subst) -> Self {
        match self {
            MonoType::TyVar(ty_var) => {
                if !subst.0.contains_key(&ty_var.name) {
                    MonoType::TyVar(ty_var)
                } else {
                    subst.0[&ty_var.name].clone()
                }
            }
            MonoType::TyCon(ty_con) => MonoType::TyCon(TyCon::new_pro(
                ty_con.name_ref().clone(),
                ty_con
                    .args_ref()
                    .clone()
                    .into_iter()
                    .map(|x| x.apply_subst(subst))
                    .collect(),
                ty_con.infix(),
            )),
            MonoType::Forall(forall) => MonoType::Forall(Forall {
                tyvars: forall.tyvars,
                ty: Box::new(forall.ty.apply_subst(subst)),
            }),
        }
    }

    pub fn contains(&self, contained: &MonoType) -> bool {
        if self == contained {
            true
        } else {
            match self {
                MonoType::TyVar(_) => false,
                MonoType::TyCon(ty_con) => ty_con.args.iter().any(|x| x.contains(contained)),
                MonoType::Forall(forall) => forall.ty.contains(contained),
            }
        }
    }
}

#[derive(Clone)]
pub struct Context {
    pub m: HashMap<String, Forall>,
    pub ty_var_count: Rc<u32>,
}

pub struct Subst(pub HashMap<String, MonoType>);

impl Context {
    pub fn new() -> Self {
        Self {
            m: HashMap::new(),
            ty_var_count: Rc::new(0),
        }
    }

    pub fn lookup(&self, name: &String) -> Option<Forall> {
        self.m.get(name).cloned()
    }

    pub fn new_type_var(&mut self) -> TyVar {
        let id = *self.ty_var_count.as_ref();
        unsafe { *Rc::get_mut_unchecked(&mut self.ty_var_count) = id + 1 };
        TyVar {
            name: format!("'ty{id}"),
        }
    }

    pub fn with_bindings(&self, bindings: HashMap<String, MonoType>) -> Self {
        let mut res = self.clone();
        for (key, val) in bindings {
            res.m.insert(
                key,
                Forall {
                    ty: Box::new(val),
                    tyvars: Vec::new(),
                },
            );
        }
        res
    }

    pub fn apply_subst(self, subst: Subst) -> Self {
        self.with_bindings(subst.0)
    }
}

impl Subst {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn extend(self, name: String, ty: MonoType) -> Self {
        let mut this = self;
        this.0.insert(name, ty);
        this
    }

    pub fn compose(self, newer: Self) -> Result<Self, TCError> {
        let mut res = HashMap::new();
        for (key, val) in self.0 {
            let val = if newer.0.contains_key(&key) {
                TypeChecker::unify_w(&val, &newer.0[&key])?
            } else {
                Subst::new().extend(key, val)
            };
            for (key, val) in val.0 {
                res.insert(key, val);
            }
        }
        for (key, val) in newer.0 {
            if res.contains_key(&key) {
                continue;
            }
            res.insert(key, val);
        }

        let res = Self(res);

        Ok(Self(
            res.0
                .iter()
                .map(|(k, x)| (k.clone(), x.clone().apply_subst(&res)))
                .collect(),
        ))
    }
}

pub struct TypeChecker;

impl TypeChecker {
    pub fn ty_of_lit(lit: &Literal) -> MonoType {
        match lit {
            Literal::Int(_) => MonoType::int_type(),
            Literal::Bool(_) => MonoType::bool_type(),
            _ => todo!(),
        }
    }

    pub fn unfold_pattern_bindings(
        p: &Pattern,
        ctx: &mut Context,
    ) -> (MonoType, HashMap<String, MonoType>) {
        fn aux(p: &Pattern, m: &mut HashMap<String, MonoType>, ctx: &mut Context) -> MonoType {
            match p {
                Pattern::Wildcard => ctx.new_type_var().as_mono(),
                Pattern::Symbol(x) => {
                    let new_tvar = ctx.new_type_var().as_mono();
                    m.insert(x.clone(), new_tvar.clone());
                    new_tvar
                }
                Pattern::Dict(items) => {
                    let mut tys = vec![];
                    for (_, item) in items {
                        tys.push(aux(item, m, ctx));
                    }
                    todo!()
                }
                Pattern::Tuple(patterns) => {
                    let mut tys = vec![];
                    for item in patterns {
                        tys.push(aux(item, m, ctx));
                    }
                    MonoType::tuple_type_from_vec(tys)
                }
                Pattern::Literal(lit) => TypeChecker::ty_of_lit(lit),
                Pattern::List(_) => todo!(),
            }
        }
        let mut m = HashMap::new();
        let ty = aux(p, &mut m, ctx);
        (ty, m)
    }

    pub fn unify_w(src: &MonoType, to: &MonoType) -> Result<Subst, TCError> {
        match src {
            MonoType::TyVar(ty_var) => {
                if to.contains(src) {
                    return Err(TCError::CyclicType(src.clone()));
                }
                return Ok(Subst::new().extend(ty_var.name.clone(), to.clone()));
            }
            _ => (),
        };
        match &to {
            MonoType::TyVar(_) => {
                return Self::unify_w(to, src);
            }
            _ => (),
        };

        match (src, to) {
            (MonoType::TyCon(s_con), MonoType::TyCon(t_con)) => {
                if s_con.name != t_con.name {
                    return Err(TCError::TypeMismatch(src.clone(), to.clone()));
                }
                if s_con.args.len() != t_con.args.len() {
                    return Err(TCError::TypeMismatch(src.clone(), to.clone()));
                }
                return s_con
                    .args
                    .iter()
                    .zip(t_con.args.iter())
                    .fold(Ok(Subst::new()), |s, (src, to)| {
                        s?.compose(Self::unify_w(src, to)?)
                    });
            }
            _ => (),
        }

        unreachable!()
    }

    pub fn infer_w(expr: &Expression, ctx: &mut Context) -> Result<(Subst, MonoType), TCError> {
        match expr {
            Expression::IntLit(_) => Ok((Subst::new(), MonoType::int_type())),
            Expression::BoolLit(_) => Ok((Subst::new(), MonoType::bool_type())),
            Expression::Var(name) => {
                let type_shceme = ctx
                    .lookup(name)
                    .ok_or(TCError::UnboundVariable(name.clone()))?;
                // .expect(format!("Unbound Variable {}", name).as_str());
                return Ok((Subst::new(), type_shceme.ty.as_ref().clone()));
            }
            Expression::Tuple(expressions) => {
                let (substs, tys) = expressions
                    .iter()
                    .map(|x| Self::infer_w(x, ctx))
                    .collect::<Result<(Vec<_>, Vec<_>), _>>()?;
                let subst = substs
                    .into_iter()
                    .fold(Ok(Subst::new()), |acc, x| acc?.compose(x))?;
                let ty = MonoType::tuple_type_from_vec(tys);
                Ok((subst, ty))
            }
            Expression::Call { fun, arg } => {
                let (subst1, callee_ty) = Self::infer_w(fun, ctx)?;
                let (subst2, arg_ty) = Self::infer_w(arg, ctx)?;
                let subst = subst1.compose(subst2)?;
                let ret_ty = ctx.new_type_var().as_mono();
                let uni_subst =
                    Self::unify_w(&callee_ty, &MonoType::func_type(arg_ty, ret_ty.clone()))?;
                Ok((subst.compose(uni_subst)?, ret_ty))
            }
            Expression::Let {
                recursive,
                binding,
                in_expr,
            } => {
                let (pat_ty, bindings) = Self::unfold_pattern_bindings(&binding.pat, ctx);
                let (pat_ty2, bindings2) = Self::unfold_pattern_bindings(&binding.pat, ctx);
                let mut new_ctx = ctx.with_bindings(bindings.clone());
                let ctx_for_binding = if *recursive {
                    &mut ctx.with_bindings(bindings2.clone())
                } else {
                    ctx
                };
                let (subst, binding_ty) = Self::infer_w(&binding.expr, ctx_for_binding)?;
                let subst = bindings2.iter().fold(Ok(subst), |acc, (k, x)| {
                    acc?.compose(Self::unify_w(x, &bindings[k])?)
                })?;
                let subst = subst
                    .compose(Self::unify_w(&pat_ty, &binding_ty)?)?
                    .compose(Self::unify_w(&pat_ty2, &pat_ty)?)?;

                let (in_subst, in_ty) = Self::infer_w(in_expr, &mut new_ctx)?;
                Ok((subst.compose(in_subst)?, in_ty))
            }
            Expression::Seq { fst, snd } => {
                let (subst, _) = Self::infer_w(fst, ctx)?;
                let (subst_seq, res_ty) = Self::infer_w(snd, ctx)?;
                Ok((subst.compose(subst_seq)?, res_ty))
            }
            Expression::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let res_ty = ctx.new_type_var().as_mono();
                let (subst, cond_ty) = Self::infer_w(cond, ctx)?;
                let (then_subst, then_ty) = Self::infer_w(then_branch, ctx)?;
                let (else_subst, else_ty) = Self::infer_w(else_branch, ctx)?;
                let subst = subst
                    .compose(then_subst)?
                    .compose(else_subst)?
                    .compose(Self::unify_w(&cond_ty, &MonoType::bool_type())?)?
                    .compose(Self::unify_w(&res_ty, &then_ty)?)?
                    .compose(Self::unify_w(&res_ty, &else_ty)?)?;
                Ok((subst, res_ty))
            }
            Expression::Lambda { param, body } => {
                let (arg, bindings) = Self::unfold_pattern_bindings(param, ctx);
                let (subst, ret) = Self::infer_w(body, &mut ctx.with_bindings(bindings))?;
                Ok((subst, MonoType::func_type(arg, ret)))
            }
            Expression::BinaryOp { op, left, right } => {
                let (subst_l, l_ty) = Self::infer_w(left, ctx)?;
                let (subst_r, r_ty) = Self::infer_w(right, ctx)?;
                let subst = subst_l.compose(subst_r)?;

                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        let int_ty = MonoType::int_type();
                        let s = subst
                            .compose(Self::unify_w(&l_ty, &int_ty)?)?
                            .compose(Self::unify_w(&r_ty, &int_ty)?)?;
                        Ok((s, int_ty))
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        let bool_ty = MonoType::bool_type();
                        let s = subst
                            .compose(Self::unify_w(&l_ty, &bool_ty)?)?
                            .compose(Self::unify_w(&r_ty, &bool_ty)?)?;
                        Ok((s, bool_ty))
                    }
                    BinaryOp::Cons => {
                        let list_ty = MonoType::list_type(l_ty);
                        let s = subst.compose(Self::unify_w(&r_ty, &list_ty)?)?;
                        Ok((s, list_ty))
                    }
                    BinaryOp::Eq | BinaryOp::Neq => {
                        let bool_ty = MonoType::bool_type();
                        let s = subst.compose(Self::unify_w(&l_ty, &r_ty)?)?;
                        Ok((s, bool_ty))
                    }
                }
            }

            Expression::List(expressions) => {
                let elem_ty = ctx.new_type_var().as_mono();
                let mut res_subst = Subst::new();
                for expr in expressions {
                    let (subst, expr_ty) = Self::infer_w(expr, ctx)?;
                    res_subst = res_subst
                        .compose(subst)?
                        .compose(Self::unify_w(&elem_ty, &expr_ty)?)?;
                }
                Ok((res_subst, MonoType::list_type(elem_ty)))
            }
            _ => todo!(),
        }
    }

    pub fn type_of(e: &Expression, ctx: &mut Context) -> Result<MonoType, TCError> {
        let (s, t) = Self::infer_w(e, ctx)?;
        Ok(t.apply_subst(&s))
    }
}
