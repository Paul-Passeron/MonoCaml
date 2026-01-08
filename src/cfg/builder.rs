use std::{collections::HashMap, mem};

use crate::{
    cfg::{
        BasicBlock, Cfg, Const, FunName, FunNameUse, Func, Instr, Label, LabelUse, Program, Sig,
        Terminator, Ty, TyCtx, Value,
        expr::Expr,
        var::{CfgVar, CfgVarUse},
    },
    helpers::unique::Use,
};

pub struct Builder {
    name: FunName,
    params: Vec<(CfgVar, Ty)>,
    ret_ty: Ty,
    label: Label,
    entry: LabelUse,
    // phis: HashMap<CfgVarUse, HashSet<CfgVarUse>>,
    locals: Vec<(CfgVar, Ty)>,
    instrs: Vec<Instr<CfgVar>>,
    blocks: Vec<BasicBlock>,
}

impl Builder {
    pub fn new(name: FunName, params: Vec<(CfgVar, Ty)>, ret_ty: Ty, ctx: &mut TyCtx) -> Self {
        let l = Label::fresh();
        let use_l = Use::from(&l);
        for p in &params {
            if p.1.is_zero_sized() {
                panic!("Zero-sized parameter not allowed");
            }
        }
        let res = Self {
            name,
            params,
            ret_ty,
            label: l,
            entry: use_l,
            locals: vec![],
            instrs: vec![],
            blocks: vec![],
        };

        let sig = Sig::new(
            res.params.iter().map(|(_, b)| b.clone()).collect(),
            res.ret_ty.clone(),
        );
        ctx.sigs.insert(Use::from(&res.name), sig);
        res
    }

    pub fn funname(&self) -> FunNameUse {
        Use::from(&self.name)
    }

    fn replace_lbl(&mut self, next: Label) -> Label {
        let mut l = next;
        mem::swap(&mut l, &mut self.label);
        l
    }

    fn replace_instrs(&mut self) -> Vec<Instr<CfgVar>> {
        let mut v = Vec::new();
        mem::swap(&mut v, &mut self.instrs);
        v
    }

    fn get_use_instrs(&mut self, ctx: &TyCtx) -> Vec<Instr<CfgVarUse>> {
        let instrs = self.replace_instrs();
        let (locals, instrs) = instrs
            .into_iter()
            .map(|x| x.into_use())
            .collect::<(Vec<Option<_>>, Vec<Instr<CfgVarUse>>)>();
        let locals = locals.into_iter().flatten().map(|x| {
            let ty = Use::from(&x).get_type(ctx);
            (x, ty)
        });
        self.locals.extend(locals);
        instrs
    }

    pub fn current_lbl(&self) -> LabelUse {
        (&self.label).into()
    }

    // pub fn get_phis(&mut self) -> HashMap<CfgVarUse, HashSet<CfgVarUse>> {
    //     let mut phis = HashMap::new();
    //     mem::swap(&mut self.phis, &mut phis);
    //     phis
    // }

    // pub fn add_phi_target(&mut self, ctx: &mut TyCtx, ty: Ty) -> CfgVarUse {
    //     assert!(!ty.is_zero_sized(), "phi target cannot be zero-sized");
    //     let new_local = self.assign(ctx, Expr::Phi(ty));
    //     self.phis.insert(new_local.clone(), HashSet::new());
    //     new_local
    // }

    // pub fn add_phi(&mut self, ctx: &mut TyCtx, to: CfgVarUse, val: CfgVarUse) {
    //     let target_ty = to.get_type(ctx);
    //     let val_ty = val.get_type(ctx);
    //     assert!(target_ty.matches(&val_ty), "{} vs {}", target_ty, val_ty);
    //     if self
    //         .params
    //         .iter()
    //         .map(|x| Use::from(&x.0))
    //         .find(|x| *x == val)
    //         .is_some()
    //     {
    //         let new_var = self.assign(ctx, Expr::value(val.into()));
    //         self.phis.get_mut(&to).unwrap().insert(new_var.clone());
    //     } else {
    //         self.phis.get_mut(&to).unwrap().insert(val);
    //     }
    // }

    pub fn finalize_block(&mut self, ctx: &TyCtx, t: Terminator, next: Label) -> LabelUse {
        let old_label = self.replace_lbl(next);
        let use_lbl: Use<_> = (&old_label).into();
        let instrs = self.get_use_instrs(ctx);
        // let phis = self.get_phis();
        let b = BasicBlock {
            label: old_label,
            // phis,
            instrs,
            terminator: t,
        };
        self.blocks.push(b);
        use_lbl
    }

    pub fn goto(&mut self, ctx: &TyCtx, lbl: LabelUse, next: Label) -> LabelUse {
        self.finalize_block(ctx, Terminator::Goto(lbl), next)
    }

    pub fn branch(
        &mut self,
        ctx: &TyCtx,
        cond: Value,
        then_bb: LabelUse,
        else_bb: LabelUse,
        next: Label,
    ) -> LabelUse {
        self.finalize_block(
            ctx,
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            },
            next,
        )
    }

    pub fn ret(&mut self, ctx: &TyCtx, value: Value) -> LabelUse {
        if self.ret_ty.is_zero_sized() {
            panic!("Cannot return value from void returning function")
        }
        if !self.ret_ty.matches(&value.get_type(ctx)) {
            panic!(
                "Cannot return value of type {} from function that should return {}",
                value.get_type(ctx),
                self.ret_ty
            );
        }
        self.finalize_block(ctx, Terminator::Return(Some(value)), Label::fresh())
    }

    pub fn ret_void(&mut self, ctx: &TyCtx) -> LabelUse {
        if !self.ret_ty.is_zero_sized() {
            panic!(
                "Cannot return void from function that should return {}",
                self.ret_ty
            )
        }
        self.finalize_block(ctx, Terminator::Return(None), Label::fresh())
    }

    pub fn finalize(self) -> Func {
        if self.instrs.len() > 0 {
            panic!("Cannot finalize builder if there is an unfinished basic block")
        }
        Func::new(
            self.name,
            self.params,
            self.ret_ty,
            Some(Cfg {
                locals: self.locals.into_iter().collect(),
                blocks: self.blocks,
                entry: self.entry,
            }),
        )
    }

    pub fn assign_to(&mut self, ctx: &mut TyCtx, var: CfgVar, expr: Expr) -> CfgVarUse {
        let ty = expr.get_type(ctx);
        let res = Use::from(&var);
        assert!(ty.matches(&res.get_type(ctx)));
        self.instrs.push(Instr::Assign(var, expr));
        res
    }

    pub fn assign(&mut self, ctx: &mut TyCtx, expr: Expr) -> CfgVarUse {
        let ty = expr.get_type(ctx);
        let var = ctx.new_var(ty);
        let res = (&var).into();
        self.instrs.push(Instr::Assign(var, expr));
        res
    }

    pub fn store(&mut self, ctx: &TyCtx, ptr: Value, value: Value) {
        let _ = ptr.get_type(ctx).into_inner(); // Asserts ptr is really a pointer
        self.instrs.push(Instr::Store { ptr, value })
    }

    pub fn get_params(&self) -> Vec<CfgVarUse> {
        self.params.iter().map(|(x, _)| x.into()).collect()
    }

    pub fn constant(&mut self, ctx: &mut TyCtx, c: Const) -> CfgVarUse {
        self.assign(ctx, Expr::Value(c.into()))
    }

    pub fn var(&mut self, ctx: &mut TyCtx, v: CfgVarUse) -> CfgVarUse {
        self.assign(ctx, Expr::Value(v.into()))
    }

    pub fn add(&mut self, ctx: &mut TyCtx, lhs: Value, rhs: Value) -> Value {
        match (&lhs, &rhs) {
            (Value::Const(Const::Int(l)), Value::Const(Const::Int(r))) => {
                Value::Const(Const::Int(l + r))
            }
            _ => self.assign(ctx, Expr::add(ctx, lhs, rhs)).into(),
        }
    }

    pub fn eq(&mut self, ctx: &mut TyCtx, lhs: Value, rhs: Value) -> Value {
        match (&lhs, &rhs) {
            (Value::Const(Const::Int(l)), Value::Const(Const::Int(r))) => {
                Value::Const(Const::Int(l + r))
            }
            _ => self.assign(ctx, Expr::eq(ctx, lhs, rhs)).into(),
        }
    }

    pub fn mul(&mut self, ctx: &mut TyCtx, lhs: Value, rhs: Value) -> Value {
        match (&lhs, &rhs) {
            (Value::Const(Const::Int(l)), Value::Const(Const::Int(r))) => {
                Value::Const(Const::Int(l * r))
            }
            _ => self.assign(ctx, Expr::mul(ctx, lhs, rhs)).into(),
        }
    }

    pub fn sub(&mut self, ctx: &mut TyCtx, lhs: Value, rhs: Value) -> Value {
        match (&lhs, &rhs) {
            (Value::Const(Const::Int(l)), Value::Const(Const::Int(r))) => {
                Value::Const(Const::Int(l - r))
            }
            _ => self.assign(ctx, Expr::sub(ctx, lhs, rhs)).into(),
        }
    }

    pub fn div(&mut self, ctx: &mut TyCtx, lhs: Value, rhs: Value) -> Value {
        match (&lhs, &rhs) {
            (Value::Const(Const::Int(l)), Value::Const(Const::Int(r))) => {
                Value::Const(Const::Int(l / r))
            }
            _ => self.assign(ctx, Expr::div(ctx, lhs, rhs)).into(),
        }
    }

    // pub fn call(&mut self, ctx: &mut TyCtx, closure: Value, arg: Value) -> CfgVarUse {
    //     self.assign(ctx, Expr::call(ctx, closure, arg))
    // }

    pub fn native_call(&mut self, ctx: &mut TyCtx, fun: Value, args: Vec<Value>) -> CfgVarUse {
        let new_args = args
            .into_iter()
            .filter(|x| !x.get_type(ctx).is_zero_sized())
            .collect();
        self.assign(ctx, Expr::native_call(ctx, fun, new_args))
    }

    pub fn get_element_ptr(
        &mut self,
        ctx: &mut TyCtx,
        ptr: Value,
        ty: Ty,
        index: usize,
    ) -> CfgVarUse {
        self.assign(ctx, Expr::get_element_ptr(ctx, ptr, ty, index))
    }

    pub fn extract(&mut self, ctx: &mut TyCtx, value: Value, index: usize) -> CfgVarUse {
        self.assign(ctx, Expr::extract(ctx, value, index))
    }

    pub fn load(&mut self, ctx: &mut TyCtx, ptr: Value, ty: Ty) -> CfgVarUse {
        self.assign(ctx, Expr::load(ctx, ptr, ty))
    }

    pub fn aggregate(&mut self, ctx: &mut TyCtx, values: Vec<Value>) -> Value {
        self.assign(ctx, Expr::aggregate(values)).into()
    }

    pub fn value(&mut self, ctx: &mut TyCtx, value: Value) -> CfgVarUse {
        self.assign(ctx, Expr::value(value))
    }

    pub fn malloc_single(&mut self, ctx: &mut TyCtx, ty: Ty) -> CfgVarUse {
        self.assign(ctx, Expr::malloc_single(ty))
    }

    pub fn malloc(&mut self, ctx: &mut TyCtx, ty: Ty, v: Value) -> CfgVarUse {
        self.assign(ctx, Expr::malloc(ty, v))
    }

    pub fn make_var(&mut self, ctx: &mut TyCtx, val: &Value) -> Use<CfgVar> {
        match val {
            Value::Var(x) => x.clone(),
            Value::Const(c) => {
                let ty = val.get_type(ctx);
                let v = ctx.new_var(ty);
                let res = Use::from(&v);
                self.assign_to(ctx, v, Expr::value(c.clone().into()));
                res
            }
        }
    }

    pub fn cast(&mut self, ctx: &mut TyCtx, val: &Value, ty: Ty) -> Use<CfgVar> {
        self.assign(ctx, Expr::cast(ty, val.clone()))
    }

    pub fn union(&mut self, ctx: &mut TyCtx, ty: Ty, val: Value, field: usize) -> Use<CfgVar> {
        assert!(ty.is_union());
        assert!(ty.field(field).matches(&val.get_type(ctx)));
        self.assign(ctx, Expr::union(ty, val, field))
    }

    pub fn alloca(&mut self, ctx: &mut TyCtx, ty: Ty) -> Use<CfgVar> {
        self.assign(ctx, Expr::alloca(ty))
    }
}

impl Program {
    pub fn add_func(&mut self, f: Func) {
        self.funcs.insert(f);
    }

    pub fn add_native_alias(&mut self, name: String, f: FunNameUse) {
        self.natives.insert(name, f);
    }
}

impl TyCtx {
    pub fn new() -> Self {
        Self {
            sigs: HashMap::new(),
            // globals: HashMap::new(),
            vars: HashMap::new(),
            natives: HashMap::new(),
        }
    }

    pub fn new_var(&mut self, ty: Ty) -> CfgVar {
        let res = CfgVar::fresh();
        self.vars.insert((&res).into(), ty);
        res
    }

    pub fn make_params<T: Iterator<Item = Ty>>(&mut self, tys: T) -> Vec<(CfgVar, Ty)> {
        tys.map(|ty| (self.new_var(ty.clone()), ty)).collect()
    }

    pub fn add_native_alias<S: ToString>(&mut self, name: S, f: FunNameUse) {
        self.natives.insert(name.to_string(), f);
    }

    pub fn dump_aliases_in_prog(&self, prog: &mut Program) {
        for (x, y) in &self.natives {
            prog.add_native_alias(x.clone(), y.clone());
        }
    }
}
