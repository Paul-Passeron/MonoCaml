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
    locals: Vec<(CfgVar, Ty)>,
    instrs: Vec<Instr<CfgVar>>,
    blocks: Vec<BasicBlock>,
}

impl Builder {
    pub fn new(name: FunName, params: Vec<(CfgVar, Ty)>, ret_ty: Ty, ctx: &mut TyCtx) -> Self {
        let l = Label::fresh();
        let use_l = Use::from(&l);
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

        let sig = Sig {
            params: res.params.iter().map(|(_, b)| b.clone()).collect(),
            ret: Box::new(res.ret_ty.clone()),
        };
        ctx.sigs.insert(Use::from(&res.name), sig);
        res
    }

    fn replace_lbl(&mut self) -> Label {
        let mut l = Label::fresh();
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

    pub fn finalize_block(&mut self, ctx: &TyCtx, t: Terminator) -> LabelUse {
        let old_label = self.replace_lbl();
        let use_lbl: Use<_> = (&old_label).into();
        let instrs = self.get_use_instrs(ctx);
        let b = BasicBlock {
            label: old_label,
            instrs,
            terminator: t,
        };
        self.blocks.push(b);
        use_lbl
    }

    pub fn goto(&mut self, ctx: &TyCtx, lbl: LabelUse) -> LabelUse {
        self.finalize_block(ctx, Terminator::Goto(lbl))
    }

    pub fn branch(
        &mut self,
        ctx: &TyCtx,
        cond: Value,
        then_bb: LabelUse,
        else_bb: LabelUse,
    ) -> LabelUse {
        self.finalize_block(
            ctx,
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            },
        )
    }

    pub fn ret(&mut self, ctx: &TyCtx, value: Value) -> LabelUse {
        self.finalize_block(ctx, Terminator::Return(Some(value)))
    }

    pub fn ret_void(&mut self, ctx: &TyCtx) -> LabelUse {
        self.finalize_block(ctx, Terminator::Return(None))
    }

    pub fn finalize(self) -> Func {
        if self.instrs.len() > 0 {
            panic!("Cannot finalize builder if there is an unfinished basic block")
        }
        Func {
            name: self.name,
            params: self.params,
            ret_ty: self.ret_ty,
            cfg: Some(Cfg {
                locals: self.locals.into_iter().collect(),
                blocks: self.blocks,
                entry: self.entry,
            }),
        }
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

    pub fn call(&mut self, ctx: &mut TyCtx, closure: Value, arg: Value) -> CfgVarUse {
        self.assign(ctx, Expr::call(ctx, closure, arg))
    }

    pub fn native_call<S: ToString>(
        &mut self,
        ctx: &mut TyCtx,
        fun: S,
        args: Vec<Value>,
    ) -> CfgVarUse {
        self.assign(ctx, Expr::native_call(ctx, fun, args))
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

    pub fn aggregate(&mut self, ctx: &mut TyCtx, values: Vec<Value>) -> CfgVarUse {
        self.assign(ctx, Expr::aggregate(values))
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
