use std::{collections::HashMap, mem};

use crate::{
    cfg::{
        BasicBlock, Cfg, Expr, FunName, FunNameUse, Func, Instr, Label, LabelUse, Program, Sig,
        Terminator, Ty, TyCtx, Value,
        var::{CfgVar, CfgVarUse},
    },
    helpers::unique::Use,
};

pub struct Builder {
    name: FunName,
    params: Vec<(CfgVar, Ty)>,
    ret_ty: Ty,
    label: Label,
    locals: Vec<(CfgVar, Ty)>,
    instrs: Vec<Instr<CfgVar>>,
    blocks: Vec<BasicBlock>,
}

impl Builder {
    pub fn new(name: FunName, params: Vec<Ty>, ret_ty: Ty, ctx: &mut TyCtx) -> Self {
        let res = Self {
            name,
            params: ctx.make_params(params.into_iter()),
            ret_ty,
            label: Label::fresh(),
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
            }),
        }
    }

    pub fn assign(&mut self, expr: Expr, ctx: &mut TyCtx) -> CfgVarUse {
        let ty = expr.get_type(ctx);
        let var = ctx.new_var(ty);
        let res = (&var).into();
        self.instrs.push(Instr::Assign(var, expr));
        res
    }

    pub fn store(&mut self, ptr: Value, value: Value, ctx: &TyCtx) {
        let _ = ptr.get_type(ctx).into_inner(); // Asserts ptr is really a pointer
        self.instrs.push(Instr::Store { ptr, value })
    }

    pub fn get_params(&self) -> Vec<CfgVarUse> {
        self.params.iter().map(|(x, _)| x.into()).collect()
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
            globals: HashMap::new(),
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

    pub fn add_native_alias(&mut self, name: String, f: FunNameUse) {
        self.natives.insert(name, f);
    }

    pub fn dump_aliases_in_prog(&self, prog: &mut Program) {
        for (x, y) in &self.natives {
            prog.add_native_alias(x.clone(), y.clone());
        }
    }
}

impl Expr {
    fn check_arith(ctx: &TyCtx, lhs: &Value, rhs: &Value, str: &str) {
        if !lhs.get_type(ctx).is_arith() || !rhs.get_type(ctx).is_arith() {
            panic!("Cannot {str} non arith")
        }
    }

    pub fn add(ctx: &TyCtx, lhs: Value, rhs: Value) -> Self {
        Self::check_arith(ctx, &lhs, &rhs, "add");
        Self::Add(lhs, rhs)
    }

    pub fn mul(ctx: &TyCtx, lhs: Value, rhs: Value) -> Self {
        Self::check_arith(ctx, &lhs, &rhs, "mul");
        Self::Mul(lhs, rhs)
    }

    pub fn sub(ctx: &TyCtx, lhs: Value, rhs: Value) -> Self {
        Self::check_arith(ctx, &lhs, &rhs, "sub");
        Self::Sub(lhs, rhs)
    }

    pub fn div(ctx: &TyCtx, lhs: Value, rhs: Value) -> Self {
        Self::check_arith(ctx, &lhs, &rhs, "div");
        Self::Div(lhs, rhs)
    }

    pub fn call(ctx: &TyCtx, closure: Value, arg: Value) -> Self {
        let ty = closure.get_type(ctx);
        if !ty.repr_closure() {
            panic!("Cannot use call on non-closure")
        }
        // We assume the closure env is always passed as first arg
        if ty.param_count() != 2 {
            panic!("Closure expects 2 arguments")
        }

        let expected_ty = ty.param(1);

        if expected_ty != arg.get_type(ctx) {
            panic!("Closure expects argument of type {expected_ty}")
        }

        Self::Call { closure, arg }
    }

    pub fn native_call<S: ToString>(ctx: &TyCtx, fun: S, args: Vec<Value>) -> Self {
        let name = fun.to_string();
        if !ctx.natives.contains_key(&name) {
            panic!("Native function {name} not found")
        }

        let fname = &ctx.natives[&name];
        let params = &ctx.sigs[fname].params;

        if params.len() != args.len() {
            panic!("Native function {name} expects {} arguments", params.len())
        }

        for (i, (arg, expected_ty)) in args.iter().zip(params).enumerate() {
            let got_ty = arg.get_type(ctx);
            if got_ty != *expected_ty {
                panic!(
                    "Native function {name} expects argument {i} of type {expected_ty} but got {got_ty}"
                )
            }
        }

        Self::NativeCall { fun: name, args }
    }

    pub fn get_element_ptr(ctx: &TyCtx, ptr: Value, index: usize) -> Self {
        let ty = ptr.get_type(ctx);
        if !ty.is_ptr() {
            panic!("Cannot use getelementptr on non-pointer type")
        }
        let _ = ty.into_inner().field(index);

        Self::GetElementPtr { ptr, index }
    }

    pub fn extract(ctx: &TyCtx, value: Value, index: usize) -> Self {
        let _ = value.get_type(ctx).field(index);
        Self::Extract { value, index }
    }

    pub fn load(ctx: &TyCtx, ptr: Value, ty: Ty) -> Self {
        if !ptr.get_type(ctx).is_ptr() {
            panic!("Cannot load from non-pointer type")
        }

        Self::Load { ptr, ty }
    }

    pub fn aggregate(values: Vec<Value>) -> Self {
        Self::Struct(values)
    }
}
