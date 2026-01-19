use crate::ast::Var;
use crate::backend::Backend;
use crate::cfg::expr::Expr;
use crate::cfg::var::CfgVarUse;
use crate::cfg::{
    BasicBlock, Cfg, FunNameUse, Func, Instr, LabelUse, Program, Terminator, Ty, Value,
};
use crate::helpers::unique::Use;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{CodeModel, FileType, RelocMode, Target, TargetMachine};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use inkwell::{AddressSpace, OptimizationLevel};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Termination};

struct LLVMBackendImpl<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub funs: HashMap<FunNameUse, FunctionValue<'ctx>>,
    pub vals: HashMap<CfgVarUse, AnyValueEnum<'ctx>>,
}

type LLBB<'a> = inkwell::basic_block::BasicBlock<'a>;

impl<'ctx> LLVMBackendImpl<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("monocaml");
        let builder = context.create_builder();

        LLVMBackendImpl {
            context,
            module,
            builder,
            funs: HashMap::new(),
            vals: HashMap::new(),
        }
    }
}

impl<'ctx> LLVMBackendImpl<'ctx> {
    pub fn link_runtime(&mut self) {
        Command::new("clang")
            .arg("runtime/runtime.c")
            .arg("-S")
            .arg("-emit-llvm")
            .arg("-o")
            .arg("./runtime.llvm")
            .output()
            .map_err(|_| format!("Failed to compile runtime library"))
            .unwrap();

        let mem = MemoryBuffer::create_from_file(&PathBuf::from("runtime.llvm")).map_err(|x| {
            let _ = std::fs::remove_file(&PathBuf::from("./runtime.llvm"));
            x
        });
        let _ = std::fs::remove_file(&PathBuf::from("./runtime.llvm"));

        let runtime_module = self.context.create_module_from_ir(mem.unwrap()).unwrap();

        self.module.link_in_module(runtime_module).unwrap();
    }

    pub fn get_type(&self, ty: &Ty) -> AnyTypeEnum<'ctx> {
        let make_type = |x| {
            let t = self.get_type(x);
            if x.is_zero_sized() {
                None
            } else {
                Some(
                    BasicTypeEnum::try_from(t)
                        .unwrap_or_else(|()| self.context.ptr_type(AddressSpace::default()).into()),
                )
            }
        };
        match ty {
            Ty::Int => self.context.i32_type().into(),
            Ty::Ptr(_) | Ty::String => self.context.ptr_type(AddressSpace::default()).into(),
            Ty::Void => self.context.void_type().into(),
            Ty::Struct(items) => self
                .context
                .struct_type(
                    &items
                        .iter()
                        .map(make_type)
                        .flatten()
                        .collect::<Vec<BasicTypeEnum>>(),
                    false,
                )
                .into(),
            Ty::Union(_) => self
                .context
                .i8_type()
                .array_type(ty.get_size() as u32)
                .into(),
            Ty::FunPtr(sig) => {
                let params = sig
                    .params()
                    .iter()
                    .map(make_type)
                    .flatten()
                    .map(BasicMetadataTypeEnum::from)
                    .collect::<Vec<_>>();
                if sig.ret().is_zero_sized() {
                    self.context.void_type().fn_type(&params, false)
                } else {
                    make_type(sig.ret()).unwrap().fn_type(&params, false)
                }
                .into()
            }
        }
    }

    fn declare_function(&mut self, f: &Func, alias: Option<String>) {
        let fun_type = self.get_type(&Ty::FunPtr(f.sig())).into_function_type();

        let f_val = self.module.add_function(
            &alias.unwrap_or(format!("{}", f.name())),
            fun_type,
            Some(Linkage::External),
        );

        self.funs.insert(f.name(), f_val);
    }

    fn create_entry(&mut self, entry: FunNameUse) {
        let start_ty = self.context.void_type().fn_type(&[], false);
        let f = self
            .module
            .add_function("start", start_ty, Some(Linkage::External));

        let entry_bb = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(entry_bb);
        self.builder.build_call(self.funs[&entry], &[], "").unwrap();
        self.builder.build_return(None).unwrap();
    }

    fn build_expr(&mut self, expr: &Expr) -> AnyValueEnum<'ctx> {
        match expr {
            Expr::Value(value) => self.build_value(value).into(),
            Expr::Add(value, value1) => todo!(),
            Expr::Mul(value, value1) => todo!(),
            Expr::Sub(value, value1) => todo!(),
            Expr::Div(value, value1) => todo!(),
            Expr::Eq(value, value1) => todo!(),
            Expr::NativeCall { fun, args, s } => {
                let sig_ty = self.get_type(&Ty::FunPtr(s.clone())).into_function_type();
                let f_val = self.build_value(fun);
                let f_val = self.into_basic(f_val);
                let args_vals = args
                    .iter()
                    .map(|x| {
                        let v = self.build_value(x);
                        self.into_basic(v).into()
                    })
                    .collect::<Vec<_>>();
                self.builder
                    .build_indirect_call(sig_ty, f_val.into_pointer_value(), &args_vals, "")
                    .unwrap();
                todo!()
            }
            Expr::GetElementPtr { ptr, ty, index } => todo!(),
            Expr::Extract { value, index } => todo!(),
            Expr::Load { ptr, ty } => todo!(),
            Expr::Struct(values) => todo!(),
            Expr::Union(ty, value, _) => todo!(),
            Expr::Malloc(ty, value) => todo!(),
            Expr::Cast(ty, value) => todo!(),
            Expr::Alloca(ty) => todo!(),
        }
    }

    fn into_basic(&mut self, val: AnyValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        if val.is_function_value() {
            let f_val = val.into_function_value();
            f_val.as_global_value().as_pointer_value().into()
        } else {
            BasicValueEnum::try_from(val).unwrap()
        }
    }

    fn build_instr(&mut self, instr: &Instr<CfgVarUse>) {
        match instr {
            Instr::Assign(var, expr) => {
                let e = self.build_expr(expr);
                self.vals.insert(var.clone(), e);
            }
            Instr::Store { ptr, value } => {
                let ptr_val = self.build_value(ptr);
                let val = self.build_value(value);
                let basic_val = self.into_basic(val);
                self.builder
                    .build_store(ptr_val.into_pointer_value(), basic_val)
                    .unwrap();
            }
        }
    }

    fn build_value(&mut self, v: &Value) -> AnyValueEnum<'ctx> {
        todo!()
    }

    fn build_terminator(&mut self, t: &Terminator, bbs: &HashMap<LabelUse, LLBB<'ctx>>) {
        match t {
            Terminator::Return(Some(value)) => {
                let v = self.build_value(value);
                let bv = self.into_basic(v);
                self.builder.build_return(Some(&bv)).unwrap();
            }
            Terminator::Return(None) => {
                self.builder.build_return(None).unwrap();
            }
            Terminator::Goto(lbl) => {
                let dest = bbs[lbl];
                self.builder.build_unconditional_branch(dest).unwrap();
            }
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                let v = self.build_value(cond);
                let then_block = bbs[then_bb];
                let else_block = bbs[else_bb];
                self.builder
                    .build_conditional_branch(v.into_int_value(), then_block, else_block)
                    .unwrap();
            }
        }
    }

    fn build_block(&mut self, b: &BasicBlock, bbs: &HashMap<LabelUse, LLBB<'ctx>>) {
        for instr in b.instrs() {
            self.build_instr(instr);
        }
        self.build_terminator(b.terminator(), bbs);
    }

    fn build_cfg(&mut self, f: FunctionValue<'ctx>, cfg: &Cfg, params: Vec<(CfgVarUse, Ty)>) {
        for (i, (param, _)) in params.iter().enumerate() {
            let param_val = f.get_nth_param(i as u32).unwrap();
            self.vals.insert(param.clone(), param_val.into());
        }

        let mut bbs = HashMap::new();

        bbs.insert(
            cfg.entry().clone(),
            self.context
                .append_basic_block(f, &format!("{}", cfg.entry())),
        );

        for bb in cfg.blocks() {
            if bb.label() == *cfg.entry() {
                continue;
            }
            let ll_bb = self
                .context
                .append_basic_block(f, &format!("{}", bb.label()));
            bbs.insert(bb.label(), ll_bb);
        }

        for bb in cfg.blocks() {
            self.build_block(bb, &bbs);
        }
    }

    fn build_function(&mut self, f: &Func) {
        let f_val = self.funs[&f.name()];
        self.build_cfg(
            f_val,
            f.cfg().as_ref().unwrap(),
            f.params()
                .iter()
                .map(|(x, y)| (Use::from(x), y.clone()))
                .collect(),
        );
    }

    pub fn compile(&mut self, prog: &Program) -> Result<(), ()> {
        self.link_runtime();
        // println!("{}", self.module.print_to_string().to_string());
        let rev_map = prog
            .natives()
            .iter()
            .map(|(alias, true_name)| (true_name.clone(), alias.clone()))
            .collect::<HashMap<_, _>>();

        for f in prog.funcs() {
            self.declare_function(f, rev_map.get(&f.name()).cloned());
        }
        self.create_entry(prog.entry());

        for f in prog.funcs() {
            if f.cfg().is_none() {
                continue;
            }
            self.build_function(f);
        }

        todo!()
    }
}

pub struct LLVMBackend {
    output_path: PathBuf,
}

impl Backend for LLVMBackend {
    type Out = Context;
    type Err = ();

    fn compile(self, program: &Program) -> Result<Context, ()> {
        let context = Context::create();
        {
            let mut backend = LLVMBackendImpl::new(&context);
            backend.compile(program)?;
            let triple = TargetMachine::get_default_triple();
            let target = Target::from_triple(&triple).unwrap();
            let target_machine = target
                .create_target_machine(
                    &triple,
                    "generic", // CPU
                    "",        // Features
                    OptimizationLevel::Aggressive,
                    RelocMode::Static,
                    CodeModel::Default,
                )
                .expect("Failed to create target machine");
            target_machine
                .write_to_file(&backend.module, FileType::Object, &self.output_path)
                .unwrap();
        }

        Ok(context)
    }
}

impl LLVMBackend {
    pub fn new<P: AsRef<Path>>(output_path: P) -> Self {
        Self {
            output_path: output_path.as_ref().to_path_buf(),
        }
    }
}
