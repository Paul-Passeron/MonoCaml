use crate::backend::Backend;
use crate::cfg::expr::Expr;
use crate::cfg::var::CfgVarUse;
use crate::cfg::{
    BasicBlock, Const, FunNameUse, Func, Instr, LabelUse, Program, Terminator, Ty, Value,
};
use crate::helpers::unique::Use;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::env::temp_dir;

struct LLVMBackendImpl<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub _pb: PassManagerBuilder,
    pub fpm: PassManager<FunctionValue<'ctx>>,
    pub mam: PassManager<Module<'ctx>>,

    pub funs: HashMap<FunNameUse, FunctionValue<'ctx>>,
    pub vals: HashMap<CfgVarUse, AnyValueEnum<'ctx>>,

    // C types
    pub type_names: HashMap<Ty, String>,
    pub decayed_type_names: HashMap<Ty, String>,
    pub count: usize,
}

type LLBB<'a> = inkwell::basic_block::BasicBlock<'a>;

impl<'ctx> LLVMBackendImpl<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("monocaml");
        let builder = context.create_builder();
        let fpm = PassManager::create(&module);
        let mam = PassManager::create(());

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_demote_memory_to_register_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_sccp_pass();
        fpm.initialize();

        mam.add_aggressive_dce_pass();
        mam.add_merge_functions_pass();
        mam.add_dead_arg_elimination_pass();
        mam.add_function_inlining_pass();
        mam.add_cfg_simplification_pass();

        let pb = PassManagerBuilder::create();
        pb.populate_function_pass_manager(&fpm);
        pb.populate_module_pass_manager(&mam);
        pb.set_optimization_level(OptimizationLevel::Aggressive);

        LLVMBackendImpl {
            context,
            module,
            builder,
            fpm,
            mam,
            _pb: pb,

            funs: HashMap::new(),
            vals: HashMap::new(),

            type_names: HashMap::new(),
            decayed_type_names: HashMap::new(),
            count: 0,
        }
    }
}

impl<'ctx> LLVMBackendImpl<'ctx> {
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
            Ty::Union(_) => {
                let align = ty.get_align();
                let size = ty.get_size();
                let elem_ty = match align {
                    8 => self.context.i64_type(),
                    4 => self.context.i32_type(),
                    2 => self.context.i16_type(),
                    _ => self.context.i8_type(),
                };
                let elem_size = align;
                let count = (size + elem_size - 1) / elem_size;
                elem_ty.array_type(count as u32).into()
            }
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

        let name = alias.unwrap_or(format!("{}", f.name()));
        let f_val = self.module.get_function(&name).unwrap_or_else(|| {
            self.module
                .add_function(&name, fun_type, Some(Linkage::External))
        });

        self.funs.insert(f.name(), f_val);
    }

    fn get_cfg_ty_of_value(&mut self, v: &Value, func: &Func) -> Ty {
        match v {
            Value::Var(v) => {
                if let Some(ty) = func
                    .cfg()
                    .as_ref()
                    .unwrap()
                    .locals()
                    .iter()
                    .map(|(a, b)| (Use::from(a), b.clone()))
                    .find_map(|(a, b)| if a == *v { Some(b) } else { None })
                {
                    ty
                } else {
                    func.params()
                        .iter()
                        .map(|(a, b)| (Use::from(a), b.clone()))
                        .find_map(|(a, b)| if a == *v { Some(b) } else { None })
                        .unwrap()
                }
            }
            Value::Const(_) => todo!(),
        }
    }

    fn build_expr(&mut self, expr: &Expr, func: &Func) -> AnyValueEnum<'ctx> {
        match expr {
            Expr::Value(value) => self.build_value(value).into(),
            Expr::Add(lhs, rhs) => {
                let v1 = self.build_value(lhs).into_int_value();
                let v2 = self.build_value(rhs).into_int_value();
                self.builder.build_int_add(v1, v2, "").unwrap().into()
            }
            Expr::Mul(lhs, rhs) => {
                let v1 = self.build_value(lhs).into_int_value();
                let v2 = self.build_value(rhs).into_int_value();
                self.builder.build_int_mul(v1, v2, "").unwrap().into()
            }
            Expr::Sub(lhs, rhs) => {
                let v1 = self.build_value(lhs).into_int_value();
                let v2 = self.build_value(rhs).into_int_value();
                self.builder.build_int_sub(v1, v2, "").unwrap().into()
            }
            Expr::Div(lhs, rhs) => {
                let v1 = self.build_value(lhs).into_int_value();
                let v2 = self.build_value(rhs).into_int_value();
                self.builder
                    .build_int_signed_div(v1, v2, "")
                    .unwrap()
                    .into()
            }
            Expr::Eq(lhs, rhs) => {
                let v1 = self.build_value(lhs).into_int_value();
                let v2 = self.build_value(rhs).into_int_value();
                let bool_val = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::EQ, v1, v2, "")
                    .unwrap();

                let i32_ty = self.context.i32_type();
                self.builder
                    .build_int_cast(bool_val, i32_ty, "")
                    .unwrap()
                    .into()
            }
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
                    .unwrap()
                    .as_any_value_enum()
            }
            Expr::GetElementPtr { ptr, ty, index } => match ty {
                Ty::Struct(_) => self.build_gep_struct(&ty, ptr, *index).into(),
                Ty::Union(_) => self.build_gep_union(ptr).into(),
                _ => unreachable!(),
            },
            Expr::Extract { value, index } => {
                let val_ty = self.get_cfg_ty_of_value(value, func);
                match val_ty {
                    Ty::Struct(_) => self.build_extract_struct(value, *index),
                    Ty::Union(items) => self.build_extract_union(value, &items, *index),
                    _ => unreachable!(),
                }
            }
            Expr::Load { ptr, ty } => {
                let ty = self.get_type(ty);
                let bty = self.into_basic_ty(ty);
                let ptr_val = self.build_value(ptr).into_pointer_value();
                self.builder.build_load(bty, ptr_val, "").unwrap().into()
            }
            Expr::Struct(values) => {
                let vs = values
                    .iter()
                    .map(|x| {
                        let any = self.build_value(x);
                        self.into_basic(any)
                    })
                    .collect::<Vec<_>>();
                let vts = vs.iter().map(|x| x.get_type()).collect::<Vec<_>>();

                let ty = self.context.struct_type(&vts, false);
                let mut struct_val = ty.get_undef();
                for (i, v) in vs.into_iter().enumerate() {
                    struct_val = self
                        .builder
                        .build_insert_value(struct_val, v, i as u32, "")
                        .unwrap()
                        .into_struct_value();
                }
                struct_val.into()
            }
            Expr::Union(ty, value, _) => {
                let ty = self.get_type(ty).into_array_type();
                let union_ptr = self.builder.build_alloca(ty, "").unwrap();
                let zeroed = ty.const_zero();
                self.builder.build_store(union_ptr, zeroed).unwrap();
                let val = self.build_value(value);
                let bv = self.into_basic(val);
                self.builder.build_store(union_ptr, bv).unwrap();
                self.builder.build_load(ty, union_ptr, "").unwrap().into()
            }
            Expr::Malloc(ty, value) => {
                let llty = self.get_type(ty);
                let bty = self.into_basic_ty(llty);
                let ptr = self.builder.build_malloc(bty, "").unwrap();

                let val = self.build_value(value);
                let bval = self.into_basic(val);
                self.builder.build_store(ptr, bval).unwrap();
                ptr.into()
            }
            Expr::Cast(ty, value) => {
                let ty = self.get_type(ty);
                let bty = self.into_basic_ty(ty);
                let v = self.build_value(value);
                let bv = self.into_basic(v);
                self.builder.build_bit_cast(bv, bty, "").unwrap().into()
            }
            Expr::Alloca(ty) => {
                let llty = self.get_type(ty);
                let bty = self.into_basic_ty(llty);
                self.builder.build_alloca(bty, "").unwrap().into()
            }
        }
    }

    fn build_gep_struct(&mut self, ty: &Ty, value: &Value, index: usize) -> PointerValue<'ctx> {
        let val = self.build_value(value).into_pointer_value();
        let llty = self.get_type(ty);
        let bty = self.into_basic_ty(llty);
        self.builder
            .build_struct_gep(bty, val, index as u32, "")
            .unwrap()
    }

    fn build_gep_union(&mut self, value: &Value) -> PointerValue<'ctx> {
        self.build_value(value).into_pointer_value()
    }

    fn build_extract_struct(&mut self, value: &Value, index: usize) -> AnyValueEnum<'ctx> {
        let val = self.build_value(value).into_struct_value();
        self.builder
            .build_extract_value(val, index as u32, "")
            .unwrap()
            .into()
    }

    fn build_extract_union(
        &mut self,
        value: &Value,
        items: &[Ty],
        index: usize,
    ) -> AnyValueEnum<'ctx> {
        let arr_val = self.build_value(value).into_array_value();
        let actual_ty = self.get_type(&items[index]);
        let bty = self.into_basic_ty(actual_ty);
        let allocated = self.builder.build_alloca(arr_val.get_type(), "").unwrap();
        self.builder.build_store(allocated, arr_val).unwrap();

        self.builder.build_load(bty, allocated, "").unwrap().into()
    }

    fn into_basic_ty(&mut self, val: AnyTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
        if val.is_function_type() {
            self.context.ptr_type(AddressSpace::default()).into()
        } else {
            BasicTypeEnum::try_from(val).unwrap()
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

    fn build_instr(&mut self, instr: &Instr<CfgVarUse>, func: &Func) {
        match instr {
            Instr::Assign(var, expr) => {
                let e = self.build_expr(expr, func);
                if !e.get_type().is_void_type() {
                    self.vals.insert(var.clone(), e);
                }
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

    fn build_const(&mut self, c: &Const) -> AnyValueEnum<'ctx> {
        match c {
            Const::Int(x) => self.context.i32_type().const_int(*x as u64, true).into(),
            Const::String(s) => self
                .builder
                .build_global_string_ptr(s, "")
                .unwrap()
                .as_any_value_enum(),
            Const::Struct(items) => {
                let elems = items
                    .iter()
                    .map(|x| {
                        let any = self.build_const(x);
                        self.into_basic(any)
                    })
                    .collect::<Vec<_>>();
                let elems_tys = elems.iter().map(|x| x.get_type()).collect::<Vec<_>>();
                let ty = self.context.struct_type(&elems_tys, false);
                let mut res = ty.get_undef();
                for (i, elem) in elems.into_iter().enumerate() {
                    res = self
                        .builder
                        .build_insert_value(res, elem, i as u32, "")
                        .unwrap()
                        .into_struct_value();
                }
                res.into()
            }
            Const::FunPtr(f) => self.funs[f].into(),
            Const::NullPtr => self
                .context
                .ptr_type(AddressSpace::default())
                .const_null()
                .into(),
        }
    }

    fn build_value(&mut self, v: &Value) -> AnyValueEnum<'ctx> {
        match v {
            Value::Var(v) => self.vals[v],
            Value::Const(c) => self.build_const(c),
        }
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
                let v = self.build_value(cond).into_int_value();
                let v_ty = v.get_type();
                let cond = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::NE, v, v_ty.const_zero().into(), "")
                    .unwrap();

                let then_block = bbs[then_bb];
                let else_block = bbs[else_bb];
                self.builder
                    .build_conditional_branch(cond, then_block, else_block)
                    .unwrap();
            }
        }
    }

    fn build_block(&mut self, b: &BasicBlock, bbs: &HashMap<LabelUse, LLBB<'ctx>>, func: &Func) {
        self.builder.position_at_end(bbs[&b.label()]);
        for instr in b.instrs() {
            self.build_instr(instr, func);
        }
        self.build_terminator(b.terminator(), bbs);
    }

    fn build_cfg(&mut self, f: FunctionValue<'ctx>, func: &Func) {
        let cfg = func.cfg().as_ref().unwrap();
        let params = func
            .params()
            .iter()
            .map(|(param, ty)| (Use::from(param), ty.clone()))
            .collect::<Vec<_>>();

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
            self.build_block(bb, &bbs, func);
        }
    }

    fn build_function(&mut self, f: &Func) {
        let f_val = self.funs[&f.name()];
        self.build_cfg(f_val, f);
        assert!(f_val.verify(true));
        println!("Function built: {}", f.name());
        self.fpm.run_on(&f_val);
    }

    fn canonical_decayed(&self, ty: &Ty) -> String {
        match ty {
            Ty::Int => "int".to_string(),
            Ty::String => "const char *".to_string(),
            Ty::Void => "void".to_string(),
            Ty::Struct(items) if items.is_empty() => "void".to_string(),
            Ty::Ptr(_) => "void*".to_string(),
            Ty::FunPtr(sig) => {
                let ret = self.canonical_decayed(sig.ret());
                let params: Vec<_> = sig
                    .params()
                    .iter()
                    .map(|p| self.canonical_decayed(p))
                    .collect();
                let params_str = if params.is_empty() {
                    "void".to_string()
                } else {
                    params.join(", ")
                };
                format!("{} (*)({})", ret, params_str)
            }
            Ty::Struct(items) => {
                let fields: Vec<_> = items
                    .iter()
                    .map(|item| self.canonical_decayed(item))
                    .collect();
                format!("{{{}}}", fields.join(", "))
            }
            Ty::Union(items) => {
                let fields: Vec<_> = items
                    .iter()
                    .map(|item| self.canonical_decayed(item))
                    .collect();
                format!("union {{{}}}", fields.join(", "))
            }
        }
    }

    fn get_type_name(&self, ty: &Ty) -> String {
        let ty = match ty {
            Ty::Struct(items) if items.is_empty() => &Ty::Void,
            _ => ty,
        };

        if let Some(name) = self.type_names.get(ty) {
            return name.clone();
        }

        match ty {
            Ty::Int => "int".to_string(),
            Ty::String => "const char *".to_string(),
            Ty::Void => "void".to_string(),
            Ty::FunPtr(_) | Ty::Ptr(_) => self.canonical_decayed(ty),
            _ => "".into(),
        }
    }

    fn get_decayed_type_name(&self, ty: &Ty) -> String {
        let ty = match ty {
            Ty::Struct(items) if items.is_empty() => &Ty::Void,
            _ => ty,
        };

        if let Some(name) = self.decayed_type_names.get(ty) {
            return name.clone();
        }

        match ty {
            Ty::Int => "int".to_string(),
            Ty::String => "const char *".to_string(),
            Ty::Void => "void".to_string(),
            Ty::FunPtr(_) | Ty::Ptr(_) => "void*".to_string(),
            _ => panic!("No type but there should be !"),
        }
    }

    fn define_c_type(&mut self, ty: Ty, f: &mut dyn Write) {
        self.define_c_type_pro(ty, None, f)
    }

    fn define_c_type_pro(&mut self, ty: Ty, name: Option<&String>, f: &mut dyn Write) {
        // Normalize empty struct to void
        let ty = match &ty {
            Ty::Struct(items) if items.is_empty() => Ty::Void,
            _ => ty,
        };

        // Already defined?
        if self.type_names.contains_key(&ty) {
            return;
        }

        match &ty {
            Ty::Int => {
                assert!(name.is_none());
                self.type_names.insert(ty.clone(), "int".to_string());
                self.decayed_type_names.insert(ty, "int".to_string());
            }
            Ty::String => {
                assert!(name.is_none());
                self.type_names
                    .insert(ty.clone(), "const char *".to_string());
                self.decayed_type_names
                    .insert(ty, "const char *".to_string());
            }
            Ty::Void => {
                assert!(name.is_none());

                self.type_names.insert(ty.clone(), "void".to_string());
                self.decayed_type_names.insert(ty, "void".to_string());
            }
            Ty::Ptr(inner) => {
                assert!(name.is_none());

                self.define_c_type((**inner).clone(), f);
                let inner_name = self.get_type_name(inner);
                self.type_names
                    .insert(ty.clone(), format!("{}*", inner_name));
                self.decayed_type_names.insert(ty, "void*".to_string());
            }
            Ty::FunPtr(_) => {
                self.type_names.insert(ty.clone(), "void*".to_string());
                self.decayed_type_names.insert(ty, "void*".to_string());
            }
            Ty::Struct(items) => {
                // Define all inner types first
                for item in items {
                    self.define_c_type(item.clone(), f);
                }

                // Compute canonical decayed representation for equivalence
                let my_canonical = self.canonical_decayed(&ty);

                // Check for existing equivalent struct
                let existing = self
                    .type_names
                    .iter()
                    .filter_map(|(existing_ty, existing_name)| {
                        if let Ty::Struct(_) = existing_ty {
                            if self.canonical_decayed(existing_ty) == my_canonical {
                                match self.decayed_type_names.get(existing_ty) {
                                    Some(decayed_name) => {
                                        return Some((existing_name.clone(), decayed_name.clone()));
                                    }
                                    None => {
                                        return Some((existing_name.clone(), my_canonical.clone()));
                                    }
                                }
                            }
                        }
                        None
                    })
                    .next();

                if let Some((existing_name, decayed_name)) = existing {
                    self.type_names.insert(ty.clone(), existing_name);
                    self.decayed_type_names.insert(ty, decayed_name);
                    return;
                }

                // Create new typedef using decayed field types
                let name = name.cloned().unwrap_or(format!("ty_{}", self.count));
                self.count += 1;

                let decayed_field_types: Vec<_> = items
                    .iter()
                    .map(|item| self.get_decayed_type_name(item))
                    .collect();

                writeln!(f, "typedef struct {{").unwrap();
                for (i, field_ty) in decayed_field_types.iter().enumerate() {
                    writeln!(f, "    {} _{};", field_ty, i).unwrap();
                }
                writeln!(f, "}} {};\n", name).unwrap();
                self.type_names.insert(ty.clone(), name.clone());
                self.decayed_type_names.insert(ty, name);
            }
            Ty::Union(items) => {
                // Define all inner types first
                for item in items {
                    self.define_c_type(item.clone(), f);
                }

                // Compute canonical decayed representation for equivalence
                let my_canonical = self.canonical_decayed(&ty);

                // Check for existing equivalent struct
                let existing = self
                    .type_names
                    .iter()
                    .filter_map(|(existing_ty, existing_name)| {
                        if let Ty::Union(_) = existing_ty {
                            if self.canonical_decayed(existing_ty) == my_canonical {
                                match self.decayed_type_names.get(existing_ty) {
                                    Some(decayed_name) => {
                                        return Some((existing_name.clone(), decayed_name.clone()));
                                    }
                                    None => {
                                        return Some((existing_name.clone(), my_canonical.clone()));
                                    }
                                }
                            }
                        }
                        None
                    })
                    .next();

                if let Some((existing_name, decayed_name)) = existing {
                    self.type_names.insert(ty.clone(), existing_name);
                    self.decayed_type_names.insert(ty, decayed_name);
                    return;
                }

                // Create new typedef using decayed field types
                let name = format!("ty_{}", self.count);
                self.count += 1;

                let decayed_field_types: Vec<_> = items
                    .iter()
                    .map(|item| self.get_decayed_type_name(item))
                    .collect();

                writeln!(f, "typedef union {{").unwrap();
                for (i, field_ty) in decayed_field_types.iter().enumerate() {
                    writeln!(f, "    {} _{};", field_ty, i).unwrap();
                }
                writeln!(f, "}} {};\n", name).unwrap();

                self.type_names.insert(ty.clone(), name.clone());
                self.decayed_type_names.insert(ty, name);
            }
        }
    }

    fn create_pool_allocators(&mut self, boxed_types: &HashMap<String, Ty>) {
        let current_dir = std::env::current_dir().unwrap().join("runtime");
        let dir = temp_dir();
        let p = dir.as_path().join("temp.c");
        let mut f = File::create(&p).unwrap();

        writeln!(
            f,
            "#include \"{}\"",
            current_dir.join("runtime.c").display()
        )
        .unwrap();

        writeln!(
            f,
            "#pragma clang diagnostic ignored \"-Wincompatible-pointer-types\""
        )
        .unwrap();

        for (name, ty) in boxed_types {
            self.define_c_type_pro(ty.into_inner().clone(), Some(name), &mut f);
        }

        for (name, _) in boxed_types {
            writeln!(f, "setup_pool({name})").unwrap();
        }

        println!("p is {}", p.display());

        let output = Command::new("clang-16")
            .arg(p)
            .arg("-S")
            .arg("-emit-llvm")
            .arg("-I/home/paulpasseron/monocaml_playground/runtime")
            .arg("-o")
            .arg("./runtime.llvm")
            .output()
            .map_err(|_| format!("Failed to compile runtime library"))
            .unwrap();

        println!("{}", String::from_utf8(output.stderr).unwrap());

        let mem = MemoryBuffer::create_from_file(&PathBuf::from("runtime.llvm")).map_err(|x| {
            let _ = std::fs::remove_file(&PathBuf::from("./runtime.llvm"));
            x
        });
        let _ = std::fs::remove_file(&PathBuf::from("./runtime.llvm"));

        let runtime_module = self.context.create_module_from_ir(mem.unwrap()).unwrap();

        self.module.link_in_module(runtime_module).unwrap();
    }

    pub fn compile(&mut self, prog: &Program) -> Result<(), ()> {
        self.create_pool_allocators(prog.boxed_types());

        let mut rev_map = prog
            .natives()
            .iter()
            .map(|(alias, true_name)| (true_name.clone(), alias.clone()))
            .collect::<HashMap<_, _>>();

        rev_map.insert(prog.entry(), format!("start"));

        for f in prog.funcs() {
            if f.cfg().is_none() {
                self.module
                    .get_function(&rev_map.get(&f.name()).cloned().unwrap())
                    .map(|name| self.funs.insert(Use::from(f.name()), name));
            } else {
                self.declare_function(f, rev_map.get(&f.name()).cloned());
            }
        }

        for f in prog.funcs() {
            if f.cfg().is_none() {
                continue;
            }
            self.build_function(f);
        }

        Ok(())
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
            Target::initialize_all(&InitializationConfig::default());
            let mut backend = LLVMBackendImpl::new(&context);
            backend.compile(program)?;
            backend
                .module
                .verify()
                .map_err(|x| println!("{}", x.to_str().unwrap()))
                .unwrap();
            backend.mam.run_on(&backend.module);
            println!("{}", backend.module.to_string());
            let triple = TargetMachine::get_default_triple();
            let target = Target::from_triple(&triple).unwrap();
            let target_machine = target
                .create_target_machine(
                    &triple,
                    "generic", // CPU
                    "",        // Features
                    OptimizationLevel::Aggressive,
                    RelocMode::PIC,
                    CodeModel::Default,
                )
                .expect("Failed to create target machine");
            let dir = temp_dir();
            let p = dir.as_path().join("temp.o");
            let _ = File::create(&p).unwrap();
            target_machine
                .write_to_file(&backend.module, FileType::Object, &p)
                .unwrap();
            let _ = Command::new("cc")
                .arg(p)
                .arg("-o")
                .arg(self.output_path)
                .output()
                .map_err(|_| format!("Failed to compile runtime library"))
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
