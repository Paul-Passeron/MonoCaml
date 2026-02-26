use std::iter::{empty, once};

use crate::{
    cfg::{Const, FunName, Func, Label, Ty, Value, builder::Builder, var::CfgVar},
    helpers::unique::Use,
    lower::mono_to_cfg_old::MonoToCfg,
    mono_ir::types::{AstTy, EnumDef},
};

impl MonoToCfg {
    // Very simple: we simply retain the current pointer and do not care about
    // constructor args because their dropping is guarded by the dropping of this
    // object
    fn create_borrow_for_rec_enum(
        &mut self,
        name: &String,
        param: Use<CfgVar>,
        builder: &mut Builder,
    ) {
        let borrow_object = self.prog.natives()[&format!("{name}_retain")].clone();
        builder.native_call(
            &mut self.ctx,
            Const::FunPtr(borrow_object).into(),
            vec![param.into()],
        );
        builder.ret_void(&self.ctx);
    }

    fn create_borrow_for_nonrec_enum(
        &mut self,
        param: Use<CfgVar>,
        enum_def: &EnumDef,
        builder: &mut Builder,
    ) {
        let discr = builder.extract(&mut self.ctx, param.clone().into(), 0);
        let ret_lbl = Label::fresh();
        let ret_use = Use::from(&ret_lbl);

        for (i, case) in enum_def.cases.iter().enumerate() {
            let is_i = builder.eq(
                &mut self.ctx,
                discr.clone().into(),
                Const::Int(i as i32).into(),
            );
            let case_body_lbl = Label::fresh();
            let case_body_use = Use::from(&case_body_lbl);
            let next_check_lbl = Label::fresh();
            let next_check_use = Use::from(&next_check_lbl);

            builder.branch(
                &self.ctx,
                is_i,
                case_body_use,
                next_check_use.clone(),
                case_body_lbl,
            );

            if let Some(t) = &case.arg {
                let union_val = builder
                    .extract(&mut self.ctx, param.clone().into(), 1)
                    .into();
                let val = builder.extract(&mut self.ctx, union_val, i).into();
                self.borrow_ty(val, t, builder);
            }

            builder.goto(&self.ctx, ret_use.clone(), next_check_lbl);
        }
        builder.goto(&self.ctx, ret_use, ret_lbl);
        builder.ret_void(&self.ctx);
    }

    fn create_borrows_for_enum(&mut self, enum_def: &EnumDef, funname: FunName) {
        println!("Borrow for {} is {}", enum_def.name, Use::from(&funname));
        let named = AstTy::named(&enum_def.name);
        let self_ty = self.ast_ty_to_ty(&named);
        let is_rec = named.is_recursive(&self.ast_ctx);
        let funname_use = Use::from(&funname);
        let param_var = self.ctx.new_var(self_ty.clone());
        let param_use = Use::from(&param_var);
        let mut builder = Builder::new(
            funname,
            vec![(param_var, self_ty.clone())],
            Ty::Void,
            &mut self.ctx,
        );

        if is_rec {
            self.create_borrow_for_rec_enum(&enum_def.name, param_use, &mut builder);
        } else {
            self.create_borrow_for_nonrec_enum(param_use, enum_def, &mut builder);
        };

        let f = builder.finalize();
        self.add_func(f);
        self.borrows
            .entry(enum_def.name.clone())
            .or_insert(funname_use);
    }

    fn declare_borrows_for_enum(&mut self, enum_def: &EnumDef) -> FunName {
        let funname = FunName::fresh();
        self.borrows
            .insert(enum_def.name.clone(), Use::from(&funname));
        funname
    }

    fn declare_drops_for_enum(&mut self, enum_def: &EnumDef) -> FunName {
        let funname = FunName::fresh();
        self.drops
            .insert(enum_def.name.clone(), Use::from(&funname));
        funname
    }

    fn create_drops_for_non_rec_enum(
        &mut self,
        param: Use<CfgVar>,
        enum_def: &EnumDef,
        builder: &mut Builder,
    ) {
        // Release constructor fields

        let discr = builder.extract(&mut self.ctx, param.clone().into(), 0);
        let ret_lbl = Label::fresh();
        let ret_use = Use::from(&ret_lbl);

        for (i, case) in enum_def.cases.iter().enumerate() {
            let is_i = builder.eq(
                &mut self.ctx,
                discr.clone().into(),
                Const::Int(i as i32).into(),
            );
            let case_body_lbl = Label::fresh();
            let case_body_use = Use::from(&case_body_lbl);
            let next_check_lbl = Label::fresh();
            let next_check_use = Use::from(&next_check_lbl);

            builder.branch(
                &self.ctx,
                is_i,
                case_body_use,
                next_check_use.clone(),
                case_body_lbl,
            );

            if let Some(t) = &case.arg {
                let union_val = builder
                    .extract(&mut self.ctx, param.clone().into(), 1)
                    .into();
                let val = builder.extract(&mut self.ctx, union_val, i).into();
                self.drop_ty(val, t, builder);
            }
            builder.goto(&self.ctx, ret_use.clone(), next_check_lbl);
        }
        builder.goto(&self.ctx, ret_use, ret_lbl);
        builder.ret_void(&self.ctx);
    }

    fn create_drops_for_rec_enum(
        &mut self,
        name: &String,
        param: Use<CfgVar>,
        enum_def: &EnumDef,
        self_ty: Ty,
        builder: &mut Builder,
    ) {
        // Release constructor fields

        let is_unique_function =
            self.prog.natives()[&format!("{}_is_unique", enum_def.name)].clone();
        let is_unique = builder.native_call(
            &mut self.ctx,
            Const::FunPtr(is_unique_function).into(),
            vec![param.clone().into()],
        );

        let if_unique = Label::fresh();
        let ret_lbl = Label::fresh();
        let ret_use = Use::from(&ret_lbl);
        let ifu = Use::from(&if_unique);

        builder.branch(&self.ctx, is_unique.into(), ifu, ret_use.clone(), if_unique);

        let ptr =
            builder.get_element_ptr(&mut self.ctx, param.clone().into(), self_ty.into_inner(), 0);

        let discr = builder.load(&mut self.ctx, ptr.into(), Ty::Int);
        for (i, case) in enum_def.cases.iter().enumerate() {
            let is_i = builder.eq(
                &mut self.ctx,
                discr.clone().into(),
                Const::Int(i as i32).into(),
            );
            let case_body_lbl = Label::fresh();
            let case_body_use = Use::from(&case_body_lbl);
            let next_check_lbl = Label::fresh();
            let next_check_use = Use::from(&next_check_lbl);

            builder.branch(
                &self.ctx,
                is_i,
                case_body_use,
                next_check_use.clone(),
                case_body_lbl,
            );
            let union_ptr = builder.get_element_ptr(
                &mut self.ctx,
                param.clone().into(),
                self_ty.into_inner(),
                1,
            );
            if let Some(t) = &case.arg {
                let union_val = builder
                    .load(
                        &mut self.ctx,
                        union_ptr.clone().into(),
                        self_ty.into_inner().field(1),
                    )
                    .into();
                let val = builder.extract(&mut self.ctx, union_val, i).into();
                self.drop_ty(val, t, builder);
            }
            builder.goto(&self.ctx, ret_use.clone(), next_check_lbl);
        }
        builder.goto(&self.ctx, ret_use, ret_lbl);

        let drop_fun = self.prog.natives()[&format!("{name}_release")].clone();
        builder.native_call(
            &mut self.ctx,
            Const::FunPtr(drop_fun).into(),
            vec![param.clone().into()],
        );

        builder.ret_void(&self.ctx);
    }

    fn create_drops_for_enum(&mut self, enum_def: &EnumDef, funname: FunName) {
        println!("Drop for {} is {}", enum_def.name, Use::from(&funname));
        let named = AstTy::named(&enum_def.name);
        let self_ty = self.ast_ty_to_ty(&named);
        let is_rec = named.is_recursive(&self.ast_ctx);
        let funname_use = Use::from(&funname);
        let param_var = self.ctx.new_var(self_ty.clone());
        let param_use = Use::from(&param_var);
        let mut builder = Builder::new(
            funname,
            vec![(param_var, self_ty.clone())],
            Ty::Void,
            &mut self.ctx,
        );

        if is_rec {
            self.create_drops_for_rec_enum(
                &enum_def.name,
                param_use,
                enum_def,
                self_ty,
                &mut builder,
            );
        } else {
            self.create_drops_for_non_rec_enum(param_use, enum_def, &mut builder);
        }

        let f = builder.finalize();
        self.add_func(f);
        self.drops
            .entry(enum_def.name.clone())
            .or_insert(funname_use);
    }

    pub fn create_drops(&mut self) {
        self.ast_ctx
            .types
            .values()
            .cloned()
            .collect::<Vec<_>>()
            .iter()
            .map(|x| (x, self.declare_drops_for_enum(x)))
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|(x, name)| self.create_drops_for_enum(x, name));
    }

    pub fn create_borrows(&mut self) {
        self.ast_ctx
            .types
            .values()
            .cloned()
            .collect::<Vec<_>>()
            .iter()
            .map(|x| (x, self.declare_borrows_for_enum(x)))
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|(x, name)| self.create_borrows_for_enum(x, name));
    }

    pub fn create_boxed_types(&mut self) {
        self.ast_ctx.types.clone().iter().for_each(|x| {
            let ty = self.ast_ty_to_ty(&AstTy::named(x.0));
            self.prog.mut_boxed_types().insert(x.0.clone(), ty);
        });
    }

    pub fn create_constructors_for_enum(&mut self, enum_def: &EnumDef) {
        let named = AstTy::named(&enum_def.name);
        let ret_ty = self.ast_ty_to_ty(&named);
        let union_ty = if ret_ty.is_ptr() {
            ret_ty.into_inner().field(1)
        } else {
            ret_ty.field(1)
        };
        assert!(union_ty.is_union());
        let is_rec = named.is_recursive(&self.ast_ctx);
        for (i, case) in enum_def.cases.iter().enumerate() {
            let funname = FunName::fresh();
            let funname_use = Use::from(&funname);
            let params = case
                .arg
                .iter()
                .map(|x| self.ast_ty_to_ty(x))
                .collect::<Vec<_>>();
            let var_params = self.ctx.make_params(params.clone().into_iter());
            let var_params = var_params
                .into_iter()
                .filter(|x| !x.1.is_zero_sized())
                .collect::<Vec<_>>();
            let use_params = var_params
                .iter()
                .map(|(x, _)| Use::from(x))
                .collect::<Vec<_>>();
            let mut builder = Builder::new(funname, var_params, ret_ty.clone(), &mut self.ctx);

            let mut args = vec![Const::Int(i as i32).into()];
            if !use_params.is_empty() {
                let val: Value = use_params[0].clone().into();
                self.inject_print(
                    format!(
                        "BORROWING from create_constructors_for_enum, in func {}\n",
                        builder.funname()
                    ),
                    &mut builder,
                );
                self.borrow_ty(val.clone(), case.arg.as_ref().unwrap(), &mut builder);
                let union = builder.union(&mut self.ctx, union_ty.clone(), val.clone(), i);
                args.push(union.into());
            }
            let struct_val = builder.aggregate(&mut self.ctx, args);

            if is_rec {
                let allocate_obj = Const::FunPtr(
                    self.ctx.natives[&format!("{}_pool_allocate", enum_def.name)].clone(),
                );
                let m = builder.native_call(&mut self.ctx, allocate_obj.into(), vec![]);
                builder.store(&self.ctx, m.clone().into(), struct_val);
                builder.ret(&self.ctx, m.into());
            } else {
                builder.ret(&self.ctx, struct_val);
            }
            let f = builder.finalize();
            self.add_func(f);
            self.constructors
                .entry(enum_def.name.clone())
                .or_default()
                .insert(case.cons_name.clone(), funname_use);
        }
    }

    pub fn create_constructors(&mut self) {
        self.ast_ctx
            .types
            .values()
            .cloned()
            .collect::<Vec<_>>()
            .iter()
            .for_each(|x| self.create_constructors_for_enum(x));
    }

    pub fn create_pool_funs(&mut self) {
        self.ast_ctx
            .types
            .iter()
            .filter(|(x, _)| AstTy::named(x).is_recursive(&self.ast_ctx))
            .map(|(_, x)| x.clone())
            .collect::<Vec<_>>()
            .iter()
            .for_each(|x| self.create_pool_funs_for_enum(x));
    }

    pub fn create_pool_funs_for_enum(&mut self, e: &EnumDef) {
        let self_ty = self.ast_ty_to_ty(&AstTy::named(&e.name));

        // Allocate type
        let name = FunName::fresh();
        let n = Use::from(&name);
        let f = Func::new(name, self.ctx.make_params(empty()), self_ty.clone(), None);
        self.add_func(f);
        self.add_named_func(format!("{}_pool_allocate", e.name), n);

        // Retain type
        let name = FunName::fresh();
        let n = Use::from(&name);
        let f = Func::new(
            name,
            self.ctx.make_params(once(self_ty.clone())),
            Ty::Void,
            None,
        );
        self.add_func(f);
        self.add_named_func(format!("{}_retain", e.name), n);

        // Release type
        let name = FunName::fresh();
        let n = Use::from(&name);
        let f = Func::new(
            name,
            self.ctx.make_params(once(self_ty.clone())),
            Ty::Void,
            None,
        );
        self.add_func(f);
        self.add_named_func(format!("{}_release", e.name), n);

        // Is unique
        let name = FunName::fresh();
        let n = Use::from(&name);
        let f = Func::new(
            name,
            self.ctx.make_params(once(self_ty.clone())),
            Ty::Int,
            None,
        );
        self.add_func(f);
        self.add_named_func(format!("{}_is_unique", e.name), n);
    }

    pub fn drop_ty(&mut self, val: Value, ty: &AstTy, b: &mut Builder) {
        let cfg_ty = self.ast_ty_to_ty(ty);
        assert!(cfg_ty.matches(&val.get_type(&self.ctx)));
        match ty {
            AstTy::Tuple(items) => {
                self.inject_print("Dropping tuple", b);
                for (i, item) in items.iter().enumerate() {
                    let v = b.extract(&mut self.ctx, val.clone(), i);
                    self.drop_ty(v.into(), item, b);
                }
            }
            AstTy::Fun { .. } => {
                self.inject_print("Dropping closure", b);
                let drop_object = self.prog.natives()["drop_object"].clone();
                let val = b.extract(&mut self.ctx, val, 1);
                b.native_call(
                    &mut self.ctx,
                    Const::FunPtr(drop_object).into(),
                    vec![val.into()],
                );
            }
            AstTy::Named(ty) => {
                self.inject_print("Dropping {ty}", b);
                let drop_fun = self.drops[ty].clone();
                b.native_call(&mut self.ctx, Const::FunPtr(drop_fun).into(), vec![val]);
            }
            _ => (),
        }
    }

    pub fn borrow_ty(&mut self, val: Value, ty: &AstTy, b: &mut Builder) {
        let cfg_ty = self.ast_ty_to_ty(ty);
        assert!(cfg_ty.matches(&val.get_type(&self.ctx)));
        match ty {
            AstTy::Tuple(items) => {
                for (i, item) in items.iter().enumerate() {
                    let v = b.extract(&mut self.ctx, val.clone(), i);
                    self.inject_print(format!("Borrowing {}\n", item), b);
                    self.borrow_ty(v.into(), item, b);
                }
            }
            AstTy::Fun { .. } => {
                self.inject_print("Borrowing closure\n", b);
                let borrow_object = self.prog.natives()["borrow_object"].clone();
                let val = b.extract(&mut self.ctx, val, 1);
                b.native_call(
                    &mut self.ctx,
                    Const::FunPtr(borrow_object).into(),
                    vec![val.into()],
                );
            }
            AstTy::Named(ty) => {
                let borrow_fun = self.borrows[ty].clone();
                self.inject_print(format!("Borrowing {ty} with borrows funcs\n",), b);
                b.native_call(&mut self.ctx, Const::FunPtr(borrow_fun).into(), vec![val]);
            }
            _ => (),
        }
    }
}
