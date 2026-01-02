use crate::{
    ast::types::{AstTy, EnumDef},
    cfg::{Const, FunName, Label, Ty, Value, builder::Builder, compile::Compiler},
    helpers::unique::Use,
};

impl Compiler {
    fn create_borrows_for_enum(&mut self, enum_def: &EnumDef, funname: FunName) {
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

        let discr: Value = if is_rec {
            let borrow_object = self.prog.natives()["borrow_object"].clone();
            builder.native_call(
                &mut self.ctx,
                Const::FunPtr(borrow_object).into(),
                vec![param_use.clone().into()],
            );
            let ptr = builder.get_element_ptr(
                &mut self.ctx,
                param_use.clone().into(),
                self_ty.into_inner(),
                0,
            );
            builder.load(&mut self.ctx, ptr.into(), Ty::Int).into()
        } else {
            builder
                .extract(&mut self.ctx, param_use.clone().into(), 0)
                .into()
        };

        let ret_lbl = Label::fresh();
        let ret_use = Use::from(&ret_lbl);

        for (i, case) in enum_def.cases.iter().enumerate() {
            let is_i = builder.eq(&mut self.ctx, discr.clone(), Const::Int(i as i32).into());
            let case_body_lbl = Label::fresh();
            let case_body_use = Use::from(&case_body_lbl);
            let next_check_lbl = Label::fresh();
            let next_check_use = Use::from(&next_check_lbl);

            builder.branch(
                &mut self.ctx,
                is_i,
                case_body_use,
                next_check_use.clone(),
                case_body_lbl,
            );

            let struct_repr = Ty::Struct(vec![Ty::Int, Ty::Ptr(Box::new(Ty::Void))]);
            for t in case.arg.iter() {
                let repr = self.ast_ty_to_ty_pro(t, true);
                let val = if is_rec {
                    let ptr = builder.get_element_ptr(
                        &mut self.ctx,
                        param_use.clone().into(),
                        struct_repr.clone(),
                        1,
                    );
                    builder.load(&mut self.ctx, ptr.into(), repr.clone()).into()
                } else {
                    builder
                        .extract(&mut self.ctx, param_use.clone().into(), 1)
                        .into()
                };
                self.borrow_ty(val, t, &mut builder);
            }

            builder.goto(&mut self.ctx, ret_use.clone(), next_check_lbl);
        }

        builder.goto(&mut self.ctx, ret_use, ret_lbl);

        builder.ret_void(&mut self.ctx);
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

    fn create_drops_for_enum(&mut self, enum_def: &EnumDef, funname: FunName) {
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

        let discr: Value = if is_rec {
            let drop_object = self.prog.natives()["drop_object"].clone();
            builder.native_call(
                &mut self.ctx,
                Const::FunPtr(drop_object).into(),
                vec![param_use.clone().into()],
            );
            let ptr = builder.get_element_ptr(
                &mut self.ctx,
                param_use.clone().into(),
                self_ty.into_inner(),
                0,
            );
            builder.load(&mut self.ctx, ptr.into(), Ty::Int).into()
        } else {
            builder
                .extract(&mut self.ctx, param_use.clone().into(), 0)
                .into()
        };

        let ret_lbl = Label::fresh();
        let ret_use = Use::from(&ret_lbl);

        for (i, case) in enum_def.cases.iter().enumerate() {
            let is_i = builder.eq(&mut self.ctx, discr.clone(), Const::Int(i as i32).into());
            let case_body_lbl = Label::fresh();
            let case_body_use = Use::from(&case_body_lbl);
            let next_check_lbl = Label::fresh();
            let next_check_use = Use::from(&next_check_lbl);

            builder.branch(
                &mut self.ctx,
                is_i,
                case_body_use,
                next_check_use.clone(),
                case_body_lbl,
            );

            let struct_repr = Ty::Struct(vec![Ty::Int, Ty::Ptr(Box::new(Ty::Void))]);
            for t in case.arg.iter() {
                let repr = self.ast_ty_to_ty_pro(t, true);
                let val = if is_rec {
                    let ptr = builder.get_element_ptr(
                        &mut self.ctx,
                        param_use.clone().into(),
                        struct_repr.clone(),
                        1,
                    );
                    builder.load(&mut self.ctx, ptr.into(), repr.clone()).into()
                } else {
                    builder
                        .extract(&mut self.ctx, param_use.clone().into(), 1)
                        .into()
                };
                self.drop_ty(val, t, &mut builder);
            }

            builder.goto(&mut self.ctx, ret_use.clone(), next_check_lbl);
        }

        builder.goto(&mut self.ctx, ret_use, ret_lbl);

        builder.ret_void(&mut self.ctx);
        let f = builder.finalize();
        self.add_func(f);
        self.drops
            .entry(enum_def.name.clone())
            .or_insert(funname_use);
    }

    pub fn create_drops(&mut self) {
        self.ast_ctx
            .types
            .iter()
            .map(|(_, x)| x.clone())
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
            .iter()
            .map(|(_, x)| x.clone())
            .collect::<Vec<_>>()
            .iter()
            .map(|x| (x, self.declare_borrows_for_enum(x)))
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|(x, name)| self.create_borrows_for_enum(x, name));
    }

    pub fn create_constructors(&mut self) {
        self.ast_ctx
            .types
            .iter()
            .map(|(_, x)| x.clone())
            .collect::<Vec<_>>()
            .iter()
            .for_each(|x| self.create_constructors_for_enum(x));
    }

    pub fn drop_ty(&mut self, val: Value, ty: &AstTy, b: &mut Builder) {
        match ty {
            AstTy::Tuple(items) => {
                for (i, item) in items.iter().enumerate() {
                    let ty_pro = self.ast_ty_to_ty_pro(item, true);
                    if ty_pro.is_ptr() {
                        let v = b.extract(&mut self.ctx, val.clone(), i);
                        self.drop_ty(v.into(), item, b);
                    }
                }
            }
            AstTy::Fun { .. } => {
                let drop_object = self.prog.natives()["drop_object"].clone();
                b.native_call(&mut self.ctx, Const::FunPtr(drop_object).into(), vec![val]);
            }
            AstTy::Named(ty) => {
                let drop_fun = self.drops[ty].clone();
                b.native_call(&mut self.ctx, Const::FunPtr(drop_fun).into(), vec![val]);
            }
            _ => (),
        }
    }

    pub fn borrow_ty(&mut self, val: Value, ty: &AstTy, b: &mut Builder) {
        match ty {
            AstTy::Tuple(items) => {
                for (i, item) in items.iter().enumerate() {
                    let ty_pro = self.ast_ty_to_ty_pro(item, true);
                    if ty_pro.is_ptr() {
                        let v = b.extract(&mut self.ctx, val.clone(), i);
                        self.borrow_ty(v.into(), item, b);
                    }
                }
            }
            AstTy::Fun { .. } => {
                let borrow_object = self.prog.natives()["borrow_object"].clone();
                b.native_call(
                    &mut self.ctx,
                    Const::FunPtr(borrow_object).into(),
                    vec![val],
                );
            }
            AstTy::Named(ty) => {
                let borrow_fun = self.borrows[ty].clone();
                b.native_call(&mut self.ctx, Const::FunPtr(borrow_fun).into(), vec![val]);
            }
            _ => (),
        }
    }
}
