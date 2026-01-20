use crate::{
    cfg::{FunName, Func, Sig, Ty, builder::Builder},
    helpers::unique::Use,
    lower::mono_to_cfg::MonoToCfg,
};

impl MonoToCfg {
    pub fn create_add(&mut self) {
        let add_name = FunName::fresh();
        let used = Use::from(&add_name);

        let params = self.ctx.make_params([Ty::Int, Ty::Int].into_iter());
        let x = Use::from(&params[0].0);
        let y = Use::from(&params[1].0);

        let mut b = Builder::new(add_name, params, Ty::Int, &mut self.ctx);
        let res = b.add(&mut self.ctx, x.into(), y.into());
        b.ret(&mut self.ctx, res.into());
        let f = b.finalize();
        self.add_func(f);
        self.add_named_func("add", used);
    }

    pub fn create_mul(&mut self) {
        let mul_name = FunName::fresh();
        let used = Use::from(&mul_name);

        let params = self.ctx.make_params([Ty::Int, Ty::Int].into_iter());
        let x = Use::from(&params[0].0);
        let y = Use::from(&params[1].0);

        let mut b = Builder::new(mul_name, params, Ty::Int, &mut self.ctx);
        let res = b.mul(&mut self.ctx, x.into(), y.into());
        b.ret(&mut self.ctx, res.into());
        let f = b.finalize();
        self.add_func(f);
        self.add_named_func("mul", used);
    }

    pub fn create_print_int(&mut self) {
        let name = FunName::fresh();
        let used = Use::from(&name);
        let s = Sig::new(vec![Ty::Int], Ty::Void);
        let print_int_fun = Func::new(
            name,
            self.ctx.make_params(s.params().iter().cloned()),
            s.ret().clone(),
            None,
        );
        self.add_func(print_int_fun);
        self.add_named_func("print_int", used);
    }

    pub fn create_print_string(&mut self) {
        let name = FunName::fresh();
        let used = Use::from(&name);
        let s = Sig::new(vec![Ty::String], Ty::Void);
        let print_string_fun = Func::new(
            name,
            self.ctx.make_params(s.params().iter().cloned()),
            s.ret().clone(),
            None,
        );
        self.add_func(print_string_fun);
        self.add_named_func("print_string", used);
    }

    pub fn create_random_int(&mut self) {
        let name = FunName::fresh();
        let used = Use::from(&name);
        let s = Sig::new(vec![Ty::Int], Ty::Int);
        let print_string_fun = Func::new(
            name,
            self.ctx.make_params(s.params().iter().cloned()),
            s.ret().clone(),
            None,
        );
        self.add_func(print_string_fun);
        self.add_named_func("random_int", used);
    }

    pub fn create_borrow_object(&mut self) {
        let name = FunName::fresh();
        let used = Use::from(&name);
        let f = Func::new(
            name,
            self.ctx
                .make_params([Ty::Ptr(Box::new(Ty::Void))].into_iter()),
            Ty::Void,
            None,
        );
        self.add_func(f);
        self.add_named_func("borrow_object", used);
    }

    pub fn create_is_unique(&mut self) {
        let name = FunName::fresh();
        let used = Use::from(&name);
        let f = Func::new(
            name,
            self.ctx
                .make_params([Ty::Ptr(Box::new(Ty::Void))].into_iter()),
            Ty::Int,
            None,
        );
        self.add_func(f);
        self.add_named_func("is_unique", used);
    }

    pub fn create_drop_object(&mut self) {
        let name = FunName::fresh();
        let used = Use::from(&name);
        let f = Func::new(
            name,
            self.ctx
                .make_params([Ty::Ptr(Box::new(Ty::Void))].into_iter()),
            Ty::Void,
            None,
        );
        self.add_func(f);
        self.add_named_func("drop_object", used);
    }

    pub fn create_register_object(&mut self) {
        let name = FunName::fresh();
        let used = Use::from(&name);
        let f = Func::new(
            name,
            self.ctx
                .make_params([Ty::Ptr(Box::new(Ty::Void))].into_iter()),
            Ty::Void,
            None,
        );
        self.add_func(f);
        self.add_named_func("register_object", used);
    }

    pub fn create_print_lst(&mut self) {
        let name = FunName::fresh();
        let used = Use::from(&name);
        let f = Func::new(
            name,
            self.ctx
                .make_params([Ty::Ptr(Box::new(Ty::Void))].into_iter()),
            Ty::Void,
            None,
        );
        self.add_func(f);
        self.add_named_func("print_lst", used);
    }
}
