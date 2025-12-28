use std::{
    collections::{HashMap, HashSet},
    iter::{empty, once, repeat_n},
    mem,
};

use crate::{
    ast::{Ast, AstTy, Var},
    cfg::{
        Const, FunName, FunNameUse, Func, Label, Program, Sig, Ty, TyCtx, Value, builder::Builder,
        var::CfgVarUse,
    },
    helpers::unique::Use,
};

pub struct Compiler {
    prog: Program,
    map: HashMap<Var, CfgVarUse>,
    type_map: HashMap<Var, Ty>,
    ctx: TyCtx,
    wrapped_natives: HashMap<FunNameUse, FunNameUse>,
}

impl Compiler {
    fn create_add(&mut self) {
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

    fn create_print_int(&mut self) {
        let name = FunName::fresh();
        let used = Use::from(&name);
        let s = Sig {
            params: vec![Ty::Int],
            ret: Box::new(Ty::Void),
        };
        let print_int_fun = Func {
            name: name,
            params: self.ctx.make_params(s.params.into_iter()),
            ret_ty: *s.ret,
            cfg: None,
        };
        self.add_func(print_int_fun);
        self.add_named_func("print_int", used);
    }

    fn add_named_func<S: ToString>(&mut self, alias: S, fun_name: FunNameUse) {
        self.prog
            .add_native_alias(alias.to_string(), fun_name.clone());
        self.ctx.add_native_alias(alias, fun_name);
    }

    fn add_func(&mut self, f: Func) {
        let sig = Sig {
            params: f.params.iter().map(|(_, x)| x.clone()).collect(),
            ret: Box::new(f.ret_ty.clone()),
        };
        let name = Use::from(&f.name);
        self.prog.funcs.insert(f);
        self.ctx.sigs.insert(name, sig);
    }

    fn new(entry: FunNameUse) -> Self {
        let mut res = Self {
            prog: Program::new(entry),
            type_map: HashMap::new(),
            map: HashMap::new(),
            ctx: TyCtx::new(),
            wrapped_natives: HashMap::new(),
        };
        res.create_add();
        res.create_print_int();
        res
    }

    pub fn compile(ast: Ast) -> Program {
        let entry = FunName::fresh();
        let entry_use = Use::from(&entry);
        let mut res = Self::new(entry_use);
        let mut b = Builder::new(entry, vec![], Ty::Void, &mut res.ctx);
        res.aux(ast, &mut b);
        b.ret_void(&mut res.ctx);
        res.add_func(b.finalize());
        res.ctx.dump_aliases_in_prog(&mut res.prog);
        res.prog
    }

    fn get_closure(&self, arg: Ty, ret: Ty) -> Ty {
        let void_ptr = Ty::Ptr(Box::new(Ty::Void));
        let params_ty = vec![void_ptr.clone(), arg];
        let funptr_ty = Ty::FunPtr(Sig {
            params: params_ty,
            ret: Box::new(ret),
        });
        Ty::Struct(vec![funptr_ty, void_ptr])
    }

    fn get_closure_ty(&self, arg: &AstTy, ret: &AstTy) -> Ty {
        let arg_ty = self.ast_ty_to_ty(arg);
        let ret_ty = self.ast_ty_to_ty(ret);
        self.get_closure(arg_ty, ret_ty)
    }

    fn ast_ty_to_ty(&self, t: &AstTy) -> Ty {
        match t {
            AstTy::Int => Ty::Int,
            AstTy::String => Ty::String,
            AstTy::Tuple(items) => Ty::Struct(items.iter().map(|x| self.ast_ty_to_ty(x)).collect()),
            AstTy::Fun { arg, ret } => self.get_closure_ty(arg, ret),
        }
    }

    fn aux(&mut self, ast: Ast, b: &mut Builder) -> Value {
        match ast {
            Ast::Str(s) => Const::String(s).into(),
            Ast::Int(i) => Const::Int(i).into(),
            Ast::Var(var) => Value::Var(self.map[&var].clone()),
            Ast::Lambda { arg, body } => {
                self.compile_lambda(arg.expr().clone(), arg.ty().clone(), *body, b)
            }
            Ast::App { fun, arg } => {
                let fun_val = self.aux(*fun, b);
                let arg_val = self.aux(*arg, b);
                b.call(&mut self.ctx, fun_val, arg_val).into()
            }
            Ast::Seq { fst, snd } => {
                let _ = self.aux(*fst, b);
                self.aux(*snd, b)
            }
            Ast::Tuple(asts) => {
                let vals = asts.into_iter().map(|x| self.aux(x, b)).collect();
                b.aggregate(&mut self.ctx, vals).into()
            }
            Ast::Get { from, index } => {
                let from_val = self.aux(*from, b);
                b.extract(&mut self.ctx, from_val, index).into()
            }
            Ast::Native(name) => self.get_native_closure(name, b),
        }
    }

    fn capture(&self, param: &Var, ast: &Ast) -> Vec<Var> {
        let mut free_vars = ast.free_vars();
        free_vars.remove(param);
        let mut res = free_vars.into_iter().collect::<Vec<_>>();
        res.sort_by(|x, y| x.cmp(y));
        res
    }

    fn get_type_of_ast(&mut self, ast: &Ast) -> Ty {
        match ast {
            Ast::Str(_) => Ty::String,
            Ast::Int(_) => Ty::Int,
            Ast::Var(var) => self.map[var].get_type(&self.ctx),
            Ast::Lambda { arg, body } => {
                let arg_ty = {
                    if !self.type_map.contains_key(arg.expr()) {
                        let arg_ty = self.ast_ty_to_ty(arg.ty());
                        self.type_map.insert(arg.expr().clone(), arg_ty.clone());
                        arg_ty
                    } else {
                        self.type_map[arg.expr()].clone()
                    }
                };
                let ty = self.get_type_of_ast(body);

                self.get_closure(arg_ty, ty)
            }
            Ast::App { fun, .. } => {
                let typeof_fun = self.get_type_of_ast(fun);
                *typeof_fun.field(0).sig().ret.clone()
            }
            Ast::Seq { snd, .. } => self.get_type_of_ast(snd),
            Ast::Tuple(asts) => Ty::Struct(asts.iter().map(|x| self.get_type_of_ast(x)).collect()),
            Ast::Get { from, index } => self.get_type_of_ast(from).field(*index),
            Ast::Native(name) => {
                let f = &self.ctx.natives[name];
                let sig = &self.ctx.sigs[f];
                self.curry(sig)
            }
        }
    }

    fn curry_aux(&self, lst: &[Ty], s: &mut HashMap<usize, Ty>) -> Ty {
        let l = lst.len();
        if s.contains_key(&l) {
            s[&l].clone()
        } else {
            let res = if l == 2 {
                self.get_closure(lst[0].clone(), lst[1].clone())
            } else if l > 2 {
                let ret_ty = self.curry_aux(&lst[1..], s);
                let arg = lst[0].clone();
                self.get_closure(arg, ret_ty)
            } else {
                unreachable!()
            };
            s.insert(l, res.clone());
            res
        }
    }

    fn curry(&self, s: &Sig) -> Ty {
        let mut v = s.params.clone();
        v.push(s.ret.as_ref().clone());
        let l = v.len();
        let mut map = HashMap::new();
        self.curry_aux(&v[..], &mut map);
        map.remove(&l).unwrap()
    }

    fn compile_lambda(&mut self, arg: Var, ty: AstTy, body: Ast, b: &mut Builder) -> Value {
        let old_ctx = self.ctx.clone();
        let old_map = self.map.clone();

        let function_name = FunName::fresh();
        let env = self.capture(&arg, &body);
        let new_vars = env
            .iter()
            .map(|x| {
                let associated = &self.map[x];
                self.ctx.vars[&associated].clone()
            })
            .collect::<Vec<_>>();

        let new_vars_len = new_vars.len();

        let closure_struct = Ty::Struct(new_vars);
        let closure_env_ty = Ty::Ptr(Box::new(closure_struct.clone()));

        let arg_ty = self.ast_ty_to_ty(&ty);
        let cfg_arg = self.ctx.new_var(arg_ty.clone());
        let cfg_arg_use = Use::from(&cfg_arg);
        let env_arg = self.ctx.new_var(closure_env_ty.clone());
        let env_arg_use = Use::from(&env_arg);
        self.map.insert(arg, cfg_arg_use);

        let params = vec![(env_arg, closure_env_ty), (cfg_arg, arg_ty)];

        let ret_ty = self.get_type_of_ast(&body);

        let mut fun_builder = Builder::new(function_name, params, ret_ty, &mut self.ctx);
        if new_vars_len > 0 {
            let loaded_env =
                fun_builder.load(&mut self.ctx, env_arg_use.into(), closure_struct.clone());

            env.iter().enumerate().for_each(|(i, x)| {
                let associated = fun_builder.extract(&mut self.ctx, loaded_env.clone().into(), i);
                *self.map.get_mut(x).unwrap() = associated;
            });
        }

        let ret_value = self.aux(body, &mut fun_builder);

        let ty = ret_value.get_type(&self.ctx);

        if ty != Ty::Void {
            fun_builder.ret(&mut self.ctx, ret_value);
        } else {
            fun_builder.ret_void(&mut self.ctx);
        }

        let func = fun_builder.finalize();
        let fun_name = Use::from(&func.name);
        self.prog.add_func(func);

        for (x, y) in old_map {
            self.map.insert(x, y);
        }
        for (x, y) in old_ctx.vars {
            self.ctx.vars.insert(x, y);
        }

        let funptr = b.constant(&mut self.ctx, Const::FunPtr(fun_name));

        let env_values = env
            .iter()
            .map(|x| self.map[x].clone().into())
            .collect::<Vec<_>>();

        let env_struct = b.aggregate(&mut self.ctx, env_values);
        let malloc = self.malloc_val(env_struct.into(), b);

        b.aggregate(&mut self.ctx, vec![funptr.into(), malloc.into()])
            .into()
    }

    fn malloc_val(&mut self, v: Value, b: &mut Builder) -> Value {
        let v_ty = v.get_type(&self.ctx);
        if v_ty.is_zero_sized() {
            Value::Const(Const::NullPtr)
        } else {
            let malloc = b.malloc_single(&mut self.ctx, v_ty);
            b.store(&mut self.ctx, malloc.clone().into(), v.into());
            malloc.into()
        }
    }

    fn get_native_closure(&mut self, name: String, b: &mut Builder) -> Value {
        let funname = self.ctx.natives[&name].clone();

        let wrapper = if !self.wrapped_natives.contains_key(&funname) {
            self.create_native_closure(name, b)
        } else {
            self.wrapped_natives[&funname].clone()
        };

        b.constant(
            &mut self.ctx,
            Const::Struct(vec![Const::FunPtr(wrapper), Const::NullPtr]),
        )
        .into()
    }

    fn create_native_closure(&mut self, name: String, b: &mut Builder) -> FunNameUse {
        let funname = self.ctx.natives[&name].clone();
        // Create the closure tree (all intermediate functions taking a single arg as well as the closure env) of the native function and return the wrapper as a closure
        let sig = self.ctx.sigs[&funname].clone();
        assert!(sig.params.len() > 0);

        let innermost_use = self.create_innermost_closure(name.clone(), funname.clone());
        let mut remaining_args = sig.params.len() - 1;
        let mut last = innermost_use.clone();
        while remaining_args > 0 {
            let wrapper_name = FunName::fresh();
            let wrapper_use = Use::from(&wrapper_name);
            let void_ptr = Ty::Ptr(Box::new(Ty::Void));
            let last_sig = self.ctx.sigs.get(&last).unwrap().clone();
            let last_ty = Ty::Struct(vec![Ty::FunPtr(last_sig), void_ptr.clone()]);
            let ret_ty = self.get_closure(sig.params[remaining_args - 1].clone(), last_ty);
            let params = self
                .ctx
                .make_params([void_ptr, sig.params[remaining_args - 1].clone()].into_iter());
            let use_params = params.iter().map(|(x, _)| Use::from(x)).collect::<Vec<_>>();
            let mut wrapper_func_builder =
                Builder::new(wrapper_name, params, ret_ty, &mut self.ctx);

            let mut new_env_values = if remaining_args > 1 {
                let env_ptr = use_params[0].clone();
                let env_ty =
                    Ty::Struct(sig.params[0..remaining_args - 1].iter().cloned().collect());
                let loaded_env = wrapper_func_builder.load(&mut self.ctx, env_ptr.into(), env_ty);
                (0..remaining_args - 1)
                    .into_iter()
                    .map(|index| {
                        wrapper_func_builder
                            .extract(&mut self.ctx, loaded_env.clone().into(), index)
                            .into()
                    })
                    .collect::<Vec<Value>>()
            } else {
                vec![]
            };

            new_env_values.push(use_params[1].clone().into());

            let aggregate_env = wrapper_func_builder.aggregate(&mut self.ctx, new_env_values);
            let malloc = self.malloc_val(aggregate_env.clone().into(), &mut wrapper_func_builder);
            let closure_struct = wrapper_func_builder.aggregate(
                &mut self.ctx,
                vec![Const::FunPtr(last).into(), malloc.into()],
            );

            wrapper_func_builder.ret(&mut self.ctx, closure_struct.into());
            let func = wrapper_func_builder.finalize();
            self.add_func(func);
            remaining_args -= 1;
            last = wrapper_use;
        }

        self.wrapped_natives.insert(funname, last.clone());

        last
    }

    // Create innermost closure because it calls the native function with the unfolded closure args and the last argument
    fn create_innermost_closure(&mut self, name: String, funname: Use<FunName>) -> Use<FunName> {
        let innermost_name = FunName::fresh();
        let innermost_use = Use::from(&innermost_name);
        let sig = self.ctx.sigs[&funname].clone();
        assert!(sig.params.len() > 0);
        let last_arg = sig.params.last().unwrap();
        let ret_ty = sig.ret.as_ref();
        let params = self
            .ctx
            .make_params([Ty::Ptr(Box::new(Ty::Void)), last_arg.clone()].into_iter());
        let use_params = params.iter().map(|(x, _)| Use::from(x)).collect::<Vec<_>>();
        let mut innermost_builder =
            Builder::new(innermost_name, params, ret_ty.clone(), &mut self.ctx);
        let mut args = if sig.params.len() > 1 {
            let env_iter = sig.params[..sig.params.len() - 1].iter();
            let env_ty = Ty::Struct(env_iter.clone().cloned().collect::<Vec<_>>());
            let env_ptr = use_params[0].clone();
            let loaded_env: Value = innermost_builder
                .load(&mut self.ctx, env_ptr.into(), env_ty)
                .into();
            env_iter
                .enumerate()
                .map(|(i, _)| {
                    innermost_builder
                        .extract(&mut self.ctx, loaded_env.clone(), i)
                        .into()
                })
                .collect::<Vec<_>>()
        } else {
            vec![]
        };
        args.push(use_params[1].clone().into());
        let res = innermost_builder.native_call(&mut self.ctx, name, args);
        if res.get_type(&self.ctx) == Ty::Void {
            innermost_builder.ret_void(&self.ctx);
        } else {
            innermost_builder.ret(&self.ctx, res.into());
        }
        let func = innermost_builder.finalize();
        self.add_func(func);
        innermost_use
    }
}

impl Program {
    fn new(entry: FunNameUse) -> Self {
        Self {
            entry,
            natives: HashMap::new(),
            funcs: HashSet::new(),
        }
    }
}
