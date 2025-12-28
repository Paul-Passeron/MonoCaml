use std::{
    collections::{HashMap, HashSet},
    iter::{empty, once},
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
}

impl Compiler {
    fn get_add_sig() -> Sig {
        Sig {
            params: vec![Ty::Int, Ty::Int],
            ret: Box::new(Ty::Int),
        }
    }

    fn create_add(&mut self) {
        let add_name = FunName::fresh();
        let used = Use::from(&add_name);
        let s = Self::get_add_sig();
        let add_fun = Func {
            name: add_name,
            params: self.ctx.make_params(s.params.into_iter()),
            ret_ty: *s.ret,
            cfg: None,
        };
        self.add_func(add_fun);
        self.add_named_func("add", used);
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
        };
        res.create_add();
        res
    }

    pub fn compile(ast: Ast) -> Program {
        let entry = FunName::fresh();
        let entry_use = Use::from(&entry);
        let mut res = Self::new(entry_use);
        let b = &mut Builder::new(entry, vec![], Ty::Void, &mut res.ctx);
        res.aux(ast, b);
        b.ret_void(&mut res.ctx);
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
        Ty::Ptr(Box::new(Ty::Struct(vec![funptr_ty, void_ptr])))
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
            Ast::Native(name) => self.create_native_closure(name, b),
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
                *typeof_fun.into_inner().field(0).sig().ret.clone()
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
        println!("Compiling lambda with arg {arg}");
        let mut old_ctx = self.ctx.clone();
        let mut old_map = self.map.clone();
        mem::swap(&mut old_ctx, &mut self.ctx);
        mem::swap(&mut old_map, &mut self.map);

        let function_name = FunName::fresh();
        let env = self.capture(&arg, &body);
        for x in &env {
            println!("ENV elem is {x}");
        }

        let new_vars = env
            .iter()
            .map(|x| {
                let associated = &self.map[x];
                self.ctx.vars[&associated].clone()
            })
            .collect::<Vec<_>>();

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

        let loaded_env =
            fun_builder.load(&mut self.ctx, env_arg_use.into(), closure_struct.clone());

        env.iter().enumerate().for_each(|(i, x)| {
            let associated = fun_builder.extract(&mut self.ctx, loaded_env.clone().into(), i);
            *self.map.get_mut(x).unwrap() = associated;
        });

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

        mem::swap(&mut self.ctx, &mut old_ctx);
        mem::swap(&mut self.map, &mut old_map);

        let funptr = b.constant(&mut self.ctx, Const::FunPtr(fun_name));

        let env_values = env
            .iter()
            .map(|x| self.map[x].clone().into())
            .collect::<Vec<_>>();

        let env_struct = b.aggregate(&mut self.ctx, env_values);

        let malloc = b.malloc_single(&mut self.ctx, closure_struct);

        b.store(&mut self.ctx, malloc.clone().into(), env_struct.into());

        b.aggregate(&mut self.ctx, vec![funptr.into(), malloc.into()])
            .into()
    }

    fn create_native_closure(&mut self, name: String, b: &mut Builder) -> Value {
        let funname = self.ctx.natives[&name].clone();
        println!("Found native {name} as {funname}");
        // Create the closure tree (all intermediate functions taking a single arg as well as the closure env) of the native function and return the wrapper as a closure
        //

        // Create innermost closure because it calls the native function with the unfolded closure args and the last argument

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
            let env_iter = sig.params.iter().rev().skip(1).rev();
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

        println!("Native func:\n{func}");

        self.add_func(func);

        todo!()
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
