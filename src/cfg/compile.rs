use std::{
    collections::{HashMap, HashSet},
    mem,
};

use crate::{
    ast::{Ast, AstTy, Var},
    cfg::{
        Const, FunName, FunNameUse, Label, Program, Ty, TyCtx, Value, builder::Builder,
        var::CfgVarUse,
    },
    helpers::unique::Use,
};

pub struct Compiler {
    prog: Program,
    map: HashMap<Var, CfgVarUse>,
    ctx: TyCtx,
}

impl Compiler {
    fn new(entry: FunNameUse) -> Self {
        Self {
            prog: Program::new(entry),
            map: HashMap::new(),
            ctx: TyCtx::new(),
        }
    }

    pub fn compile(ast: Ast) -> Program {
        let entry = FunName::fresh();
        let entry_use = Use::from(&entry);
        let mut res = Self::new(entry_use);
        let b = &mut Builder::new(entry, vec![], Ty::Void, &mut res.ctx);
        res.aux(ast, b);
        b.ret_void(&mut res.ctx);
        &mut res.ctx.dump_aliases_in_prog(&mut res.prog);
        res.prog
    }

    fn ast_ty_to_ty(&self, t: AstTy) -> Ty {
        // match t {}
        todo!()
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

    fn get_type_of_ast(&self, ast: &Ast) -> Ty {
        todo!()
    }

    fn compile_lambda(&mut self, arg: Var, ty: AstTy, body: Ast, b: &mut Builder) -> Value {
        let mut old_ctx = self.ctx.clone();
        let mut old_map = HashMap::new();
        mem::swap(&mut old_ctx, &mut self.ctx);
        mem::swap(&mut old_map, &mut self.map);

        let function_name = FunName::fresh();
        let env = self.capture(&arg, &body);
        let new_vars = env
            .iter()
            .map(|x| {
                let associated = &self.map[x];
                self.ctx.vars[&associated].clone()
            })
            .collect::<Vec<_>>();

        let closure_struct = Ty::Struct(new_vars);
        let closure_env_ty = Ty::Ptr(Box::new(closure_struct.clone()));

        let arg_ty = self.ast_ty_to_ty(ty);
        let arg = self.ctx.new_var(arg_ty.clone());
        let env_arg = self.ctx.new_var(closure_env_ty.clone());
        let env_arg_use = Use::from(&env_arg);

        let params = vec![(env_arg, closure_env_ty), (arg, arg_ty)];

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
