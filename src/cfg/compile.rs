use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Ast, AstTy, Var},
    cfg::{
        Const, FunName, FunNameUse, Program, Ty, TyCtx, Value, builder::Builder, var::CfgVarUse,
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

    fn compile_lambda(&mut self, arg: Var, ty: AstTy, body: Ast, b: &mut Builder) -> Value {
        let ctx = self.ctx.clone();
        let cfg_ty = self.ast_ty_to_ty(ty);
        let cfg_var = self.ctx.new_var(cfg_ty);
        let cfg_use = Use::from(&cfg_var);
        self.map.insert(arg, cfg_use);
        todo!()
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
