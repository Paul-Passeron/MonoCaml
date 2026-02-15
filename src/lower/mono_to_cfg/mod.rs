use std::{collections::HashMap, iter::once};

use crate::{
    cfg::{
        Const, FunName, FunNameUse, Func, Label, Program, Sig, Ty, TyCtx, Value,
        builder::Builder,
        expr::Expr,
        var::{CfgVar, CfgVarUse},
    },
    helpers::unique::Use,
    mono_ir::{
        Ast, AstKind, MatchCase, Var,
        pattern::Pattern,
        types::{AstCtx, AstTy},
    },
};

pub mod enums;
pub mod native_funs;

pub enum RecFlag {
    Rec,
    NoRec,
}

pub struct MonoToCfg {
    pub prog: Program,
    pub map: HashMap<Var, CfgVarUse>,
    pub type_map: HashMap<Var, Ty>,
    pub ctx: TyCtx,
    pub wrapped_natives: HashMap<FunNameUse, FunNameUse>,
    pub ast_ctx: AstCtx,
    pub ast_tys: HashMap<Var, AstTy>,
    pub constructors: HashMap<String, HashMap<String, FunNameUse>>,
    pub borrows: HashMap<String, FunNameUse>,
    pub drops: HashMap<String, FunNameUse>,
    pub debug: bool,
}

#[allow(unused)]
impl MonoToCfg {
    fn get_enum_repr(m: &HashMap<String, Ty>) -> Ty {
        let discr = Ty::Int;
        let largest = m.values().max_by_key(|x| x.get_size()).unwrap().clone();
        Ty::Struct(vec![discr, largest])
    }

    pub fn add_named_func<S: ToString>(&mut self, alias: S, fun_name: FunNameUse) {
        let alias = alias.to_string();
        self.prog.add_native_alias(alias.clone(), fun_name.clone());
        self.ctx.add_native_alias(alias, fun_name);
    }

    pub fn add_func(&mut self, f: Func) {
        let sig = Sig::new(
            f.params().iter().map(|(_, x)| x.clone()).collect(),
            f.ret_ty().clone(),
        );
        let name = f.name();
        self.prog.mut_funcs().insert(f);
        self.ctx.sigs.insert(name, sig);
    }

    fn new(entry: FunNameUse, ast_ctx: AstCtx) -> Self {
        let mut res = Self {
            prog: Program::new(entry),
            type_map: HashMap::new(),
            map: HashMap::new(),
            ctx: TyCtx::new(),
            wrapped_natives: HashMap::new(),
            ast_ctx,
            ast_tys: HashMap::new(),
            constructors: HashMap::new(),
            borrows: HashMap::new(),
            drops: HashMap::new(),
            debug: false,
        };
        res.create_add();
        res.create_mul();
        res.create_print_int();
        res.create_print_string();
        res.create_random_int();
        res.create_borrow_object();
        res.create_drop_object();
        res.create_register_object();
        res.create_print_lst();
        res.create_is_unique();
        res
    }

    pub fn compile<T: Clone>(ast: Ast<T>, ctx: AstCtx) -> Program {
        let entry = FunName::fresh();
        let entry_use = Use::from(&entry);
        let mut res = Self::new(entry_use, ctx);
        res.create_pool_funs();
        res.create_boxed_types();
        res.create_borrows();
        res.create_drops();
        res.create_constructors();
        let _ = res.get_ast_ty_of(&ast);
        let mut b = Builder::new(entry, vec![], Ty::Void, &mut res.ctx);
        res.aux(&ast, &mut b);
        b.ret_void(&res.ctx);
        res.add_func(b.finalize());
        res.ctx.dump_aliases_in_prog(&mut res.prog);
        res.prog
    }

    fn get_closure(&self, arg: Ty, ret: Ty) -> Ty {
        let void_ptr = Ty::Ptr(Box::new(Ty::Void));
        let mut params_ty = vec![void_ptr.clone()];
        if !arg.is_zero_sized() {
            params_ty.push(arg);
        }
        let funptr_ty = Ty::FunPtr(Sig::new(params_ty, ret));
        Ty::Struct(vec![funptr_ty, void_ptr])
    }

    fn get_closure_ty(&self, arg: &AstTy, ret: &AstTy) -> Ty {
        let arg_ty = self.ast_ty_to_ty(arg);
        let ret_ty = self.ast_ty_to_ty(ret);
        self.get_closure(arg_ty, ret_ty)
    }

    fn get_closure_ty_pro(&self, arg: &AstTy, ret: &AstTy, r: bool) -> Ty {
        let arg_ty = self.ast_ty_to_ty_pro(arg, r);
        let ret_ty = self.ast_ty_to_ty_pro(ret, r);
        self.get_closure(arg_ty, ret_ty)
    }

    pub fn ast_ty_to_ty_pro(&self, t: &AstTy, r: bool) -> Ty {
        match t {
            AstTy::Int => Ty::Int,
            AstTy::String => Ty::String,
            AstTy::Tuple(items) if items.is_empty() => Ty::Void,
            AstTy::Tuple(items) => {
                Ty::Struct(items.iter().map(|x| self.ast_ty_to_ty_pro(x, r)).collect())
            }
            AstTy::Fun { arg, ret } => self.get_closure_ty_pro(arg, ret, r),
            AstTy::Named(ty) => {
                if AstTy::named(ty).is_recursive(&self.ast_ctx) {
                    if r {
                        let enum_def = &self.ast_ctx.types[ty];
                        let union_ty = Ty::Union(
                            enum_def
                                .cases
                                .iter()
                                .map(|x| {
                                    x.arg
                                        .as_ref()
                                        .map_or(Ty::Int, |t| self.ast_ty_to_ty_pro(t, false))
                                })
                                .collect(),
                        );

                        let fields = if !union_ty.is_zero_sized() {
                            vec![Ty::Int, union_ty]
                        } else {
                            vec![Ty::Int]
                        };
                        Ty::Ptr(Box::new(Ty::Struct(fields)))
                    } else {
                        Ty::Ptr(Box::new(Ty::Void))
                    }
                } else {
                    let enum_def = &self.ast_ctx.types[ty];
                    let union_ty = Ty::Union(
                        enum_def
                            .cases
                            .iter()
                            .map(|x| {
                                x.arg
                                    .as_ref()
                                    .map_or(Ty::Int, |t| self.ast_ty_to_ty_pro(t, false))
                            })
                            .collect(),
                    );

                    let fields = if !union_ty.is_zero_sized() {
                        vec![Ty::Int, union_ty]
                    } else {
                        vec![Ty::Int]
                    };
                    Ty::Struct(fields)
                }
            }
        }
    }

    pub fn ast_ty_to_ty(&self, t: &AstTy) -> Ty {
        self.ast_ty_to_ty_pro(t, true)
    }

    fn normalize_lambda<T: Clone>(&mut self, a: Ast<T>) -> (Option<(Var, AstTy, Ty)>, Ast<T>) {
        let ty = self.get_type_of_ast(&a);
        if !ty.repr_closure() {
            return (None, a);
        }
        match a.extract() {
            AstKind::Lambda { arg, body, arg_ty } => (
                {
                    let cfg_arg_ty = self.ast_ty_to_ty(&arg_ty);
                    Some((arg, arg_ty, cfg_arg_ty))
                },
                *body,
            ),
            _ => {
                panic!("This is actually not what we want to be doing")
            }
        }
    }

    fn is_sat_aux<T: Clone>(&self, ast: &Ast<T>, remaining: usize) -> bool {
        match ast.expr() {
            AstKind::App { fun, .. } => match fun.as_ref().expr() {
                AstKind::Native(x) => {
                    let name = &self.ctx.natives[x];
                    let sig = &self.ctx.sigs[name];
                    let l = sig.params().len();
                    l == remaining
                }
                _ => self.is_sat_aux(fun, remaining + 1),
            },
            _ => false,
        }
    }

    fn ast_is_saturated<T: Clone>(&self, ast: &Ast<T>) -> bool {
        match ast.expr() {
            AstKind::App { .. } => self.is_sat_aux(ast, 1),
            _ => false,
        }
    }

    fn get_sat_aux<T: Clone>(&self, ast: Ast<T>, v: &mut Vec<Ast<T>>) -> FunNameUse {
        match ast.extract() {
            AstKind::App { fun, arg } => {
                let res = self.get_sat_aux(*fun, v);
                v.push(*arg);
                res
            }
            AstKind::Native(x) => self.ctx.natives[&x].clone(),
            _ => panic!("Not a saturated call"),
        }
    }

    fn get_saturated_args_and_fun<T: Clone>(&self, ast: Ast<T>) -> (Vec<Ast<T>>, FunNameUse) {
        let mut v = vec![];
        let f = self.get_sat_aux(ast, &mut v);
        (v, f)
    }

    fn aux<T: Clone>(&mut self, ast: &Ast<T>, b: &mut Builder) -> Value {
        match ast.expr() {
            AstKind::Str(s) => Const::String(s.clone()).into(),
            AstKind::Int(i) => Const::Int(*i).into(),
            AstKind::Var(var) => Value::Var(self.map[var].clone()),
            AstKind::Lambda { arg, body, arg_ty } => {
                self.compile_lambda(*arg, arg_ty.clone(), body.as_ref().clone(), b)
            }
            AstKind::App { fun, arg } => {
                if self.ast_is_saturated(ast) {
                    let (args, name) = self.get_saturated_args_and_fun(ast.clone());
                    let vals = args.iter().map(|x| self.aux(x, b)).collect();
                    return b
                        .native_call(&mut self.ctx, Const::FunPtr(name).into(), vals)
                        .into();
                }

                let fun_val = self.aux(fun, b);
                let arg_val = self.aux(arg, b);
                let fun_ptr = b.extract(&mut self.ctx, fun_val.clone(), 0);
                let env_ptr = b.extract(&mut self.ctx, fun_val, 1);
                b.native_call(&mut self.ctx, fun_ptr.into(), vec![env_ptr.into(), arg_val])
                    .into()
            }
            AstKind::Seq { fst, snd } => {
                let _ = self.aux(fst, b);
                self.aux(snd, b)
            }
            AstKind::Tuple(asts) => {
                let vals = asts.iter().map(|x| self.aux(x, b)).collect();
                b.aggregate(&mut self.ctx, vals)
            }
            AstKind::Native(name) => self.get_native_closure(name.clone(), b),
            AstKind::LetBinding {
                bound,
                bound_ty,
                value,
                in_expr,
            } => {
                if value.free_vars().contains(bound) {
                    self.compile_rec_let(
                        *bound,
                        bound_ty.clone(),
                        value.as_ref().clone(),
                        in_expr.as_ref().clone(),
                        b,
                    )
                } else {
                    self.compile_nonrec_let(b, bound, bound_ty, value, in_expr)
                }
            }
            AstKind::If {
                cond,
                then_e,
                else_e,
            } => self.compile_if(b, cond, then_e, else_e),
            AstKind::Cons {
                enum_name,
                case,
                arg,
            } => {
                let arg = arg.as_ref().map(|x| self.aux(x, b));
                let cons_fun = self.constructors[enum_name][case].clone();
                b.native_call(
                    &mut self.ctx,
                    Const::FunPtr(cons_fun).into(),
                    arg.into_iter().collect(),
                )
                .into()
            }
            AstKind::Match { expr, cases } => {
                let target = self.get_type_of_case(&cases[0]);
                self.compile_match(expr, cases, b, target)
            }
        }
    }

    fn compile_nonrec_let<T: Clone>(
        &mut self,
        b: &mut Builder,
        bound: &Var,
        bound_ty: &AstTy,
        value: &Ast<T>,
        in_expr: &Ast<T>,
    ) -> Value {
        let val = self.aux(value, b);
        let bound_ty = self.ast_ty_to_ty(bound_ty);
        assert!(val.get_type(&self.ctx).matches(&bound_ty));
        let bound_cfg = self.ctx.new_var(bound_ty);
        self.map.insert(*bound, Use::from(&bound_cfg));
        b.assign_to(&mut self.ctx, bound_cfg, Expr::value(val));
        self.aux(in_expr, b)
    }

    fn compile_if<T: Clone>(
        &mut self,
        b: &mut Builder,
        cond: &Ast<T>,
        then_e: &Ast<T>,
        else_e: &Ast<T>,
    ) -> Value {
        let compiled_cond = self.aux(cond, b);
        let then_bb = Label::fresh();
        let else_bb = Label::fresh();
        let merge_bb = Label::fresh();
        let then_bb_use = Use::from(&then_bb);
        let else_bb_use = Use::from(&else_bb);
        let merge_bb_use = Use::from(&merge_bb);
        let res_ty = self.get_type_of_ast(then_e);

        let res = if !res_ty.is_zero_sized() {
            Some(b.alloca(&mut self.ctx, res_ty.clone()))
        } else {
            None
        };

        b.branch(&self.ctx, compiled_cond, then_bb_use, else_bb_use, then_bb);
        let then_value = match self.aux(then_e, b) {
            Value::Var(v) => v,
            c => b.value(&mut self.ctx, c),
        };

        if let Some(res) = &res {
            b.store(&self.ctx, res.clone().into(), then_value.into());
        }

        b.goto(&self.ctx, merge_bb_use.clone(), else_bb);

        let else_value = match self.aux(else_e, b) {
            Value::Var(v) => v,
            c => b.value(&mut self.ctx, c),
        };

        if let Some(res) = &res {
            b.store(&self.ctx, res.clone().into(), else_value.into());
        }
        b.goto(&self.ctx, merge_bb_use, merge_bb);
        res.map_or(Const::Struct(vec![]).into(), |x| {
            b.load(&mut self.ctx, x.clone().into(), res_ty).into()
        })
    }

    fn compile_rec_let<T: Clone>(
        &mut self,
        bound: Var,
        bound_ty: AstTy,
        value: Ast<T>,
        in_expr: Ast<T>,
        b: &mut Builder,
    ) -> Value {
        let cfg_bound_ty = self.ast_ty_to_ty(&bound_ty);
        assert!(cfg_bound_ty.repr_closure());
        self.type_map.insert(bound, cfg_bound_ty.clone());
        let (param, value) = self.normalize_lambda(value);
        let (param, ast_ty, arg_ty) = param.unwrap();

        let funname = FunName::fresh();
        let funname_use = Use::from(&funname);

        let mut outside_env = value.free_vars();
        outside_env.remove(&param);
        let mut outside_env: Vec<_> = outside_env.into_iter().collect();
        outside_env.sort();

        self.ctx
            .sigs
            .insert(funname_use.clone(), cfg_bound_ty.field(0).sig());
        self.create_initial_closure_for_recursion(
            bound,
            bound_ty.clone(),
            b,
            funname_use,
            outside_env.clone(),
        );

        // Now it's just like building a regular lambda (I think)

        let old_ctx = self.ctx.clone();
        let old_map = self.map.clone();

        let sig = cfg_bound_ty.field(0).sig();
        let ret_ty = sig.ret().clone();

        let env = outside_env.clone();

        let new_vars = env
            .iter()
            .map(|x| {
                let associated = &self.map[x];
                self.ctx.vars[associated].clone()
            })
            .collect::<Vec<_>>();

        let new_vars_len = new_vars.len();

        let closure_struct = Ty::Struct(new_vars.clone());
        let closure_env_ty = Ty::Ptr(Box::new(closure_struct.clone()));

        let cfg_arg = self.ctx.new_var(arg_ty.clone());
        let cfg_arg_use = Use::from(&cfg_arg);
        let env_arg = self.ctx.new_var(closure_env_ty.clone());
        let env_arg_use = Use::from(&env_arg);
        self.map.insert(param, cfg_arg_use.clone());

        let params = vec![(env_arg, closure_env_ty), (cfg_arg, arg_ty)];
        let params = params
            .into_iter()
            .filter(|x| !x.1.is_zero_sized())
            .collect();

        let mut builder = Builder::new(funname, params, ret_ty.clone(), &mut self.ctx);
        self.inject_print(
            format!(
                "BORROWING from compile_rec_let, in func {}\n",
                builder.funname()
            ),
            &mut builder,
        );
        self.borrow_ty(cfg_arg_use.clone().into(), &ast_ty, &mut builder);

        if new_vars_len > 0 {
            let loaded_env =
                builder.load(&mut self.ctx, env_arg_use.into(), closure_struct.clone());

            env.iter().enumerate().for_each(|(i, x)| {
                let associated = builder.extract(&mut self.ctx, loaded_env.clone().into(), i);
                *self.map.get_mut(x).unwrap() = associated;
            });

            env.iter().enumerate().for_each(|(i, x)| {
                let associated = builder.extract(&mut self.ctx, loaded_env.clone().into(), i);
                *self.map.get_mut(x).unwrap() = associated;
            });
        }

        let ret_value = self.aux(&value, &mut builder);
        self.inject_print(
            format!(
                "BORROWING from compile_rec_let, in func {}\n",
                builder.funname()
            ),
            &mut builder,
        );
        self.drop_ty(cfg_arg_use.into(), &ast_ty, &mut builder);

        if ret_ty.is_void() {
            builder.ret_void(&self.ctx);
        } else {
            builder.ret(&self.ctx, ret_value);
        }

        let func = builder.finalize();
        self.prog.add_func(func);

        for (x, y) in old_map {
            self.map.insert(x, y);
        }
        for (x, y) in old_ctx.vars {
            self.ctx.vars.insert(x, y);
        }

        self.aux(&in_expr, b)
    }

    fn create_initial_closure_for_recursion(
        &mut self,
        bound: Var,
        bound_ty: AstTy,
        b: &mut Builder,
        funname_use: Use<FunName>,
        outside_env: Vec<Var>,
    ) -> CfgVarUse {
        let mut pos = -1;

        let env_values = outside_env
            .iter()
            .enumerate()
            .map(|(i, x)| match self.map.get(x) {
                Some(x) => x.clone().into(),
                None => {
                    pos = i as i64;
                    Const::Struct(vec![Const::FunPtr(funname_use.clone()), Const::NullPtr]).into()
                }
            })
            .collect::<Vec<_>>();

        assert!(pos >= 0);

        let env_struct = b.aggregate(&mut self.ctx, env_values);

        let malloc = self.malloc_val(env_struct.clone(), b);
        let register_object = Const::FunPtr(self.ctx.natives["register_object"].clone());

        b.native_call(
            &mut self.ctx,
            register_object.clone().into(),
            vec![malloc.clone()],
        );

        let initial_closure = b.aggregate(
            &mut self.ctx,
            vec![Const::FunPtr(funname_use.clone()).into(), malloc],
        );

        let malloc = self.malloc_val(initial_closure.clone(), b);

        b.native_call(&mut self.ctx, register_object.into(), vec![malloc.clone()]);

        let clos_ty = malloc.get_type(&self.ctx).into_inner();

        let env_ptr_addr = b.get_element_ptr(&mut self.ctx, malloc.clone(), clos_ty.clone(), 1);

        let env_ty = env_struct.get_type(&self.ctx);

        let env_ptr = b.load(
            &mut self.ctx,
            env_ptr_addr.into(),
            Ty::Ptr(Box::new(env_ty.clone())),
        );

        let self_ref_addr =
            b.get_element_ptr(&mut self.ctx, env_ptr.into(), env_ty.clone(), pos as usize);

        b.store(&self.ctx, self_ref_addr.into(), initial_closure);
        let res = b.load(&mut self.ctx, malloc, clos_ty);
        self.map.insert(bound, res.clone());
        res
    }

    fn capture<T: Clone>(&self, param: &Var, ast: &Ast<T>) -> Vec<Var> {
        let mut free_vars = ast.free_vars();
        free_vars.remove(param);
        let mut res = free_vars.into_iter().collect::<Vec<_>>();
        res.sort();
        res
    }

    fn get_type_of_ast<T: Clone>(&mut self, ast: &Ast<T>) -> Ty {
        match ast.expr() {
            AstKind::Str(_) => Ty::String,
            AstKind::Int(_) => Ty::Int,
            AstKind::Var(var) => self.type_map[var].clone(),
            AstKind::Lambda { arg, arg_ty, body } => {
                let cfg_arg_ty = {
                    if !self.type_map.contains_key(arg) {
                        let cfg_arg_ty = self.ast_ty_to_ty(arg_ty);
                        self.type_map.insert(*arg, cfg_arg_ty.clone());
                        cfg_arg_ty
                    } else {
                        self.type_map[arg].clone()
                    }
                };
                let ty = self.get_type_of_ast(body);

                self.get_closure(cfg_arg_ty, ty)
            }
            AstKind::App { fun, .. } => {
                let typeof_fun = self.get_type_of_ast(fun);
                typeof_fun.field(0).sig().ret().clone()
            }
            AstKind::Seq { snd, .. } => self.get_type_of_ast(snd),
            AstKind::Tuple(asts) => {
                Ty::Struct(asts.iter().map(|x| self.get_type_of_ast(x)).collect())
            }
            AstKind::Native(name) => {
                let f = &self.ctx.natives[name];
                let sig = &self.ctx.sigs[f];
                self.curry(sig)
            }
            AstKind::LetBinding {
                bound,
                bound_ty,
                in_expr,
                ..
            } => {
                let bound_ty = self.ast_ty_to_ty(bound_ty);
                self.type_map.insert(*bound, bound_ty);
                self.get_type_of_ast(in_expr)
            }
            AstKind::If {
                cond,
                then_e,
                else_e,
            } => {
                if !self.get_type_of_ast(cond).is_arith() {
                    panic!("Condition must be arithmetic");
                }
                let t = self.get_type_of_ast(then_e);
                let e = self.get_type_of_ast(else_e);
                if !(t.matches(&e) || t.is_zero_sized() && e.is_zero_sized()) {
                    panic!("Branches must have the same type ({t} vs {e})");
                }
                t
            }
            AstKind::Cons { enum_name, .. } => self.ast_ty_to_ty(&AstTy::named(enum_name)),
            AstKind::Match { expr, cases } => {
                assert!(!cases.is_empty());
                self.get_type_of_case(&cases[0])
            }
        }
    }

    fn collect_types_in_pat(&mut self, pat: &Pattern) {
        match pat {
            Pattern::Int(_) => (),
            Pattern::Symbol(var, ty) => {
                self.ast_tys.insert(*var, ty.clone());
                let cfg_ty = self.ast_ty_to_ty(ty);
                self.type_map.insert(*var, cfg_ty);
            }
            Pattern::Cons {
                enum_name,
                cons,
                arg,
            } => {
                arg.iter().for_each(|x| self.collect_types_in_pat(x));
            }
            Pattern::Tuple(patterns) => {
                patterns.iter().for_each(|x| self.collect_types_in_pat(x));
            }
        }
    }

    fn get_type_of_case<T: Clone>(&mut self, case: &MatchCase<T>) -> Ty {
        self.collect_types_in_pat(&case.pat);
        self.get_type_of_ast(&case.expr)
    }

    fn get_ast_type_of_case<T: Clone>(&mut self, case: &MatchCase<T>) -> AstTy {
        self.collect_types_in_pat(&case.pat);
        self.get_ast_ty_of(&case.expr)
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
        let mut v = s.params().clone();
        v.push(s.ret().clone());
        let l = v.len();
        let mut map = HashMap::new();
        self.curry_aux(&v[..], &mut map);
        map.remove(&l).unwrap()
    }

    fn inject_print<S: ToString>(&mut self, str: S, b: &mut Builder) {
        if !self.debug {
            return;
        }
        let print_func = Const::FunPtr(self.prog.natives()["print_string"].clone());
        let print_arg = Const::String(str.to_string());
        b.native_call(&mut self.ctx, print_func.into(), vec![print_arg.into()]);
    }

    fn compile_lambda<T: Clone>(
        &mut self,
        arg: Var,
        ty: AstTy,
        body: Ast<T>,
        b: &mut Builder,
    ) -> Value {
        let old_ctx = self.ctx.clone();
        let old_map = self.map.clone();

        let function_name = FunName::fresh();
        let env = self.capture(&arg, &body);
        let new_vars = env
            .iter()
            .map(|x| {
                let associated = &self.map[x];
                self.ctx.vars[associated].clone()
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
        self.type_map.insert(arg, arg_ty.clone());
        self.map.insert(arg, cfg_arg_use.clone());

        let params = vec![(env_arg, closure_env_ty), (cfg_arg, arg_ty)];
        let params = params
            .into_iter()
            .filter(|x| !x.1.is_zero_sized())
            .collect();
        let ret_ty = self.get_type_of_ast(&body);

        let mut fun_builder = Builder::new(function_name, params, ret_ty.clone(), &mut self.ctx);
        self.inject_print(
            format!(
                "BORROWING from compile_lambda, in func {}\n",
                fun_builder.funname()
            ),
            &mut fun_builder,
        );
        self.borrow_ty(cfg_arg_use.clone().into(), &ty, &mut fun_builder);
        if new_vars_len > 0 {
            let loaded_env = fun_builder.load(
                &mut self.ctx,
                env_arg_use.clone().into(),
                closure_struct.clone(),
            );
            env.iter().enumerate().for_each(|(i, x)| {
                let associated = fun_builder.extract(&mut self.ctx, loaded_env.clone().into(), i);
                *self.map.get_mut(x).unwrap() = associated;
            });
        }
        let ret_value = self.aux(&body, &mut fun_builder);

        self.inject_print(
            format!(
                "DROPPING from compile_lambda, in func {}\n",
                fun_builder.funname()
            ),
            &mut fun_builder,
        );
        self.drop_ty(cfg_arg_use.into(), &ty, &mut fun_builder);

        if ret_ty.is_zero_sized() {
            fun_builder.ret_void(&self.ctx);
        } else {
            fun_builder.ret(&self.ctx, ret_value);
        }

        let func = fun_builder.finalize();
        let fun_name = func.name();
        self.prog.add_func(func);

        for (x, y) in old_map {
            self.map.insert(x, y);
        }
        for (x, y) in old_ctx.vars {
            self.ctx.vars.insert(x, y);
        }

        let env_values = env
            .iter()
            .map(|x| self.map[x].clone().into())
            .collect::<Vec<_>>();

        let env_struct = b.aggregate(&mut self.ctx, env_values);
        let malloc = self.malloc_val(env_struct, b);
        let register_object = Const::FunPtr(self.ctx.natives["register_object"].clone());
        b.native_call(&mut self.ctx, register_object.into(), vec![malloc.clone()]);

        b.aggregate(&mut self.ctx, vec![Const::FunPtr(fun_name).into(), malloc])
    }

    fn malloc_val(&mut self, v: Value, b: &mut Builder) -> Value {
        let v_ty = v.get_type(&self.ctx);
        if v_ty.is_zero_sized() {
            Value::Const(Const::NullPtr)
        } else {
            let malloc = b.malloc_single(&mut self.ctx, v_ty);
            b.store(&self.ctx, malloc.clone().into(), v);
            malloc.into()
        }
    }

    fn get_native_closure(&mut self, name: String, b: &mut Builder) -> Value {
        let funname = self.ctx.natives[&name].clone();

        let wrapper = if !self.wrapped_natives.contains_key(&funname) {
            self.create_native_closure(name)
        } else {
            self.wrapped_natives[&funname].clone()
        };

        b.constant(
            &mut self.ctx,
            Const::Struct(vec![Const::FunPtr(wrapper), Const::NullPtr]),
        )
        .into()
    }

    fn create_native_closure(&mut self, name: String) -> FunNameUse {
        let funname = self.ctx.natives[&name].clone();

        // Create the closure tree (all intermediate functions taking a single arg as well as the closure env) of the native function and return the wrapper as a closure
        let sig = self.ctx.sigs[&funname].clone();
        let l = sig.params().len();
        assert!(l > 0);

        let innermost_use = self.create_innermost_closure(name.clone());
        let mut remaining_args = sig.params().len() - 1;
        let mut last = innermost_use.clone();

        let mut current_env_ty = if l > 1 {
            Ty::Struct(sig.params()[0..l - 1].to_vec())
        } else {
            Ty::Struct(vec![])
        };

        while remaining_args > 0 {
            let wrapper_name = FunName::fresh();
            let wrapper_use = Use::from(&wrapper_name);
            let void_ptr = Ty::Ptr(Box::new(Ty::Void));
            let last_sig = self.ctx.sigs.get(&last).unwrap().clone();
            let param_index = l - remaining_args - 1;
            let ret_ty = Ty::Struct(vec![Ty::FunPtr(last_sig), void_ptr.clone()]);
            let params = self
                .ctx
                .make_params([void_ptr, sig.params()[param_index].clone()].into_iter());
            let params: Vec<(CfgVar, Ty)> = params
                .into_iter()
                .filter(|x| !x.1.is_zero_sized())
                .collect();
            let use_params = params.iter().map(|(x, _)| Use::from(x)).collect::<Vec<_>>();
            let mut wrapper_func_builder =
                Builder::new(wrapper_name, params, ret_ty, &mut self.ctx);

            let (mut new_env_values, env_ptr) = if remaining_args > 1 {
                let env_ptr = use_params[0].clone();
                let env_ty = Ty::Struct(sig.params()[0..remaining_args - 1].to_vec());
                let borrow_object = Const::FunPtr(self.ctx.natives["borrow_object"].clone());

                wrapper_func_builder.native_call(
                    &mut self.ctx,
                    borrow_object.into(),
                    vec![env_ptr.clone().into()],
                );
                let loaded_env =
                    wrapper_func_builder.load(&mut self.ctx, env_ptr.clone().into(), env_ty);
                let v = (0..remaining_args - 1)
                    .map(|index| {
                        wrapper_func_builder
                            .extract(&mut self.ctx, loaded_env.clone().into(), index)
                            .into()
                    })
                    .collect::<Vec<Value>>();
                (v, Some(Value::variable(env_ptr.clone())))
            } else {
                (vec![], None)
            };

            new_env_values.push(use_params[1].clone().into());

            let aggregate_env = wrapper_func_builder.aggregate(&mut self.ctx, new_env_values);
            current_env_ty = aggregate_env.get_type(&self.ctx);

            let malloc = self.malloc_val(aggregate_env.clone(), &mut wrapper_func_builder);
            let register_object = Const::FunPtr(self.ctx.natives["register_object"].clone());

            wrapper_func_builder.native_call(
                &mut self.ctx,
                register_object.into(),
                vec![malloc.clone()],
            );
            let closure_struct = wrapper_func_builder
                .aggregate(&mut self.ctx, vec![Const::FunPtr(last).into(), malloc]);

            if let Some(env_ptr) = env_ptr {
                self.inject_print(
                    format!("Dropping {}:{}\n", file!(), line!()),
                    &mut wrapper_func_builder,
                );
                let drop_object = Const::FunPtr(self.ctx.natives["drop_object"].clone());

                wrapper_func_builder.native_call(&mut self.ctx, drop_object.into(), vec![env_ptr]);
            }

            wrapper_func_builder.ret(&self.ctx, closure_struct);
            let func = wrapper_func_builder.finalize();
            self.add_func(func);
            remaining_args -= 1;
            last = wrapper_use;
        }

        self.wrapped_natives.insert(funname, last.clone());

        last
    }

    // Create innermost closure because it calls the native function with the unfolded closure args and the last argument
    fn create_innermost_closure(&mut self, name: String) -> Use<FunName> {
        let funname = self.ctx.natives[&name].clone();
        let innermost_name = FunName::fresh();
        let innermost_use = Use::from(&innermost_name);
        let sig = self.ctx.sigs[&funname].clone();
        let l = sig.params().len();
        assert!(l > 0);

        let last_arg = sig.params().last().unwrap();
        let ret_ty = sig.ret();
        let params = self
            .ctx
            .make_params([Ty::Ptr(Box::new(Ty::Void)), last_arg.clone()].into_iter());
        let params: Vec<(CfgVar, Ty)> = params
            .into_iter()
            .filter(|x| !x.1.is_zero_sized())
            .collect();
        let use_params = params.iter().map(|(x, _)| Use::from(x)).collect::<Vec<_>>();
        let mut innermost_builder =
            Builder::new(innermost_name, params, ret_ty.clone(), &mut self.ctx);
        let (mut args, env_ptr) = if l > 1 {
            let env_iter = sig.params()[..l - 1].iter();
            let env_ty = Ty::Struct(env_iter.clone().cloned().collect::<Vec<_>>());
            let env_ptr = use_params[0].clone();
            let borrow_object = Const::FunPtr(self.ctx.natives["borrow_object"].clone());

            innermost_builder.native_call(
                &mut self.ctx,
                borrow_object.into(),
                vec![env_ptr.clone().into()],
            );
            let loaded_env: Value = innermost_builder
                .load(&mut self.ctx, env_ptr.clone().into(), env_ty)
                .into();
            let v = env_iter
                .enumerate()
                .map(|(i, _)| {
                    innermost_builder
                        .extract(&mut self.ctx, loaded_env.clone(), i)
                        .into()
                })
                .collect::<Vec<_>>();
            (v, Some(Value::Var(env_ptr)))
        } else {
            (vec![], None)
        };
        args.push(use_params[1].clone().into());
        let res = innermost_builder.native_call(
            &mut self.ctx,
            Const::FunPtr(funname.clone()).into(),
            args,
        );

        if let Some(env_ptr) = env_ptr {
            self.inject_print(
                format!("Dropping {}:{}\n", file!(), line!()),
                &mut innermost_builder,
            );
            let drop_object = Const::FunPtr(self.ctx.natives["drop_object"].clone());

            innermost_builder.native_call(&mut self.ctx, drop_object.into(), vec![env_ptr.clone()]);
        };
        if res.get_type(&self.ctx).is_void() {
            innermost_builder.ret_void(&self.ctx);
        } else {
            innermost_builder.ret(&self.ctx, res.into());
        }
        let func = innermost_builder.finalize();
        self.add_func(func);
        innermost_use
    }

    fn get_ast_ty_of<T: Clone>(&mut self, a: &Ast<T>) -> AstTy {
        match a.expr() {
            AstKind::Str(_) => AstTy::String,
            AstKind::Int(_) => AstTy::Int,
            AstKind::Var(var) => self.ast_tys[var].clone(),
            AstKind::Lambda { arg, arg_ty, body } => {
                self.ast_tys.insert(*arg, arg_ty.clone());
                let ret = self.get_ast_ty_of(body);
                AstTy::fun(arg_ty.clone(), ret)
            }
            AstKind::App { fun, .. } => {
                let f_ty = self.get_ast_ty_of(fun);
                match f_ty {
                    AstTy::Fun { ret, .. } => *ret,
                    x => unreachable!("{x}"),
                }
            }
            AstKind::Seq { fst, snd } => {
                let _ = self.get_ast_ty_of(fst);
                self.get_ast_ty_of(snd)
            }
            AstKind::Tuple(asts) => {
                AstTy::Tuple(asts.iter().map(|x| self.get_ast_ty_of(x)).collect())
            }
            AstKind::Native(x) => self.ast_ctx.natives[x].clone(),
            AstKind::LetBinding {
                bound,
                bound_ty,
                value,
                in_expr,
            } => {
                self.ast_tys.insert(*bound, bound_ty.clone());
                let _ = self.get_ast_ty_of(value);
                self.get_ast_ty_of(in_expr)
            }
            AstKind::If {
                cond,
                then_e,
                else_e,
            } => {
                let _ = self.get_ast_ty_of(cond);
                let _ = self.get_ast_ty_of(then_e);
                self.get_ast_ty_of(else_e)
            }
            AstKind::Cons { enum_name, .. } => AstTy::Named(enum_name.clone()),
            AstKind::Match { expr, cases } => {
                assert!(!cases.is_empty());
                self.get_ast_type_of_case(&cases[0])
            }
        }
    }

    fn matches(
        &mut self,
        v: CfgVarUse,
        ty: &AstTy,
        pat: &Pattern,
        b: &mut Builder,
    ) -> (Value, HashMap<Var, CfgVarUse>) {
        match pat {
            Pattern::Int(x) => (
                b.eq(&mut self.ctx, Const::Int(*x).into(), v.into()),
                HashMap::new(),
            ),
            Pattern::Symbol(var, _) => (Const::Int(1).into(), HashMap::from_iter(once((*var, v)))),
            Pattern::Cons {
                enum_name,
                cons,
                arg,
            } => {
                match ty {
                    AstTy::Named(n) => {
                        if n != enum_name {
                            panic!("Expected type {} but found {}", n, enum_name)
                        }
                    }
                    _ => unreachable!(),
                };
                let self_ty = self.ast_ty_to_ty(&AstTy::named(enum_name));
                let cases = self.ast_ctx.types[enum_name].cases.clone();
                let pos = cases.iter().position(|x| &x.cons_name == cons).unwrap();
                let marker = if self_ty.is_ptr() {
                    let v = b.load(&mut self.ctx, v.clone().into(), self_ty.into_inner());
                    b.extract(&mut self.ctx, v.into(), 0)
                } else {
                    b.extract(&mut self.ctx, v.clone().into(), 0)
                };
                let val = b.eq(&mut self.ctx, Const::Int(pos as i32).into(), marker.into());
                arg.iter()
                    .fold((val, HashMap::new()), |(val, mut bindings), x| {
                        let arg_ty = cases[pos].arg.as_ref().unwrap().clone();
                        let v = if ty.is_recursive(&self.ast_ctx) {
                            let ptr = b.get_element_ptr(
                                &mut self.ctx,
                                v.clone().into(),
                                self_ty.into_inner().clone(),
                                1,
                            );
                            let ptr = b.get_element_ptr(
                                &mut self.ctx,
                                ptr.into(),
                                self_ty.into_inner().field(1),
                                pos,
                            );
                            b.load(
                                &mut self.ctx,
                                ptr.into(),
                                self_ty.into_inner().field(1).field(pos),
                            )
                        } else {
                            let arg_ty = cases[pos].arg.as_ref().unwrap().clone();
                            let v = b.extract(&mut self.ctx, v.clone().into(), 1);
                            b.extract(&mut self.ctx, v.into(), pos)
                        };
                        let (new_val, new_bindings) = self.matches(v, &arg_ty, x, b);
                        bindings.extend(new_bindings);
                        let mat = b.mul(&mut self.ctx, new_val, val);
                        (mat, bindings)
                    })
            }
            Pattern::Tuple(patterns) => match ty {
                AstTy::Tuple(items) => {
                    assert_eq!(items.len(), patterns.len());
                    items.iter().zip(patterns.iter()).enumerate().fold(
                        (Value::Const(Const::Int(1)), HashMap::new()),
                        |(val, mut bindings), (i, (ty, pat))| {
                            let v = b.extract(&mut self.ctx, v.clone().into(), i);
                            let (this, new_bindings) = self.matches(v, ty, pat, b);
                            let new_val = b.mul(&mut self.ctx, this, val);
                            bindings.extend(new_bindings);
                            (new_val, bindings)
                        },
                    )
                }
                _ => unreachable!("Not a tuple"),
            },
        }
    }

    fn compile_match<T: Clone>(
        &mut self,
        expr: &Ast<T>,
        cases: &[MatchCase<T>],
        b: &mut Builder,
        target_ty: Ty,
    ) -> Value {
        let ty = self.get_ast_ty_of(expr);
        let val = self.aux(expr, b);
        let var = b.make_var(&mut self.ctx, &val);
        let merge_lbl = Label::fresh();
        let merge_use = Use::from(&merge_lbl);
        let cfg_ty = self.ast_ty_to_ty(&ty);
        let mut vals = vec![];

        let res = if !target_ty.is_zero_sized() {
            Some(b.alloca(&mut self.ctx, target_ty.clone()))
        } else {
            None
        };

        for case in cases {
            let (mat, bindings) = self.matches(var.clone(), &ty, &case.pat, b);

            for (var, cfg) in bindings {
                let ty = cfg.get_type(&self.ctx);
                self.map.insert(var, cfg);
                self.type_map.insert(var, ty);
            }

            let case_body_lbl = Label::fresh();
            let case_body_use = Use::from(&case_body_lbl);
            let next_check_lbl = Label::fresh();
            let next_check_use = Use::from(&next_check_lbl);
            b.branch(
                &self.ctx,
                mat,
                case_body_use,
                next_check_use.clone(),
                case_body_lbl,
            );
            self.drop_ty(var.clone().into(), &ty, b);
            // if cfg_ty.is_ptr() {
            //     self.inject_print(format!("Dropping {}:{}\n", file!(), line!()), b);
            //     let drop_object = Const::FunPtr(self.ctx.natives["drop_object"].clone());
            //     b.native_call(&mut self.ctx, drop_object.into(), vec![var.clone().into()]);
            // }
            let this_res = self.aux(&case.expr, b);
            if !target_ty.is_zero_sized() {
                let cast = b.cast(&mut self.ctx, &this_res, target_ty.clone());
                vals.push(cast);
            }
            if let Some(res) = &res {
                b.store(&self.ctx, res.clone().into(), this_res);
            }
            b.goto(&self.ctx, merge_use.clone(), next_check_lbl);
        }

        b.goto(&self.ctx, merge_use, merge_lbl);
        res.map_or(Const::Struct(vec![]).into(), |x| {
            b.load(&mut self.ctx, x.into(), target_ty).into()
        })
    }
}
