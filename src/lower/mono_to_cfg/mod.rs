use std::collections::HashMap;

use crate::{
    cfg::{FunName, FunNameUse, Func, Program, Sig, Ty, TyCtx, Value, builder::Builder},
    helpers::unique::Use,
    monomorph::ConcrTy,
    poly_ir::{
        VarId,
        id::Arena,
        item::{Item, ItemNode, TypeDeclInfo},
    },
    resolution::VarInfo,
};

pub mod expr;
pub mod item;

#[allow(unused)]
pub struct MonoToCfg<'a> {
    ctx: TyCtx,
    program: Program,
    vars: HashMap<VarId, Value>,
    builtins: HashMap<VarId, FunNameUse>,
    var_arena: &'a Arena<VarInfo>,
    tys: &'a Arena<TypeDeclInfo>,
}

impl<'a> MonoToCfg<'a> {
    pub fn new(
        entry: FunNameUse,
        builtins: &HashMap<VarId, ConcrTy>,
        var_arena: &'a Arena<VarInfo>,
        tys: &'a Arena<TypeDeclInfo>,
    ) -> Self {
        let mut this = Self {
            ctx: TyCtx::new(),
            program: Program::new(entry),
            vars: HashMap::new(),
            builtins: HashMap::new(),
            var_arena,
            tys,
        };
        this.init_builtins(builtins);
        this
    }

    fn init_builtins(&mut self, builtins: &HashMap<VarId, ConcrTy>) {
        builtins.iter().for_each(|(id, ty)| {
            let name = FunName::fresh();
            let name_use = Use::from(&name);
            let sig = self.ty_as_simple_sig(ty);
            let func = Func::new(
                name,
                self.ctx.make_params(sig.params().iter().cloned()),
                sig.ret().clone(),
                None,
            );
            self.ctx.sigs.insert(name_use, sig);
            self.program.add_func(func);
            self.builtins.insert(*id, name_use);

            for (id, key) in &self.builtins {
                self.program
                    .add_native_alias(self.var_arena[*id].name.to_string(), *key);
            }
        });
    }

    fn get_ty(&self, ty: &ConcrTy) -> Ty {
        let type_name = self.tys[ty.id].name.to_string();
        if type_name == "int" {
            Ty::Int
        } else if type_name == "string" {
            Ty::String
        } else if type_name == "unit" {
            Ty::Void
        } else {
            todo!("{type_name}")
        }
    }

    fn ty_as_simple_sig(&self, ty: &ConcrTy) -> Sig {
        let param = &ty.args[0];
        let ret = &ty.args[1];

        let n_param = self.get_ty(param);
        let n_ret = self.get_ty(ret);

        Sig::new(vec![n_param], n_ret)
    }

    pub fn compile(
        program: &[Item<ConcrTy>],
        builtins: &HashMap<VarId, ConcrTy>,
        var_arena: &'a Arena<VarInfo>,
        tys: &'a Arena<TypeDeclInfo>,
    ) -> Program {
        let entry = FunName::fresh();
        let entry_ref = Use::from(&entry);
        let mut this = Self::new(entry_ref, builtins, var_arena, tys);
        let mut b = Builder::new(entry, vec![], Ty::Void, &mut this.ctx);
        for item in program {
            if let ItemNode::Value {
                recursive,
                bindings,
            } = &item.node
            {
                this.compile_bindings(*recursive, bindings, &mut HashMap::new(), &mut b);
            }
        }
        b.ret_void(&this.ctx);
        let main_function = b.finalize();
        this.program.add_func(main_function);
        this.program
    }
}
