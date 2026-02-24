use std::collections::{HashMap, HashSet};

use crate::{
    inference::{
        InferenceCtx,
        solved_ty::{MonoTy, TyForall, TyVar},
    },
    intern_symbol,
    lexer::interner::Symbol,
};

impl<'a> InferenceCtx<'a> {
    pub fn init_builtins(&mut self) {
        // useful types
        let int_ty = self.get_ty(MonoTy::int_ty(self));
        let string_ty = self.get_ty(MonoTy::string_ty(self));
        let unit_ty = self.get_ty(MonoTy::unit_ty(self));
        let string_unit = MonoTy::func_ty(string_ty, unit_ty, self);
        let int_unit = MonoTy::func_ty(int_ty, unit_ty, self);

        let schemes: HashMap<Symbol, TyForall> = HashMap::from_iter(vec![
            (
                intern_symbol("print_endline"),
                TyForall::mono(string_unit.clone()),
            ),
            (
                intern_symbol("print_string"),
                TyForall::mono(string_unit.clone()),
            ),
            (intern_symbol("print_int"), TyForall::mono(int_unit.clone())),
            (intern_symbol("failwith"), {
                let tick_a = TyVar { id: self.fresh() };
                let func_ty = MonoTy::func_ty(string_ty, self.get_ty(tick_a.into()), self);
                TyForall::new(vec![tick_a], func_ty)
            }),
        ]);

        let schemes_symbs = schemes.keys().copied().collect::<HashSet<_>>();
        let builtins_symbs = self.builtins.keys().copied().collect::<HashSet<_>>();
        assert_eq!(schemes_symbs, builtins_symbs);

        for (symb, scheme) in schemes {
            let id = self.builtins.get(&symb).unwrap();
            self.map.insert(*id, scheme);
        }
    }
}
