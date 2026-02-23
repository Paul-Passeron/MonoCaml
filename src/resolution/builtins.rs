use crate::{
    intern_symbol,
    poly_ir::{TypeId, ValueRef, item::TypeDeclInfo},
    resolution::{Resolver, VarInfo},
};

impl Resolver {
    pub(super) fn add_builtins(&mut self) {
        self.add_builtin_types();
        self.add_builtin_values();
    }

    fn add_builtin_types(&mut self) {
        let _int_ty = self.add_builtin_type("int", 0);
        let _bool_ty = self.add_builtin_type("bool", 0);
        let _string_ty = self.add_builtin_type("string", 0);

        let list_ty = self.add_builtin_type("list", 1);
        self.add_builtin_constructor("[]", list_ty, 0);
        self.add_builtin_constructor("::", list_ty, 1);

        let option_ty = self.add_builtin_type("option", 1);
        self.add_builtin_constructor("None", option_ty, 0);
        self.add_builtin_constructor("Some", option_ty, 1);
    }

    fn add_builtin_value(&mut self, name: &str) {
        let sym = intern_symbol(name);
        let id = self.vars.alloc(VarInfo { name: sym });
        self.scope.bind_value(sym, ValueRef::Local(id));
    }

    fn add_builtin_values(&mut self) {
        self.add_builtin_value("print_endline");
    }

    fn add_builtin_type(&mut self, name: &str, arity: usize) -> TypeId {
        let sym = intern_symbol(name);
        let id = self.types.alloc(TypeDeclInfo { name: sym, arity });
        self.scope.bind_type(sym, id);
        id
    }

    fn add_builtin_constructor(&mut self, name: &str, type_id: TypeId, index: u32) {
        let sym = intern_symbol(name);
        self.scope
            .bind_value(sym, ValueRef::Constructor { type_id, index });
    }
}
