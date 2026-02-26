use crate::{
    intern_symbol,
    poly_ir::{TPMarker, TypeId, TypeParamId, ValueRef, item::TypeDeclInfo, type_expr::Type},
    resolution::{Resolver, VarInfo},
};

pub const NIL_IDX: usize = 0;
pub const CONS_IDX: usize = 1;

impl Resolver {
    pub(super) fn add_builtins(&mut self) {
        self.add_builtin_types();
        self.add_builtin_values();
    }

    fn add_builtin_types(&mut self) {
        let _int_ty = self.add_builtin_type("int", vec![]);
        let _bool_ty = self.add_builtin_type("bool", vec![]);
        let _string_ty = self.add_builtin_type("string", vec![]);
        let _unit_ty = self.add_builtin_type("unit", vec![]);

        let arrow_args = (0..2).map(|_| self.tps.alloc(TPMarker)).collect();
        let _arrow_ty = self.add_builtin_type("->", arrow_args);

        let _tuple_ty = self.add_builtin_type("*", vec![]); // arity is not relevant here

        let list_param = self.tps.alloc(TPMarker);
        let list_ty = self.add_builtin_type("list", vec![list_param]);
        self.add_builtin_constructor("[]", list_ty, None);
        self.add_builtin_constructor(
            "::",
            list_ty,
            Some(Type::Tuple(vec![
                Type::Param(list_param),
                Type::Constr {
                    id: list_ty,
                    args: vec![Type::Param(list_param)],
                },
            ])),
        );
    }

    fn add_builtin_value(&mut self, name: &str) {
        let sym = intern_symbol(name);
        let id = self.vars.alloc(VarInfo { name: sym });
        self.scope.bind_value(sym, ValueRef::Local(id));
        self.builtins.insert(sym, id);
    }

    fn add_builtin_values(&mut self) {
        self.add_builtin_value("print_endline");
        self.add_builtin_value("print_string");
        self.add_builtin_value("print_int");
        self.add_builtin_value("failwith");
    }

    fn add_builtin_type(&mut self, name: &str, params: Vec<TypeParamId>) -> TypeId {
        let sym = intern_symbol(name);
        let id = self.types.alloc(TypeDeclInfo { name: sym, params });
        self.scope.bind_type(sym, id);
        self.builtin_types.insert(name.to_string(), id);
        id
    }

    fn add_builtin_constructor(&mut self, name: &str, type_id: TypeId, arg_ty: Option<Type>) {
        assert!(!arg_ty.iter().any(Type::is_infer));
        let sym = intern_symbol(name);

        self.constructors.entry(type_id).or_default().push(arg_ty);
        let index = self.constructors[&type_id].len() - 1;
        self.scope.bind_value(
            sym,
            ValueRef::Constructor {
                type_id,
                index: index as u32,
            },
        );
    }
}
