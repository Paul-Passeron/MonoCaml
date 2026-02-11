use crate::{poly_ir::TypeId, resolution::Resolver};

impl Resolver {
    pub fn add_builtins(&mut self) {
        let int_ty = self.add_builtin_type("int", 0);
    }

    fn add_builtin_type(&mut self, name: &str, arity: usize) -> TypeId {
        todo!()
    }
}
