use crate::{
    helpers::unique::Token,
    resolved::poly_ir::types::{TypeDef, TypeDefId},
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ValueId(u32);

pub struct ValueDef {}

pub struct DefTable {
    values: Vec<ValueDef>,
    types: Vec<TypeDef>,
}

impl DefTable {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn add_value(&mut self, v: ValueDef) -> ValueId {
        let id = ValueId(self.values.len() as u32);
        self.values.push(v);
        id
    }

    pub fn add_type(&mut self, t: TypeDef) -> TypeDefId {
        let id = TypeDefId::from(Token::from(self.types.len()));
        self.types.push(t);
        id
    }

    pub fn get_value(&self, id: ValueId) -> &ValueDef {
        self.values.get(id.0 as usize).unwrap()
    }

    pub fn get_type(&self, id: TypeDefId) -> &TypeDef {
        self.types.get(id.extract()).unwrap()
    }
}
