use crate::parse_tree::{
    Located,
    expression::{Expression, RecFlag, ValueBinding},
    type_declaration::TypeDeclaration,
};

pub type Structure = Vec<StructureItem>;

pub type StructureItem = Located<StructureItemDesc>;

#[derive(Debug)]
pub enum StructureItemDesc {
    Eval(Expression),
    Value(RecFlag, Vec<ValueBinding>),
    Type(Vec<TypeDeclaration>),
}
