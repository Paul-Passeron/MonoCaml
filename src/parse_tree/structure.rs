use crate::{
    lower::mono_to_cfg::RecFlag,
    parse_tree::{
        Located,
        expression::{Expression, ValueBinding},
        type_declaration::TypeDeclaration,
    },
};

pub type Structure = Vec<StructureItem>;

pub type StructureItem = Located<StructureItemDesc>;

pub enum StructureItemDesc {
    Eval(Expression),
    Value(RecFlag, Vec<ValueBinding>),
    Type(Vec<TypeDeclaration>),
}
