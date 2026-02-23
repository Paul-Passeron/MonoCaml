use id::Id;

use crate::resolution::VarInfo;

pub mod expr;
pub mod id;
pub mod item;
pub mod pattern;
pub mod spanned;
pub mod type_expr;
pub mod typed_node_map;

pub type TypeId = Id<item::TypeDeclInfo>;
pub type VarId = Id<VarInfo>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeParamId {
    pub depth: u32,
    pub index: u32,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueRef {
    Local(VarId),
    Constructor { type_id: TypeId, index: u32 },
}
