use crate::{
    lexer::interner::Symbol,
    parse_tree::{Located, type_expr::TypeExpr},
    source_manager::loc::Loc,
};

#[derive(Debug)]
pub struct TypeDeclaration {
    pub name: Located<Symbol>,
    pub params: Vec<TypeExpr>,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Variant(Vec<ConstructorDeclaration>),
    Record(Vec<LabelDeclaration>),
}

#[derive(Debug)]
pub struct ConstructorDeclaration {
    pub name: Located<Symbol>,
    pub vars: Vec<Symbol>,
    pub args: ConstructorArguments,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum ConstructorArguments {
    Tuple(Vec<TypeExpr>),
    Record(Vec<LabelDeclaration>),
}

#[derive(Debug)]
pub enum MutableFlag {
    Mut,
    NonMut,
}

#[derive(Debug)]
pub struct LabelDeclaration {
    pub name: Located<Symbol>,
    pub mutable: MutableFlag,
    pub typ: TypeExpr,
    pub loc: Loc,
}
