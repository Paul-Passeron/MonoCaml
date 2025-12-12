use crate::parsing::{
    ast_helper::get_default_loc,
    asttypes::RecFlag,
    docstring::DocString,
    location::Location,
    parsetree::{
        Attribute, Attributes, ClassDeclaration, ClassTypeDeclaration, Expression, Extension,
        IncludeDeclaration, ModuleBinding, ModuleTypeDeclaration, OpenDeclaration, StructureItem,
        StructureItemDesc, TypeDeclaration, TypeException, TypeExtension, ValueBinding,
        ValueDescription,
    },
};

impl StructureItem {
    pub fn mk(loc: Option<Location>, d: StructureItemDesc) -> Self {
        Self {
            desc: d,
            loc: loc.unwrap_or(get_default_loc()),
        }
    }

    pub fn eval(loc: Option<Location>, attrs: Option<Attributes>, e: Expression) -> Self {
        Self::mk(
            loc,
            StructureItemDesc::Eval(Box::new(e), attrs.unwrap_or_default()),
        )
    }

    pub fn value(loc: Option<Location>, rec_flag: RecFlag, bindings: Vec<ValueBinding>) -> Self {
        Self::mk(loc, StructureItemDesc::Value(rec_flag, bindings))
    }

    pub fn primitive(loc: Option<Location>, v: ValueDescription) -> Self {
        Self::mk(loc, StructureItemDesc::Primitive(v))
    }

    pub fn type_(loc: Option<Location>, rec_flag: RecFlag, lst: Vec<TypeDeclaration>) -> Self {
        Self::mk(loc, StructureItemDesc::Type(rec_flag, lst))
    }

    pub fn type_extension(loc: Option<Location>, t: TypeExtension) -> Self {
        Self::mk(loc, StructureItemDesc::TypeExt(t))
    }

    pub fn exception(loc: Option<Location>, t: TypeException) -> Self {
        Self::mk(loc, StructureItemDesc::Exception(t))
    }

    pub fn module(loc: Option<Location>, m: ModuleBinding) -> Self {
        Self::mk(loc, StructureItemDesc::Module(m))
    }

    pub fn rec_module(loc: Option<Location>, m: Vec<ModuleBinding>) -> Self {
        Self::mk(loc, StructureItemDesc::RecModule(m))
    }

    pub fn module_type(loc: Option<Location>, m: ModuleTypeDeclaration) -> Self {
        Self::mk(loc, StructureItemDesc::ModType(Box::new(m)))
    }

    pub fn open(loc: Option<Location>, d: OpenDeclaration) -> Self {
        Self::mk(loc, StructureItemDesc::Open(d))
    }

    pub fn class(loc: Option<Location>, cl: Vec<ClassDeclaration>) -> Self {
        Self::mk(loc, StructureItemDesc::Class(cl))
    }

    pub fn class_type(loc: Option<Location>, cl: Vec<ClassTypeDeclaration>) -> Self {
        Self::mk(loc, StructureItemDesc::ClassType(cl))
    }

    pub fn include(loc: Option<Location>, d: IncludeDeclaration) -> Self {
        Self::mk(loc, StructureItemDesc::Include(d))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, e: Extension) -> Self {
        Self::mk(
            loc,
            StructureItemDesc::Extension(Box::new(e), attrs.unwrap_or_default()),
        )
    }

    pub fn attribute(loc: Option<Location>, a: Attribute) -> Self {
        Self::mk(loc, StructureItemDesc::Attribute(a))
    }

    pub fn text(txt: Vec<DocString>) -> Vec<Self> {
        txt.into_iter()
            .filter(|ds| !ds.body.is_empty())
            .map(|ds| Self::attribute(Some(ds.loc.clone()), ds.text_attr()))
            .collect()
    }
}
