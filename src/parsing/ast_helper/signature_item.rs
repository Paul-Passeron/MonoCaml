use crate::parsing::{
    ast_helper::get_default_loc,
    docstring::DocString,
    location::Location,
    parsetree::{
        Attribute, Attributes, ClassDescription, ClassTypeDeclaration, Extension,
        IncludeDescription, ModuleDeclaration, ModuleSubstitution, ModuleTypeDeclaration,
        OpenDescription, SignatureItem, SignatureItemDesc, TypeDeclaration, TypeException,
        TypeExtension, ValueDescription,
    },
};

impl SignatureItem {
    pub fn mk(loc: Option<Location>, desc: SignatureItemDesc) -> Self {
        Self {
            desc,
            loc: loc.unwrap_or(get_default_loc()),
        }
    }

    pub fn value(loc: Option<Location>, v: ValueDescription) -> Self {
        Self::mk(loc, SignatureItemDesc::Value(v))
    }

    pub fn type_subst(loc: Option<Location>, lst: Vec<TypeDeclaration>) -> Self {
        Self::mk(loc, SignatureItemDesc::TypeSubst(lst))
    }

    pub fn type_extension(loc: Option<Location>, ext: TypeExtension) -> Self {
        Self::mk(loc, SignatureItemDesc::TypeExt(ext))
    }

    pub fn exception(loc: Option<Location>, exc: TypeException) -> Self {
        Self::mk(loc, SignatureItemDesc::Exception(exc))
    }

    pub fn module(loc: Option<Location>, m: ModuleDeclaration) -> Self {
        Self::mk(loc, SignatureItemDesc::Module(m))
    }

    pub fn mod_subst(loc: Option<Location>, m: ModuleSubstitution) -> Self {
        Self::mk(loc, SignatureItemDesc::ModSubst(m))
    }

    pub fn rec_module(loc: Option<Location>, m: Vec<ModuleDeclaration>) -> Self {
        Self::mk(loc, SignatureItemDesc::RecModule(m))
    }

    pub fn modtype(loc: Option<Location>, m: ModuleTypeDeclaration) -> Self {
        Self::mk(loc, SignatureItemDesc::ModTypeSubst(m))
    }

    pub fn modtype_subst(loc: Option<Location>, m: ModuleTypeDeclaration) -> Self {
        Self::mk(loc, SignatureItemDesc::ModTypeSubst(m))
    }

    pub fn open(loc: Option<Location>, d: OpenDescription) -> Self {
        Self::mk(loc, SignatureItemDesc::Open(d))
    }

    pub fn include(loc: Option<Location>, d: IncludeDescription) -> Self {
        Self::mk(loc, SignatureItemDesc::Include(d))
    }

    pub fn class(loc: Option<Location>, cl: Vec<ClassDescription>) -> Self {
        Self::mk(loc, SignatureItemDesc::Class(cl))
    }

    pub fn class_type(loc: Option<Location>, ct: Vec<ClassTypeDeclaration>) -> Self {
        Self::mk(loc, SignatureItemDesc::ClassType(ct))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, ext: Extension) -> Self {
        Self::mk(
            loc,
            SignatureItemDesc::Extension(ext, attrs.unwrap_or_default()),
        )
    }

    pub fn attribute(loc: Option<Location>, a: Attribute) -> Self {
        Self::mk(loc, SignatureItemDesc::Attribute(a))
    }

    pub fn text(txt: Vec<DocString>) -> Vec<Self> {
        txt.into_iter()
            .filter(|ds| !ds.body.is_empty())
            .map(|ds| Self::attribute(Some(ds.loc.clone()), ds.text_attr()))
            .collect()
    }
}
