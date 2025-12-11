use crate::parsing::{
    ast_helper::{get_default_loc, LId},
    location::Location,
    parsetree::{
        Attribute, Attributes, Extension, FunctorParameter, ModuleExpr, ModuleType, ModuleTypeDesc,
        Signature, WithConstraint,
    },
};

impl ModuleType {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, desc: ModuleTypeDesc) -> Self {
        Self {
            desc,
            loc: loc.unwrap_or(get_default_loc()),
            attributes: attrs.unwrap_or_default(),
        }
    }

    pub fn attr(self, attr: Attribute) -> Self {
        let mut this = self;
        this.attributes.push(attr);
        this
    }

    pub fn ident(loc: Option<Location>, attrs: Option<Attributes>, ident: LId) -> Self {
        Self::mk(loc, attrs, ModuleTypeDesc::Ident(ident))
    }

    pub fn alias(loc: Option<Location>, attrs: Option<Attributes>, ident: LId) -> Self {
        Self::mk(loc, attrs, ModuleTypeDesc::Alias(ident))
    }

    pub fn signature(loc: Option<Location>, attrs: Option<Attributes>, sig: Signature) -> Self {
        Self::mk(loc, attrs, ModuleTypeDesc::Signature(sig))
    }

    pub fn functor(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        f_p: FunctorParameter,
        t: Self,
    ) -> Self {
        Self::mk(loc, attrs, ModuleTypeDesc::Functor(f_p, Box::new(t)))
    }

    pub fn with(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        t: Self,
        lst: Vec<WithConstraint>,
    ) -> Self {
        Self::mk(loc, attrs, ModuleTypeDesc::With(Box::new(t), lst))
    }

    pub fn typeof_(loc: Option<Location>, attrs: Option<Attributes>, e: ModuleExpr) -> Self {
        Self::mk(loc, attrs, ModuleTypeDesc::TypeOf(e))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, e: Extension) -> Self {
        Self::mk(loc, attrs, ModuleTypeDesc::Extension(e))
    }
}
