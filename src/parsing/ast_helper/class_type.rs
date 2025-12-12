use crate::parsing::{
    ast_helper::{get_default_loc, LId},
    asttypes::ArgLabel,
    location::Location,
    parsetree::{
        Attribute, Attributes, ClassSignature, ClassType, ClassTypeDesc, CoreType, Extension,
        OpenDescription,
    },
};

impl ClassType {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, d: ClassTypeDesc) -> Self {
        Self {
            desc: d,
            loc: loc.unwrap_or_else(get_default_loc),
            attributes: attrs.unwrap_or_default(),
        }
    }

    pub fn attr(self, attr: Attribute) -> Self {
        let mut this = self;
        this.attributes.push(attr);
        this
    }

    pub fn constr(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        id: LId,
        tys: Vec<CoreType>,
    ) -> Self {
        Self::mk(loc, attrs, ClassTypeDesc::Constr(id, tys))
    }

    pub fn signature(loc: Option<Location>, attrs: Option<Attributes>, s: ClassSignature) -> Self {
        Self::mk(loc, attrs, ClassTypeDesc::Signature(s))
    }

    pub fn arrow(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        lbl: ArgLabel,
        from: CoreType,
        to: Self,
    ) -> Self {
        Self::mk(loc, attrs, ClassTypeDesc::Arrow(lbl, from, Box::new(to)))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, ext: Extension) -> Self {
        Self::mk(loc, attrs, ClassTypeDesc::Extension(ext))
    }

    pub fn open(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        d: OpenDescription,
        c: Self,
    ) -> Self {
        Self::mk(loc, attrs, ClassTypeDesc::Open(d, Box::new(c)))
    }
}
