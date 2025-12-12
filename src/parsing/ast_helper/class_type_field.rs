use crate::parsing::{
    ast_helper::{get_default_loc, Str},
    asttypes::{MutableFlag, PrivateFlag, VirtualFlag},
    docstring::DocString,
    location::Location,
    parsetree::{
        Attribute, Attributes, ClassType, ClassTypeField, ClassTypeFieldDesc, CoreType, Extension,
    },
};

impl ClassTypeField {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, d: ClassTypeFieldDesc) -> Self {
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

    pub fn inherit(loc: Option<Location>, attrs: Option<Attributes>, t: ClassType) -> Self {
        Self::mk(loc, attrs, ClassTypeFieldDesc::Inherit(t))
    }

    pub fn val(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        lbl: Str,
        mut_flag: MutableFlag,
        vir_flag: VirtualFlag,
        ty: CoreType,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ClassTypeFieldDesc::Val(lbl, mut_flag, vir_flag, ty),
        )
    }

    pub fn method(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        lbl: Str,
        priv_flag: PrivateFlag,
        vir_flag: VirtualFlag,
        ty: CoreType,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ClassTypeFieldDesc::Method(lbl, priv_flag, vir_flag, ty),
        )
    }

    pub fn constraint(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        t1: CoreType,
        t2: CoreType,
    ) -> Self {
        Self::mk(loc, attrs, ClassTypeFieldDesc::Constraint(t1, t2))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, ext: Extension) -> Self {
        Self::mk(loc, attrs, ClassTypeFieldDesc::Extension(ext))
    }

    pub fn attribute(loc: Option<Location>, attr: Attribute) -> Self {
        Self::mk(loc, None, ClassTypeFieldDesc::Attribute(attr))
    }

    pub fn text(txt: Vec<DocString>) -> Vec<Self> {
        txt.into_iter()
            .filter(|ds| !ds.body.is_empty())
            .map(|ds| Self::attribute(Some(ds.loc.clone()), ds.text_attr()))
            .collect()
    }
}
