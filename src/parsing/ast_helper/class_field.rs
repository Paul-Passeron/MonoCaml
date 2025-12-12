use crate::parsing::{
    ast_helper::{get_default_loc, Str},
    asttypes::{MutableFlag, OverrideFlag, PrivateFlag},
    docstring::DocString,
    location::Location,
    parsetree::{
        Attribute, Attributes, ClassExpr, ClassExprDesc, ClassField, ClassFieldDesc,
        ClassFieldKind, CoreType, Expression, Extension,
    },
};

impl ClassField {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, d: ClassFieldDesc) -> Self {
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

    pub fn inherit(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        ov_flag: OverrideFlag,
        e: ClassExpr,
        lbl: Option<Str>,
    ) -> Self {
        Self::mk(loc, attrs, ClassFieldDesc::Inherit(ov_flag, e, lbl))
    }

    pub fn val(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        lbl: Str,
        mut_flag: MutableFlag,
        kind: ClassFieldKind,
    ) -> Self {
        Self::mk(loc, attrs, ClassFieldDesc::Val(lbl, mut_flag, kind))
    }

    pub fn method(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        lbl: Str,
        priv_flag: PrivateFlag,
        kind: ClassFieldKind,
    ) -> Self {
        Self::mk(loc, attrs, ClassFieldDesc::Method(lbl, priv_flag, kind))
    }

    pub fn constraint(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        t1: CoreType,
        t2: CoreType,
    ) -> Self {
        Self::mk(loc, attrs, ClassFieldDesc::Constraint(t1, t2))
    }

    pub fn initializer(loc: Option<Location>, attrs: Option<Attributes>, e: Expression) -> Self {
        Self::mk(loc, attrs, ClassFieldDesc::Initializer(e))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, e: Extension) -> Self {
        Self::mk(loc, attrs, ClassFieldDesc::Extension(e))
    }

    pub fn attribute(loc: Option<Location>, attr: Attribute) -> Self {
        Self::mk(loc, None, ClassFieldDesc::Attribute(attr))
    }

    pub fn text(txt: Vec<DocString>) -> Vec<Self> {
        txt.into_iter()
            .filter(|ds| !ds.body.is_empty())
            .map(|ds| Self::attribute(Some(ds.loc.clone()), ds.text_attr()))
            .collect()
    }

    pub fn virtual_(ty: CoreType) -> ClassFieldKind {
        ClassFieldKind::Virtual(ty)
    }

    pub fn concrete(o: OverrideFlag, e: Expression) -> ClassFieldKind {
        ClassFieldKind::Concrete(o, e)
    }
}
