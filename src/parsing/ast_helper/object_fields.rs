use crate::parsing::{
    ast_helper::{get_default_loc, Str},
    location::Location,
    parsetree::{Attributes, CoreType, ObjectField, ObjectFieldDesc},
};

impl ObjectField {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, desc: ObjectFieldDesc) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let attributes = attrs.unwrap_or_default();
        Self {
            loc,
            attributes,
            desc,
        }
    }

    pub fn tag(loc: Option<Location>, attrs: Option<Attributes>, label: Str, ty: CoreType) -> Self {
        Self::mk(loc, attrs, ObjectFieldDesc::Tag(label, ty))
    }

    pub fn inherit(loc: Option<Location>, ty: CoreType) -> Self {
        Self::mk(loc, None, ObjectFieldDesc::Inherit(ty))
    }
}
