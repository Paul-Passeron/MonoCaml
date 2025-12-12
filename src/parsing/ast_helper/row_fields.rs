use crate::parsing::{
    ast_helper::{get_default_loc, Str},
    location::Location,
    parsetree::{Attributes, CoreType, RowField, RowFieldDesc},
};

impl RowField {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, desc: RowFieldDesc) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let attributes = attrs.unwrap_or_default();
        Self {
            loc,
            attributes,
            desc,
        }
    }

    pub fn tag(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        label: Str,
        cons: bool,
        tys: Vec<CoreType>,
    ) -> Self {
        Self::mk(loc, attrs, RowFieldDesc::Tag(label, cons, tys))
    }

    pub fn inherit(loc: Option<Location>, ty: CoreType) -> Self {
        Self::mk(loc, None, RowFieldDesc::Inherit(ty))
    }
}
