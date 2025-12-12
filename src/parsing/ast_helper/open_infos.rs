use crate::parsing::{
    ast_helper::get_default_loc,
    asttypes::OverrideFlag,
    docstring::{get_empty_docs, Docs},
    location::Location,
    parsetree::{Attributes, OpenInfos},
};

impl<T> OpenInfos<T> {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        ov_flag: Option<OverrideFlag>,
        expr: T,
    ) -> Self {
        let mut attrs = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs).add_attrs(&mut attrs);
        Self {
            overr_flag: ov_flag.unwrap_or(OverrideFlag::Fresh),
            expr,
            attributes: attrs,
            loc: loc.unwrap_or_else(get_default_loc),
        }
    }
}
