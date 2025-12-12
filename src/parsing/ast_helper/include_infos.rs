use crate::parsing::{
    ast_helper::get_default_loc,
    docstring::{get_empty_docs, Docs},
    location::Location,
    parsetree::{Attributes, IncludeInfos},
};

impl<T> IncludeInfos<T> {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        mexpr: T,
    ) -> Self {
        let mut attrs = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs).add_attrs(&mut attrs);
        Self {
            modd: mexpr,
            attributes: attrs,
            loc: loc.unwrap_or_else(get_default_loc),
        }
    }
}
