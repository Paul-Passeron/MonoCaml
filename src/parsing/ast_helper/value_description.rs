use crate::parsing::{
    ast_helper::{get_default_loc, Str},
    docstring::{get_empty_docs, Docs},
    location::Location,
    parsetree::{Attributes, CoreType, ValueDescription},
};

impl ValueDescription {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        prim: Option<Vec<String>>,
        name: Str,
        ty: CoreType,
    ) -> Self {
        let mut attrs = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs).add_attrs(&mut attrs);
        Self {
            name,
            ty,
            prim: prim.unwrap_or_default(),
            attributes: attrs,
            loc: loc.unwrap_or_else(get_default_loc),
        }
    }
}
