use crate::parsing::{
    ast_helper::get_default_loc,
    docstring::{get_empty_docs, Docs},
    location::Location,
    parsetree::{Attributes, ExtensionConstructor, TypeException},
};

impl TypeException {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        constructor: ExtensionConstructor,
    ) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let mut attributes = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs)
            .add_attrs(&mut attributes);
        Self {
            constructor,
            loc,
            attributes,
        }
    }
}
