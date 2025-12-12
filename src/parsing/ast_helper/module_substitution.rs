use crate::parsing::{
    ast_helper::{get_default_loc, LId, Str},
    docstring::{get_empty_docs, DocString, Docs},
    location::Location,
    parsetree::{Attributes, ModuleSubstitution},
};

impl ModuleSubstitution {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        text: Option<Vec<DocString>>,
        name: Str,
        syn: LId,
    ) -> Self {
        let mut attrs = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs).add_attrs(&mut attrs);
        DocString::add_attrs(text.unwrap_or_default(), &mut attrs);
        Self {
            name,
            manifest: syn,
            attributes: attrs,
            loc: loc.unwrap_or_else(get_default_loc),
        }
    }
}
