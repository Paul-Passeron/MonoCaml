use crate::parsing::{
    ast_helper::{get_default_loc, LId},
    asttypes::{Injectivity, PrivateFlag, Variance},
    docstring::{get_empty_docs, Docs},
    location::Location,
    parsetree::{Attributes, CoreType, ExtensionConstructor, TypeExtension},
};

impl TypeExtension {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        params: Option<Vec<(CoreType, (Variance, Injectivity))>>,
        priv_flag: Option<PrivateFlag>,
        path: LId,
        constructors: Vec<ExtensionConstructor>,
    ) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let mut attributes = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs)
            .add_attrs(&mut attributes);
        let params = params.unwrap_or_default();
        let private = priv_flag.unwrap_or(PrivateFlag::Public);
        Self {
            path,
            params,
            constructors,
            private,
            attributes,
            loc,
        }
    }
}
