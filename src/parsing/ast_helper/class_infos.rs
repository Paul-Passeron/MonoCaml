use crate::parsing::{
    ast_helper::{get_default_loc, Str},
    asttypes::{Injectivity, Variance, VirtualFlag},
    docstring::{get_empty_docs, DocString, DocStringImpl, Docs},
    location::Location,
    parsetree::{Attributes, ClassInfos, CoreType},
};

impl<T> ClassInfos<T> {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        text: Option<Vec<DocString>>,
        virt: Option<VirtualFlag>,
        params: Option<Vec<(CoreType, (Variance, Injectivity))>>,
        name: Str,
        expr: T,
    ) -> Self {
        let mut attrs = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs).add_attrs(&mut attrs);
        DocStringImpl::add_attrs(text.unwrap_or_default(), &mut attrs);
        Self {
            expr,
            params: params.unwrap_or_default(),
            attributes: attrs,
            loc: loc.unwrap_or_else(get_default_loc),
            name,
            virt: virt.unwrap_or(VirtualFlag::Concrete),
        }
    }
}
