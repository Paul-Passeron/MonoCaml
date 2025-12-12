use crate::parsing::{
    ast_helper::get_default_loc,
    asttypes::{Injectivity, Variance, VirtualFlag},
    docstring::{get_empty_docs, DocString, Docs},
    location::Location,
    parsetree::{Attributes, ClassInfos, CoreType, IncludeInfos},
};

impl<T> ClassInfos<T> {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        text: Option<Vec<DocString>>,
        virt: Option<VirtualFlag>,
        params: Option<Vec<(CoreType, (Variance, Injectivity))>>,
        expr: T,
    ) -> Self {
        let mut attrs = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs).add_attrs(&mut attrs);
        DocString::add_attrs(text.unwrap_or_default(), &mut attrs);
        Self {
            expr,
            params: params.unwrap_or_default(),
            attributes: attrs,
            loc: loc.unwrap_or_else(get_default_loc),
            name: todo!(),
            virt: virt.unwrap_or(VirtualFlag::Concrete),
        }
    }
}
