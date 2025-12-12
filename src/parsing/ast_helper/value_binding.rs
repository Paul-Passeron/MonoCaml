use crate::parsing::{
    ast_helper::get_default_loc,
    docstring::{get_empty_docs, DocString, Docs},
    location::Location,
    parsetree::{Attributes, Expression, Pattern, ValueBinding, ValueConstraint},
};

impl ValueBinding {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        text: Option<Vec<DocString>>,
        constraint: Option<ValueConstraint>,
        pat: Pattern,
        expr: Expression,
    ) -> Self {
        let mut attrs = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs).add_attrs(&mut attrs);
        DocString::add_attrs(text.unwrap_or_default(), &mut attrs);

        Self {
            attributes: attrs,
            loc: loc.unwrap_or_else(get_default_loc),
            pat,
            expr,
            constraint,
        }
    }
}
