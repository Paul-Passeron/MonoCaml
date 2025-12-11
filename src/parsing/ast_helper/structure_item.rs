use crate::parsing::{
    ast_helper::get_default_loc,
    location::Location,
    parsetree::{Attributes, Expression, PatternDesc, StructureItem, StructureItemDesc},
};

impl StructureItem {
    pub fn mk(loc: Option<Location>, d: StructureItemDesc) -> Self {
        Self {
            desc: d,
            loc: loc.unwrap_or(get_default_loc()),
        }
    }

    pub fn eval(loc: Option<Location>, attrs: Option<Attributes>, e: Expression) -> Self {
        Self::mk(
            loc,
            StructureItemDesc::Eval(Box::new(e), attrs.unwrap_or_default()),
        )
    }
}
