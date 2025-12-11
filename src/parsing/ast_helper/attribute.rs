use crate::parsing::{
    ast_helper::{get_default_loc, Str},
    location::Location,
    parsetree::{Attribute, Payload},
};

impl Attribute {
    pub fn mk(loc: Option<Location>, name: Str, payload: Payload) -> Attribute {
        Attribute {
            name,
            payload,
            loc: loc.unwrap_or(get_default_loc()),
        }
    }
}
