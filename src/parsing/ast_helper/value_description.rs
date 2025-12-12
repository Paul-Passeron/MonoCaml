use crate::parsing::{
    location::Location,
    parsetree::{Attributes, ValueDescription},
};

impl ValueDescription {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>) -> Self {
        Self {
            name: todo!(),
            ty: todo!(),
            prim: todo!(),
            attributes: todo!(),
            loc: todo!(),
        }
    }
}
