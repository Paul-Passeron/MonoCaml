use crate::parsing::{
    asttypes::Loc,
    location::Location,
    parsetree::{Attribute, Constant, Expression, Payload, StructureItem, StructureItemDesc},
};
use lazy_static::lazy_static;

pub enum DSAttached {
    Unattached,
    Info,
    Docs,
}

pub enum DSAssociated {
    Zero,
    One,
    Many,
}

pub struct DocString<'a> {
    pub body: &'a str,
    pub loc: Location,
    pub attached: DSAttached,
    pub associated: DSAssociated,
}

lazy_static! {
    static ref TEXT_LOC: Loc<String> = {
        Loc {
            txt: format!("ocaml.text"),
            loc: Location::none(),
        }
    };
}

impl DocString<'_> {
    pub fn text_attr(&self) -> Attribute {
        let loc = self.loc.clone();
        let c = Constant::string(None, Some(loc.clone()), self.body.into());
        let exp = Expression::constant(Some(loc.clone()), None, c);
        let item = StructureItem {
            desc: StructureItemDesc::Eval(Box::new(exp), vec![]),
            loc: loc.clone(),
        };

        Attribute::mk(Some(loc), TEXT_LOC.clone(), Payload::Str(vec![item]))
    }
}
