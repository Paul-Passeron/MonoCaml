use std::sync::Mutex;

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

pub struct DocString {
    pub body: String,
    pub loc: Location,
    pub attached: DSAttached,
    pub associated: DSAssociated,
}
lazy_static! {
    static ref DOCSTRINGS: Mutex<Vec<DocString>> = { Mutex::new(Vec::new()) };
    static ref TEXT_LOC: Loc<String> = {
        Loc {
            txt: format!("monocaml.text"),
            loc: Location::none(),
        }
    };
}

impl DocString {
    pub fn text_attr(&self) -> Attribute {
        let loc = self.loc.clone();
        let c = Constant::string(None, Some(loc.clone()), self.body.clone());
        let exp = Expression::constant(Some(loc.clone()), None, c);
        let item = StructureItem::eval(Some(loc.clone()), None, exp);

        Attribute::mk(Some(loc), TEXT_LOC.clone(), Payload::Str(vec![item]))
    }

    pub fn docs_attr(&self) -> Attribute {
        let loc = self.loc.clone();
        let c = Constant::string(None, Some(loc.clone()), self.body.clone());
        let exp = Expression::constant(Some(loc.clone()), None, c);
        let item = StructureItem::eval(Some(loc.clone()), None, exp);
        Attribute::mk(Some(loc), DOC_LOC.clone(), Payload::Str(vec![item]))
    }

    pub fn register(self) {
        DOCSTRINGS.lock().unwrap().push(self);
    }
}

pub struct Docs {
    pub docs_pre: Option<DocString>,
    pub docs_post: Option<DocString>,
}

lazy_static! {
    static ref EMPTY_DOCS: Docs = {
        Docs {
            docs_pre: None,
            docs_post: None,
        }
    };
    static ref DOC_LOC: Loc<String> = {
        Loc {
            txt: format!("monocaml.doc"),
            loc: Location::none(),
        }
    };
}
