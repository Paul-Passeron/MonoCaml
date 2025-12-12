use std::sync::Mutex;

use crate::parsing::{
    asttypes::Loc,
    location::Location,
    parsetree::{
        Attribute, Attributes, Constant, Expression, Payload, StructureItem, StructureItemDesc,
    },
};
use lazy_static::lazy_static;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DSAttached {
    Unattached,
    Info,
    Docs,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DSAssociated {
    Zero,
    One,
    Many,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

pub fn get_empty_docs() -> Docs {
    EMPTY_DOCS.clone()
}

impl Docs {
    pub fn add_attrs(self, attrs: &mut Attributes) {
        let mut new_attrs = match &self.docs_pre {
            None => vec![],
            Some(DocString { body, .. }) if body.is_empty() => {
                vec![]
            }
            Some(ds) => vec![ds.docs_attr()],
        };

        match &self.docs_post {
            Some(DocString { body, .. }) if body.is_empty() => (),
            Some(ds) => new_attrs.push(ds.docs_attr()),
            _ => (),
        };

        for attr in new_attrs {
            attrs.push(attr);
        }
    }
}
