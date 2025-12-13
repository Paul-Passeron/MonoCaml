use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{
    lexing::Position,
    parsing::{
        asttypes::Loc,
        location::Location,
        parsetree::{Attribute, Attributes, Constant, Expression, Payload, StructureItem},
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
pub struct DocStringImpl {
    pub body: String,
    pub loc: Location,
    pub attached: DSAttached,
    pub associated: DSAssociated,
}

pub type DocString = Arc<DocStringImpl>;

pub type Text = Vec<DocString>;

lazy_static! {
    static ref DOCSTRINGS: Mutex<Vec<DocString>> = Mutex::new(Vec::new());
    static ref TEXT_LOC: Loc<String> = {
        Loc {
            txt: format!("monocaml.text"),
            loc: Location::none(),
        }
    };
}

impl DocStringImpl {
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

    pub fn add_attrs(lst: Vec<DocString>, attrs: &mut Attributes) {
        lst.into_iter()
            .filter_map(|x| (!x.body.is_empty()).then_some(x.docs_attr()))
            .for_each(|x| attrs.push(x));
    }

    pub fn register(self) {
        DOCSTRINGS.lock().unwrap().push(Arc::new(self));
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
            Some(ds) if ds.body.is_empty() => {
                vec![]
            }
            Some(ds) => vec![ds.docs_attr()],
        };

        match &self.docs_post {
            Some(ds) if ds.body.is_empty() => (),
            Some(ds) => new_attrs.push(ds.docs_attr()),
            _ => (),
        };

        for attr in new_attrs {
            attrs.push(attr);
        }
    }
}

pub type Info = Option<DocString>;

lazy_static! {
    static ref EMPTY_INFO: Info = None;
}

pub fn add_info_attrs(info: Info, attrs: &mut Attributes) {
    if let Some(info) = info {
        attrs.push(info.docs_attr());
    }
}

pub fn get_empty_info() -> Info {
    EMPTY_INFO.clone()
}

lazy_static! {
    static ref FLOATING_TABLE: Mutex<HashMap<Position, Vec<DocString>>> =
        Mutex::new(HashMap::new());
    static ref POST_TABLE: Mutex<HashMap<Position, Vec<DocString>>> = Mutex::new(HashMap::new());
    static ref PRE_EXTRA_TABLE: Mutex<HashMap<Position, Vec<DocString>>> =
        Mutex::new(HashMap::new());
    static ref POST_EXTRA_TABLE: Mutex<HashMap<Position, Vec<DocString>>> =
        Mutex::new(HashMap::new());
}

pub fn set_floating_docstrings(pos: Position, dsl: Vec<DocString>) {
    if !dsl.is_empty() {
        FLOATING_TABLE.lock().unwrap().insert(pos, dsl);
    }
}

pub fn get_text(pos: &Position) -> Vec<DocString> {
    FLOATING_TABLE
        .lock()
        .unwrap()
        .get(&pos)
        .cloned()
        .unwrap_or_default()
}

pub fn get_post_text(pos: &Position) -> Vec<DocString> {
    POST_TABLE
        .lock()
        .unwrap()
        .get(&pos)
        .cloned()
        .unwrap_or_default()
}

pub fn set_post_docstrings(pos: Position, dsl: Vec<DocString>) {
    if !dsl.is_empty() {
        POST_TABLE.lock().unwrap().insert(pos, dsl);
    }
}

pub fn associate_docstrings(dsl: &mut Text) {
    dsl.iter_mut().for_each(|ds| match ds.associated.clone() {
        DSAssociated::Zero => Arc::get_mut(ds).unwrap().associated = DSAssociated::One,
        DSAssociated::One | DSAssociated::Many => {
            Arc::get_mut(ds).unwrap().associated = DSAssociated::Many
        }
    })
}

pub fn get_docstring(info: bool, dsl: &mut Vec<DocString>) -> Option<DocString> {
    for ds in dsl.iter_mut().filter(|ds| ds.attached != DSAttached::Info) {
        Arc::get_mut(ds).unwrap().attached = if info {
            DSAttached::Info
        } else {
            DSAttached::Docs
        };
        return Some(ds.clone());
    }
    None
}

pub fn get_post_docs(pos: &Position) -> Option<DocString> {
    if let Some(dsl) = POST_TABLE.lock().unwrap().get_mut(pos) {
        associate_docstrings(dsl);
        get_docstring(false, dsl)
    } else {
        None
    }
}

pub fn set_pre_extra_docstrings(pos: Position, dsl: Vec<DocString>) {
    if !dsl.is_empty() {
        PRE_EXTRA_TABLE.lock().unwrap().insert(pos, dsl);
    }
}

pub fn get_pre_extra_text(pos: &Position) -> Vec<DocString> {
    PRE_EXTRA_TABLE
        .lock()
        .unwrap()
        .get(&pos)
        .cloned()
        .unwrap_or_default()
}

pub fn set_post_extra_docstrings(pos: Position, dsl: Vec<DocString>) {
    if !dsl.is_empty() {
        POST_EXTRA_TABLE.lock().unwrap().insert(pos, dsl);
    }
}

pub fn get_post_extra_text(pos: &Position) -> Vec<DocString> {
    POST_EXTRA_TABLE
        .lock()
        .unwrap()
        .get(&pos)
        .cloned()
        .unwrap_or_default()
}

pub fn rhs_text(pos: &Position) -> Vec<DocString> {
    get_text(pos)
}

pub fn rhs_post_text(pos: &Position) -> Vec<DocString> {
    get_post_text(pos)
}

pub fn rhs_pre_extra_text(pos: &Position) -> Vec<DocString> {
    get_pre_extra_text(pos)
}

pub fn rhs_post_extra_text(pos: &Position) -> Vec<DocString> {
    get_post_extra_text(pos)
}

pub fn get_info(pos: &Position) -> Option<DocString> {
    let mut mutex = POST_TABLE.lock().ok()?;
    let dsl = mutex.get_mut(pos)?;
    get_docstring(true, dsl)
}

pub fn symbol_info(endpos: &Position) -> Option<DocString> {
    get_info(endpos)
}

pub fn rhs_info(endpos: &Position) -> Option<DocString> {
    get_info(endpos)
}

pub fn symbol_text(endpos: &Position) -> Vec<DocString> {
    get_text(endpos)
}
