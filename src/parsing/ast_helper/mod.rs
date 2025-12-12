use std::sync::Mutex;

use lazy_static::lazy_static;

use crate::parsing::{
    asttypes::Loc, location::Location, longident::LongIdent, parsetree::Attribute,
};

use super::parsetree::{ObjectFieldDesc, RowFieldDesc};

type LId = Loc<LongIdent>;
type Str = Loc<String>;
type StrOpt = Loc<Option<String>>;
type Attrs = Vec<Attribute>;

lazy_static! {
    static ref DEFAULT_LOC: Mutex<Location> = { Mutex::new(Location::none()) };
}

pub fn get_default_loc() -> Location {
    DEFAULT_LOC.lock().unwrap().clone()
}

pub fn with_default_loc<F, T>(l: Location, f: F) -> T
where
    F: Fn() -> T,
{
    let old = {
        let mut lock = DEFAULT_LOC.lock().unwrap();
        let old = lock.clone();
        *lock = l;
        old
    };
    let res = f();
    *DEFAULT_LOC.lock().unwrap() = old;
    res
}

pub mod attribute;
pub mod class_expr;
pub mod class_type;
pub mod class_type_field;
pub mod constant;
pub mod core_type;
pub mod expression;
pub mod module_expr;
pub mod module_type;
pub mod pattern;
pub mod signature_item;
pub mod structure_item;
