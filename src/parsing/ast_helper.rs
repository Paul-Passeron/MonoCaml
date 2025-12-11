use std::sync::Mutex;

use crate::{
    parsing::{
        asttypes::Loc,
        location::Location,
        longident::LongIdent,
        parsetree::{Attribute, Constant, ConstantDesc},
    },
    platform::NativeInt,
};
use lazy_static::lazy_static;

pub type LId = Loc<LongIdent>;
pub type Str = Loc<String>;
pub type StrOpt = Loc<Option<String>>;
pub type Attrs = Vec<Attribute>;

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

pub struct ConstHelper;

impl ConstHelper {
    pub fn mk(loc: Option<Location>, d: ConstantDesc) -> Constant {
        Constant {
            const_desc: d,
            const_loc: loc.unwrap_or(get_default_loc()),
        }
    }

    pub fn integer(loc: Option<Location>, suffix: Option<char>, i: String) -> Constant {
        Self::mk(loc, ConstantDesc::Integer(i, suffix))
    }

    pub fn int(loc: Option<Location>, suffix: Option<char>, i: i64) -> Constant {
        Self::integer(loc, suffix, i.to_string())
    }

    pub fn int32(loc: Option<Location>, suffix: Option<char>, i: i32) -> Constant {
        Self::integer(loc, Some(suffix.unwrap_or('l')), i.to_string())
    }

    pub fn int64(loc: Option<Location>, suffix: Option<char>, i: i64) -> Constant {
        Self::integer(loc, Some(suffix.unwrap_or('L')), i.to_string())
    }

    pub fn nativeint<N: NativeInt>(loc: Option<Location>, suffix: Option<char>, i: N) -> Constant {
        Self::integer(loc, Some(suffix.unwrap_or('n')), i.to_string())
    }

    pub fn float(loc: Option<Location>, suffix: Option<char>, f: String) -> Constant {
        Self::mk(loc, ConstantDesc::Float(f, suffix))
    }

    pub fn char(loc: Option<Location>, c: char) -> Constant {
        Self::mk(loc, ConstantDesc::Char(c))
    }

    pub fn string(quotation_delim: Option<String>, loc: Option<Location>, s: String) -> Constant {
        let l = loc.unwrap_or(get_default_loc());
        Self::mk(Some(l.clone()), ConstantDesc::String(s, l, quotation_delim))
    }
}
