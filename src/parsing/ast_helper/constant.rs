use crate::{
    parsing::{
        ast_helper::get_default_loc,
        location::Location,
        parsetree::{Constant, ConstantDesc},
    },
    platform::NativeInt,
};

impl Constant {
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
