use std::sync::Mutex;

use crate::{
    parsing::{
        asttypes::{ArgLabel, ClosedFlag, Loc},
        location::Location,
        longident::LongIdent,
        parsetree::{
            Attribute, Attributes, Constant, ConstantDesc, CoreType, CoreTypeDesc, Extension,
            ObjectField, PackageType, Payload, RowField,
        },
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

pub struct AttrHelper;

impl AttrHelper {
    pub fn mk(loc: Option<Location>, name: Str, payload: Payload) -> Attribute {
        Attribute {
            name,
            payload,
            loc: loc.unwrap_or(get_default_loc()),
        }
    }
}

pub struct TypHelper;

impl TypHelper {
    pub fn mk(loc: Option<Location>, attrs: Option<Vec<Attribute>>, d: CoreTypeDesc) -> CoreType {
        let l = loc.unwrap_or(get_default_loc());
        CoreType {
            type_desc: d,
            loc: l,
            loc_stack: vec![],
            attributes: attrs.unwrap_or_default(),
        }
    }

    pub fn attr(d: CoreType, a: Attribute) -> CoreType {
        let mut d = d;
        d.attributes.push(a);
        d
    }

    pub fn any(loc: Option<Location>, attrs: Option<Attributes>) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Any)
    }

    pub fn var(loc: Option<Location>, attrs: Option<Attributes>, a: String) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Var(a))
    }

    pub fn arrow(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        label: ArgLabel,
        from: CoreType,
        to: CoreType,
    ) -> CoreType {
        Self::mk(
            loc,
            attrs,
            CoreTypeDesc::Arrow(label, Box::new(from), Box::new(to)),
        )
    }

    pub fn tuple(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        tys: Vec<(Option<String>, CoreType)>,
    ) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Tuple(tys))
    }

    pub fn constr(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        iden: LId,
        tys: Vec<CoreType>,
    ) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Constr(iden, tys))
    }

    pub fn object(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        fields: Vec<ObjectField>,
        closed: ClosedFlag,
    ) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Object(fields, closed))
    }

    pub fn class(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        iden: LId,
        tys: Vec<CoreType>,
    ) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Class(iden, tys))
    }

    pub fn alias(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        ty: CoreType,
        iden: Str,
    ) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Alias(Box::new(ty), iden))
    }

    pub fn variant(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        row_fields: Vec<RowField>,
        closed: ClosedFlag,
        labels: Option<Vec<String>>,
    ) -> CoreType {
        Self::mk(
            loc,
            attrs,
            CoreTypeDesc::Variant(row_fields, closed, labels),
        )
    }

    pub fn poly(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        labels: Vec<Str>,
        ty: CoreType,
    ) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Poly(labels, Box::new(ty)))
    }

    pub fn package(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        package_type: PackageType,
    ) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Package(package_type))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, ext: Extension) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Extension(Box::new(ext)))
    }

    pub fn open(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        mod_ident: LId,
        t: CoreType,
    ) -> CoreType {
        Self::mk(loc, attrs, CoreTypeDesc::Open(mod_ident, Box::new(t)))
    }
}
