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
        syntaxerr,
    },
    platform::NativeInt,
};
use lazy_static::lazy_static;

use super::parsetree::{ObjectFieldDesc, RowFieldDesc};

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

impl Attribute {
    pub fn mk(loc: Option<Location>, name: Str, payload: Payload) -> Attribute {
        Attribute {
            name,
            payload,
            loc: loc.unwrap_or(get_default_loc()),
        }
    }
}

impl CoreType {
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

    pub fn force_poly(self) -> Self {
        match self.type_desc {
            CoreTypeDesc::Poly(_, _) => self,
            _ => Self::poly(
                Some(self.loc.clone()),
                Some(self.attributes.clone()),
                vec![],
                self,
            ),
        }
    }

    pub fn varify_constructors(self, var_names: &Vec<Str>) -> Result<Self, syntaxerr::Error> {
        CoreTypeHelper::new(&var_names).loopfun(&self)
    }

    pub fn package_type(
        loc: Option<Location>,
        attrs: Option<Attrs>,
        p: LId,
        c: Vec<(LId, CoreType)>,
    ) -> PackageType {
        PackageType {
            path: p,
            constraints: c,
            loc: loc.unwrap_or(get_default_loc()),
            attrs: attrs.unwrap_or_default(),
        }
    }
}

struct CoreTypeHelper<'a> {
    vars: &'a Vec<Str>,
}

impl<'a> CoreTypeHelper<'a> {
    pub fn new(vars: &'a Vec<Str>) -> Self {
        Self { vars }
    }

    pub fn check_variable(&self, var: &String, loc: &Location) -> Result<(), syntaxerr::Error> {
        if self.mem(var) {
            Err(syntaxerr::Error::VariableInScope(loc.clone(), var.clone()))
        } else {
            Ok(())
        }
    }

    pub fn mem(&self, v: &String) -> bool {
        self.vars.iter().find(|Loc { txt, loc }| txt == v).is_some()
    }
}

trait CoreTypeLoopHelper<T> {
    fn loopfun(&self, x: &T) -> Result<T, syntaxerr::Error>;
}

impl<'a, T> CoreTypeLoopHelper<Vec<T>> for CoreTypeHelper<'a>
where
    CoreTypeHelper<'a>: CoreTypeLoopHelper<T>,
{
    fn loopfun(&self, x: &Vec<T>) -> Result<Vec<T>, syntaxerr::Error> {
        x.iter().map(|x| self.loopfun(x)).collect()
    }
}

impl<'a, T> CoreTypeLoopHelper<Box<T>> for CoreTypeHelper<'a>
where
    CoreTypeHelper<'a>: CoreTypeLoopHelper<T>,
{
    fn loopfun(&self, x: &Box<T>) -> Result<Box<T>, syntaxerr::Error> {
        Ok(Box::new(self.loopfun(x)?))
    }
}

impl<'a, A, B> CoreTypeLoopHelper<(B, A)> for CoreTypeHelper<'a>
where
    CoreTypeHelper<'a>: CoreTypeLoopHelper<A>,
    B: Clone,
{
    fn loopfun(&self, (l, x): &(B, A)) -> Result<(B, A), syntaxerr::Error> {
        Ok((l.clone(), self.loopfun(x)?))
    }
}

impl<'a> CoreTypeLoopHelper<CoreType> for CoreTypeHelper<'a> {
    fn loopfun(&self, ty: &CoreType) -> Result<CoreType, syntaxerr::Error> {
        let desc = match &ty.type_desc {
            CoreTypeDesc::Any => CoreTypeDesc::Any,
            CoreTypeDesc::Var(x) => {
                self.check_variable(x, &ty.loc)?;
                CoreTypeDesc::Var(x.clone())
            }
            CoreTypeDesc::Arrow(arglabel, ty1, ty2) => {
                let new_ty1 = self.loopfun(ty1)?;
                let new_ty2 = self.loopfun(ty2)?;
                CoreTypeDesc::Arrow(arglabel.clone(), new_ty1, new_ty2)
            }
            CoreTypeDesc::Tuple(tys) => CoreTypeDesc::Tuple(self.loopfun(tys)?),
            CoreTypeDesc::Constr(
                Loc {
                    txt: LongIdent::Ident(label),
                    ..
                },
                lst,
            ) if lst.is_empty() && self.mem(&label) => CoreTypeDesc::Var(label.clone()),
            CoreTypeDesc::Constr(longident, lst) => {
                CoreTypeDesc::Constr(longident.clone(), self.loopfun(lst)?)
            }
            CoreTypeDesc::Object(fields, flag) => {
                CoreTypeDesc::Object(self.loopfun(fields)?, *flag)
            }
            CoreTypeDesc::Class(longident, lst) => {
                CoreTypeDesc::Class(longident.clone(), self.loopfun(lst)?)
            }
            CoreTypeDesc::Alias(core_type, alias) => {
                self.check_variable(&alias.txt, &alias.loc)?;
                CoreTypeDesc::Alias(self.loopfun(core_type)?, alias.clone())
            }
            CoreTypeDesc::Variant(row_field_list, flag, lbl_lst_option) => {
                CoreTypeDesc::Variant(self.loopfun(row_field_list)?, *flag, lbl_lst_option.clone())
            }
            CoreTypeDesc::Poly(string_lst, core_type) => {
                string_lst
                    .iter()
                    .try_for_each(|Loc { txt, loc }| self.check_variable(txt, loc))?;
                CoreTypeDesc::Poly(string_lst.clone(), self.loopfun(core_type)?)
            }
            CoreTypeDesc::Package(ptyp) => CoreTypeDesc::Package(self.loopfun(ptyp)?),
            CoreTypeDesc::Open(mod_ident, core_type) => {
                CoreTypeDesc::Open(mod_ident.clone(), self.loopfun(core_type)?)
            }
            CoreTypeDesc::Extension(ext) => CoreTypeDesc::Extension(ext.clone()),
        };
        Ok(CoreType {
            type_desc: desc,
            loc: ty.loc.clone(),
            loc_stack: ty.loc_stack.clone(),
            attributes: ty.attributes.clone(),
        })
    }
}

impl<'a> CoreTypeLoopHelper<RowField> for CoreTypeHelper<'a> {
    fn loopfun(&self, field: &RowField) -> Result<RowField, syntaxerr::Error> {
        let desc = match &field.desc {
            RowFieldDesc::Tag(label, flag, core_types) => {
                RowFieldDesc::Tag(label.clone(), *flag, self.loopfun(core_types)?)
            }
            RowFieldDesc::Inherit(core_type) => RowFieldDesc::Inherit(self.loopfun(core_type)?),
        };
        Ok(RowField {
            desc,
            loc: field.loc.clone(),
            attributes: field.attributes.clone(),
        })
    }
}

impl<'a> CoreTypeLoopHelper<ObjectField> for CoreTypeHelper<'a> {
    fn loopfun(&self, field: &ObjectField) -> Result<ObjectField, syntaxerr::Error> {
        let desc = match &field.desc {
            ObjectFieldDesc::Tag(loc, core_type) => {
                ObjectFieldDesc::Tag(loc.clone(), self.loopfun(core_type)?)
            }
            ObjectFieldDesc::Inherit(core_type) => {
                ObjectFieldDesc::Inherit(self.loopfun(core_type)?)
            }
        };
        Ok(ObjectField {
            desc,
            loc: field.loc.clone(),
            attributes: field.attributes.clone(),
        })
    }
}

impl<'a> CoreTypeLoopHelper<PackageType> for CoreTypeHelper<'a> {
    fn loopfun(&self, ptyp: &PackageType) -> Result<PackageType, syntaxerr::Error> {
        Ok(PackageType {
            path: ptyp.path.clone(),
            loc: ptyp.loc.clone(),
            attrs: ptyp.attrs.clone(),
            constraints: self.loopfun(&ptyp.constraints)?,
        })
    }
}
