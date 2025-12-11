use crate::parsing::{
    ast_helper::{get_default_loc, LId},
    location::Location,
    parsetree::{
        Attribute, Attributes, Expression, Extension, FunctorParameter, ModuleExpr, ModuleExprDesc,
        ModuleType, Structure,
    },
};

impl ModuleExpr {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, desc: ModuleExprDesc) -> Self {
        Self {
            desc,
            loc: loc.unwrap_or(get_default_loc()),
            attributes: attrs.unwrap_or_default(),
        }
    }

    pub fn attr(self, attr: Attribute) -> Self {
        let mut this = self;
        this.attributes.push(attr);
        this
    }

    pub fn ident(loc: Option<Location>, attrs: Option<Attributes>, ident: LId) -> Self {
        Self::mk(loc, attrs, ModuleExprDesc::Ident(ident))
    }

    pub fn structure(loc: Option<Location>, attrs: Option<Attributes>, struc: Structure) -> Self {
        Self::mk(loc, attrs, ModuleExprDesc::Structure(struc))
    }

    pub fn functor(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        f_p: FunctorParameter,
        t: Self,
    ) -> Self {
        Self::mk(loc, attrs, ModuleExprDesc::Functor(f_p, Box::new(t)))
    }

    pub fn apply(loc: Option<Location>, attrs: Option<Attributes>, m1: Self, m2: Self) -> Self {
        Self::mk(
            loc,
            attrs,
            ModuleExprDesc::Apply(Box::new(m1), Box::new(m2)),
        )
    }

    pub fn apply_unit(loc: Option<Location>, attrs: Option<Attributes>, m: Self) -> Self {
        Self::mk(loc, attrs, ModuleExprDesc::ApplyUnit(Box::new(m)))
    }

    pub fn constraint(
        self,
        loc: Option<Location>,
        attrs: Option<Attributes>,
        mty: ModuleType,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ModuleExprDesc::Constraint(Box::new(self), Box::new(mty)),
        )
    }

    pub fn unpack(loc: Option<Location>, attrs: Option<Attributes>, e: Expression) -> Self {
        Self::mk(loc, attrs, ModuleExprDesc::Unpack(Box::new(e)))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, e: Extension) -> Self {
        Self::mk(loc, attrs, ModuleExprDesc::Extension(Box::new(e)))
    }
}
