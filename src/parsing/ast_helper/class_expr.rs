use crate::parsing::{
    ast_helper::{get_default_loc, LId},
    asttypes::{ArgLabel, RecFlag},
    location::Location,
    parsetree::{
        Attribute, Attributes, ClassExpr, ClassExprDesc, ClassStructure, ClassType, CoreType,
        Expression, Extension, OpenDescription, Pattern, ValueBinding,
    },
};

impl ClassExpr {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, d: ClassExprDesc) -> Self {
        Self {
            desc: d,
            loc: loc.unwrap_or_else(get_default_loc),
            attributes: attrs.unwrap_or_default(),
        }
    }

    pub fn attr(self, attr: Attribute) -> Self {
        let mut this = self;
        this.attributes.push(attr);
        this
    }

    pub fn constr(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        id: LId,
        tys: Vec<CoreType>,
    ) -> Self {
        Self::mk(loc, attrs, ClassExprDesc::Constr(id, tys))
    }

    pub fn structure(loc: Option<Location>, attrs: Option<Attributes>, cs: ClassStructure) -> Self {
        Self::mk(loc, attrs, ClassExprDesc::Structure(cs))
    }

    pub fn fun(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        lbl: ArgLabel,
        e: Option<Expression>,
        pat: Pattern,
        c: Self,
    ) -> Self {
        Self::mk(loc, attrs, ClassExprDesc::Fun(lbl, e, pat, Box::new(c)))
    }

    pub fn apply(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        c: Self,
        to: Vec<(ArgLabel, Expression)>,
    ) -> Self {
        Self::mk(loc, attrs, ClassExprDesc::Apply(Box::new(c), to))
    }

    pub fn let_(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        rec_flag: RecFlag,
        bindings: Vec<ValueBinding>,
        in_expr: Self,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ClassExprDesc::Let(rec_flag, bindings, Box::new(in_expr)),
        )
    }

    pub fn constraint(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        c: Self,
        ty: ClassType,
    ) -> Self {
        Self::mk(loc, attrs, ClassExprDesc::Constraint(Box::new(c), ty))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, ext: Extension) -> Self {
        Self::mk(loc, attrs, ClassExprDesc::Extension(ext))
    }

    pub fn open(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        d: OpenDescription,
        c: Self,
    ) -> Self {
        Self::mk(loc, attrs, ClassExprDesc::Open(d, Box::new(c)))
    }
}
