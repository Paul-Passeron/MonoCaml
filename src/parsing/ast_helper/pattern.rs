use crate::parsing::{
    ast_helper::{get_default_loc, LId, Str},
    asttypes::ClosedFlag,
    location::Location,
    parsetree::{
        Attribute, Attributes, Constant, CoreType, Extension, PackageType, Pattern, PatternDesc,
    },
};

impl Pattern {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, d: PatternDesc) -> Self {
        Self {
            desc: d,
            loc: loc.unwrap_or(get_default_loc()),
            loc_stack: vec![],
            attributes: attrs.unwrap_or_default(),
        }
    }

    pub fn attr(self, attr: Attribute) -> Self {
        let mut this = self;
        this.attributes.push(attr);
        this
    }

    pub fn any(loc: Option<Location>, attrs: Option<Attributes>) -> Self {
        Self::mk(loc, attrs, PatternDesc::Any)
    }

    pub fn var(loc: Option<Location>, attrs: Option<Attributes>, name: Str) -> Self {
        Self::mk(loc, attrs, PatternDesc::Var(name))
    }

    pub fn alias(self, loc: Option<Location>, attrs: Option<Attributes>, name: Str) -> Self {
        Self::mk(loc, attrs, PatternDesc::Alias(Box::new(self), name))
    }

    pub fn constant(loc: Option<Location>, attrs: Option<Attributes>, cst: Constant) -> Self {
        Self::mk(loc, attrs, PatternDesc::Constant(cst))
    }

    pub fn interval(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        from: Constant,
        to: Constant,
    ) -> Self {
        Self::mk(loc, attrs, PatternDesc::Interval(from, to))
    }

    pub fn tuple(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        items: Vec<(Option<String>, Self)>,
        closed_flag: ClosedFlag,
    ) -> Self {
        Self::mk(loc, attrs, PatternDesc::Tuple(items, closed_flag))
    }

    pub fn construct(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        name: LId,
        item: Option<(Vec<Str>, Self)>,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            PatternDesc::Construct {
                c: name,
                args: item.map(|(a, b)| (a, Box::new(b))),
            },
        )
    }

    pub fn variant(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        name: String,
        pat: Option<Self>,
    ) -> Self {
        Self::mk(loc, attrs, PatternDesc::Variant(name, pat.map(Box::new)))
    }

    pub fn record(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        items: Vec<(LId, Self)>,
        closed_flag: ClosedFlag,
    ) -> Self {
        Self::mk(loc, attrs, PatternDesc::Record(items, closed_flag))
    }

    pub fn array(loc: Option<Location>, attrs: Option<Attributes>, pats: Vec<Self>) -> Self {
        Self::mk(loc, attrs, PatternDesc::Array(pats))
    }

    pub fn or(loc: Option<Location>, attrs: Option<Attributes>, a: Self, b: Self) -> Self {
        Self::mk(loc, attrs, PatternDesc::Or(Box::new(a), Box::new(b)))
    }

    pub fn constraint(
        self,
        loc: Option<Location>,
        attrs: Option<Attributes>,
        ty: CoreType,
    ) -> Self {
        Self::mk(loc, attrs, PatternDesc::Constraint(Box::new(self), ty))
    }

    pub fn type_(loc: Option<Location>, attrs: Option<Attributes>, name: LId) -> Self {
        Self::mk(loc, attrs, PatternDesc::Type(name))
    }

    pub fn lazy(self, loc: Option<Location>, attrs: Option<Attributes>) -> Self {
        Self::mk(loc, attrs, PatternDesc::Lazy(Box::new(self)))
    }

    pub fn unpack(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        lbl: Option<Str>,
        ptyp: Option<PackageType>,
    ) -> Self {
        Self::mk(loc, attrs, PatternDesc::Unpack(lbl, ptyp))
    }

    pub fn open(self, loc: Option<Location>, attrs: Option<Attributes>, lbl: LId) -> Self {
        Self::mk(loc, attrs, PatternDesc::Open(lbl, Box::new(self)))
    }

    pub fn exception(self, loc: Option<Location>, attrs: Option<Attributes>) -> Self {
        Self::mk(loc, attrs, PatternDesc::Exception(Box::new(self)))
    }

    pub fn effect(loc: Option<Location>, attrs: Option<Attributes>, a: Self, b: Self) -> Self {
        Self::mk(loc, attrs, PatternDesc::Effect(Box::new(a), Box::new(b)))
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, ext: Extension) -> Self {
        Self::mk(loc, attrs, PatternDesc::Extension(Box::new(ext)))
    }
}
