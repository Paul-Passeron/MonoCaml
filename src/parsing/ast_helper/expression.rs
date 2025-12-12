use crate::parsing::{
    ast_helper::{get_default_loc, LId, Str},
    asttypes::{ArgLabel, DirectionFlag, RecFlag},
    location::Location,
    parsetree::{
        Attribute, Attributes, BindingOp, Case, ClassStructure, Constant, CoreType, Expression,
        ExpressionDesc, Extension, FunctionBody, FunctionParam, Letop, ModuleExpr, PackageType,
        Pattern, StructureItem, TypeConstraint, ValueBinding,
    },
};

impl Expression {
    pub fn mk(loc: Option<Location>, attrs: Option<Attributes>, desc: ExpressionDesc) -> Self {
        Self {
            desc,
            loc: loc.unwrap_or(get_default_loc()),
            attributes: attrs.unwrap_or_default(),
            loc_stack: vec![],
        }
    }

    pub fn attr(self, attr: Attribute) -> Self {
        let mut this = self;
        this.attributes.push(attr);
        this
    }

    pub fn ident(loc: Option<Location>, attrs: Option<Attributes>, ident: LId) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Ident(ident))
    }

    pub fn constant(loc: Option<Location>, attrs: Option<Attributes>, cst: Constant) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Constant(cst))
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
            ExpressionDesc::Let {
                recursive: rec_flag,
                bindings,
                in_expr: Box::new(in_expr),
            },
        )
    }

    pub fn function(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        params: Vec<FunctionParam>,
        constraint: Option<TypeConstraint>,
        body: FunctionBody,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::Function {
                params,
                constraint,
                body,
            },
        )
    }

    pub fn apply(
        self,
        loc: Option<Location>,
        attrs: Option<Attributes>,
        exprs: Vec<(ArgLabel, Expression)>,
    ) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Apply(Box::new(self), exprs))
    }

    pub fn match_(
        self,
        loc: Option<Location>,
        attrs: Option<Attributes>,
        cases: Vec<Case>,
    ) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Match(Box::new(self), cases))
    }

    pub fn try_(self, loc: Option<Location>, attrs: Option<Attributes>, cases: Vec<Case>) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Match(Box::new(self), cases))
    }

    pub fn tuple(
        self,
        loc: Option<Location>,
        attrs: Option<Attributes>,
        exprs: Vec<(Option<String>, Expression)>,
    ) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Tuple(exprs))
    }

    pub fn construct(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        id: LId,
        expr: Option<Expression>,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::Construct(id, expr.map(Box::new)),
        )
    }

    pub fn variant(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        id: String,
        expr: Option<Expression>,
    ) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Variant(id, expr.map(Box::new)))
    }

    pub fn record(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        lst: Vec<(LId, Expression)>,
        e: Option<Expression>,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::Record {
                fields: lst,
                with_expr: e.map(Box::new),
            },
        )
    }

    pub fn field(self, loc: Option<Location>, attrs: Option<Attributes>, id: LId) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Field(Box::new(self), id))
    }

    pub fn setfield(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        field: Self,
        id: LId,
        value: Self,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::SetField(Box::new(field), id, Box::new(value)),
        )
    }

    pub fn array(loc: Option<Location>, attrs: Option<Attributes>, exprs: Vec<Expression>) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Array(exprs))
    }

    pub fn ifthenelse(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        cond: Self,
        then_expr: Self,
        else_expr: Option<Expression>,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::IfThenElse {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: else_expr.map(Box::new),
            },
        )
    }

    pub fn sequence(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        fst: Self,
        snd: Self,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::Seq(Box::new(fst), Box::new(snd)),
        )
    }

    pub fn while_(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        cond: Self,
        body: Self,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::While {
                cond: Box::new(cond),
                body: Box::new(body),
            },
        )
    }

    pub fn for_(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        pat: Pattern,
        from: Self,
        to: Self,
        dir: DirectionFlag,
        body: Self,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::For {
                pat: pat,
                from: Box::new(from),
                to: Box::new(to),
                dir,
                do_expr: Box::new(body),
            },
        )
    }

    pub fn constraint(
        self,
        loc: Option<Location>,
        attrs: Option<Attributes>,
        ty: CoreType,
    ) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Constraint(Box::new(self), ty))
    }

    pub fn coerce(
        self,
        loc: Option<Location>,
        attrs: Option<Attributes>,
        from: Option<CoreType>,
        to: CoreType,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::Coerce {
                e: Box::new(self),
                from,
                to,
            },
        )
    }

    pub fn lazy(self, loc: Option<Location>, attrs: Option<Attributes>) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Lazy(Box::new(self)))
    }

    pub fn poly(
        self,
        loc: Option<Location>,
        attrs: Option<Attributes>,
        ty: Option<CoreType>,
    ) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Poly(Box::new(self), ty))
    }

    pub fn object(loc: Option<Location>, attrs: Option<Attributes>, cs: ClassStructure) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Object(cs))
    }

    pub fn new_type(self, loc: Option<Location>, attrs: Option<Attributes>, lbl: Str) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::NewType(lbl, Box::new(self)))
    }

    pub fn pack(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        m: ModuleExpr,
        ptyp: Option<PackageType>,
    ) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Pack(m, ptyp))
    }

    pub fn letop(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        let_: BindingOp,
        ands: Vec<BindingOp>,
        body: Self,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::Letop(Letop {
                let_,
                ands,
                body: Box::new(body),
            }),
        )
    }

    pub fn extension(loc: Option<Location>, attrs: Option<Attributes>, ext: Extension) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Extension(Box::new(ext)))
    }

    pub fn unreach(loc: Option<Location>, attrs: Option<Attributes>) -> Self {
        Self::mk(loc, attrs, ExpressionDesc::Unreachable)
    }

    pub fn struct_item(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        si: StructureItem,
        e: Self,
    ) -> Self {
        Self::mk(
            loc,
            attrs,
            ExpressionDesc::StructItem(Box::new(si), Box::new(e)),
        )
    }

    pub fn case(lhs: Pattern, guard: Option<Self>, rhs: Self) -> Case {
        Case {
            lhs,
            guard: guard.map(Box::new),
            rhs: Box::new(rhs),
        }
    }

    pub fn binding_op(op: Str, pat: Pattern, exp: Self, loc: Location) -> BindingOp {
        BindingOp {
            op,
            pat,
            exp: Box::new(exp),
            loc,
        }
    }
}
