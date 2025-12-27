use crate::cfg::{
    Const, Expr, FunNameUse, Sig, Ty, TyCtx, Value,
    var::{
        // CfgGlobalUse ,
        CfgVarUse,
    },
};

impl Ty {
    pub fn into_inner(&self) -> Ty {
        match self {
            Ty::Ptr(x) => x.as_ref().clone(),
            x => panic!("Expected ptr type but got {x}"),
        }
    }

    pub fn field(&self, n: usize) -> Ty {
        match self {
            Ty::Struct(v) if v.len() > n => v[n].clone(),
            x => panic!("Expected struct type with at least {n} field but got {x}",),
        }
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Self::Ptr(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::String)
    }

    pub fn is_arith(&self) -> bool {
        self.is_ptr() || self.is_int()
    }

    pub fn is_funptr(&self) -> bool {
        matches!(self, Self::FunPtr(_))
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Self::Struct(_))
    }

    pub fn field_count(&self) -> usize {
        match self {
            Ty::Struct(fields) => fields.len(),
            _ => 0,
        }
    }

    pub fn repr_closure(&self) -> bool {
        if !self.is_struct() {
            return false;
        }
        if self.field_count() != 2 {
            return false;
        }

        if !self.field(0).is_funptr() {
            return false;
        }

        let closure_env = self.field(1);

        if !closure_env.is_ptr() {
            return false;
        }

        if !closure_env.into_inner().is_struct() {
            return false;
        }

        true
    }

    fn expect_funptr(&self) -> &Sig {
        match self {
            Ty::FunPtr(sig) => &sig,
            _ => panic!("Expected function pointer but got {}", self),
        }
    }

    fn params(&self) -> &Vec<Ty> {
        &self.expect_funptr().params
    }

    pub fn param_count(&self) -> usize {
        self.params().len()
    }

    pub fn param(&self, n: usize) -> Ty {
        let params = self.params();
        if params.len() <= n {
            panic!(
                "Function pointer has fewer parameters than expected (got: {} but expected at least {}",
                params.len(),
                n + 1
            );
        }

        params[n].clone()
    }
}

impl Value {
    pub fn get_type(&self, ctx: &TyCtx) -> Ty {
        match self {
            // Value::Global(x) => x.get_type(ctx),
            Value::Var(x) => x.get_type(ctx),
            Value::Const(x) => x.get_type(ctx),
        }
    }

    pub fn get_ret_type_as_closure(&self, ctx: &TyCtx) -> Ty {
        match self.get_type(ctx) {
            Ty::Struct(items) if items.len() == 2 => {
                let fst = &items[0];
                match fst {
                    Ty::FunPtr(sig) => {
                        assert!(sig.params.len() == 1);
                        sig.ret.as_ref().clone()
                    }
                    _ => unreachable!(),
                }
            }
            x => unreachable!("Expected closure but got {x}"),
        }
    }
}

impl CfgVarUse {
    pub fn get_type(&self, ctx: &TyCtx) -> Ty {
        ctx.vars[self].clone()
    }
}

// impl CfgGlobalUse {
//     pub fn get_type(&self, ctx: &TyCtx) -> Ty {
//         // ctx.globals[self].clone()
//     }
// }

impl FunNameUse {
    pub fn get_return_type(&self, ctx: &TyCtx) -> Ty {
        match Const::FunPtr(self.clone()).get_type(ctx) {
            Ty::FunPtr(sig) => {
                assert!(sig.params.len() == 1);
                sig.ret.as_ref().clone()
            }
            _ => unreachable!(),
        }
    }
}

impl Expr {
    pub fn get_type(&self, ctx: &TyCtx) -> Ty {
        match self {
            Expr::Value(v) => v.get_type(ctx),
            Expr::Add(_, _) | Expr::Mul(_, _) | Expr::Sub(_, _) | Expr::Div(_, _) => Ty::Int,
            Expr::Call { closure, .. } => closure.get_ret_type_as_closure(ctx),
            Expr::NativeCall { fun, .. } => ctx.natives[fun].get_return_type(ctx),
            Expr::GetElementPtr { ptr, index } => {
                Ty::Ptr(Box::new(ptr.get_type(ctx).into_inner().field(*index)))
            }
            Expr::Extract { value, index } => value.get_type(ctx).field(*index),
            Expr::Load { ty, .. } => ty.clone(),
            Expr::Struct(values) => Ty::Struct(values.iter().map(|x| x.get_type(ctx)).collect()),
        }
    }
}

impl Const {
    pub fn get_type(&self, ctx: &TyCtx) -> Ty {
        match self {
            Const::Int(_) => Ty::Int,
            Const::String(_) => Ty::String,
            Const::Struct(items) => Ty::Struct(items.iter().map(|x| x.get_type(ctx)).collect()),
            Const::FunPtr(name) => Ty::FunPtr(ctx.sigs[name].clone()),
            Const::NullPtr => Ty::Ptr(Box::new(Ty::Void)),
        }
    }
}
