use crate::cfg::{Const, FunNameUse, Sig, Ty, TyCtx, Value, expr::Expr, var::CfgVarUse};

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

    pub fn sig(&self) -> Sig {
        match self {
            Ty::FunPtr(sig) => sig.clone(),
            x => panic!("Expected funptr type but got {x}"),
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

    pub fn choose_arith(a: Ty, b: Ty) -> Ty {
        if !a.is_arith() {
            panic!("Expected arithmetic type but got {}", a);
        } else if !b.is_arith() {
            panic!("Expected arithmetic type but got {}", b);
        } else if a.is_ptr() {
            a
        } else if b.is_ptr() {
            b
        } else {
            Ty::Int
        }
    }

    pub fn is_zero_sized(&self) -> bool {
        match self {
            Ty::Void => true,
            Ty::Struct(items) => items.is_empty(),
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Ty::Void)
    }

    pub fn matches(&self, other: &Self) -> bool {
        if self.is_zero_sized() && other.is_zero_sized() {
            return true;
        }

        if self.is_ptr() && other.is_ptr() {
            return true;
        }

        match (&self, &other) {
            (Ty::Int, Ty::Int) => true,
            (Ty::String, Ty::String) => true,
            (Ty::Struct(items), Ty::Struct(items2)) => {
                if items.len() != items2.len() {
                    return false;
                }
                items.iter().zip(items2.iter()).all(|(x, y)| x.matches(y))
            }
            (Ty::FunPtr(s1), Ty::FunPtr(s2)) => {
                if !s1.ret.matches(&s2.ret) {
                    return false;
                }

                if s1.params.len() != s2.params.len() {
                    return false;
                }

                s1.params
                    .iter()
                    .zip(s2.params.iter())
                    .all(|(x, y)| x.matches(y))
            }
            _ => false,
        }
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
                        // assert!(sig.params.len() == 1);
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
                // assert!(sig.params.len() == 1);
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
            Expr::Add(a, b) | Expr::Mul(a, b) | Expr::Sub(a, b) | Expr::Div(a, b) => {
                Ty::choose_arith(a.get_type(ctx), b.get_type(ctx))
            }
            Expr::NativeCall { fun, .. } => *fun.get_type(ctx).sig().ret,
            Expr::GetElementPtr { ty, index, .. } => Ty::Ptr(Box::new(ty.field(*index))),
            Expr::Extract { value, index } => value.get_type(ctx).field(*index),
            Expr::Load { ty, .. } => ty.clone(),
            Expr::Struct(values) => Ty::Struct(values.iter().map(|x| x.get_type(ctx)).collect()),
            Expr::Malloc(ty, _) => Ty::Ptr(Box::new(ty.clone())),
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
