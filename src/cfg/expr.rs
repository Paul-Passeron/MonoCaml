use crate::cfg::{Const, Ty, TyCtx, Value};

pub enum Expr {
    Value(Value),

    // Those expects two integer values
    Add(Value, Value),
    Mul(Value, Value),
    Sub(Value, Value),
    Div(Value, Value),
    Eq(Value, Value),

    NativeCall { fun: Value, args: Vec<Value> },

    GetElementPtr { ptr: Value, ty: Ty, index: usize },

    Extract { value: Value, index: usize },

    Load { ptr: Value, ty: Ty },

    Struct(Vec<Value>),
    Union(Ty, Value, usize),

    Malloc(Ty, Value),

    Cast(Ty, Value),

    Phi(Ty),
}

impl Expr {
    fn check_arith(ctx: &TyCtx, lhs: &Value, rhs: &Value, str: &str) {
        let lhs_t = lhs.get_type(ctx);
        let rhs_t = rhs.get_type(ctx);
        if !lhs_t.is_arith() || !rhs_t.is_arith() {
            panic!("Cannot {str} non arith ({lhs_t} and {rhs_t}")
        }
    }

    pub fn eq(ctx: &TyCtx, lhs: Value, rhs: Value) -> Self {
        Self::check_arith(ctx, &lhs, &rhs, "eq");
        Self::Eq(lhs, rhs)
    }

    pub fn add(ctx: &TyCtx, lhs: Value, rhs: Value) -> Self {
        Self::check_arith(ctx, &lhs, &rhs, "add");
        Self::Add(lhs, rhs)
    }

    pub fn mul(ctx: &TyCtx, lhs: Value, rhs: Value) -> Self {
        Self::check_arith(ctx, &lhs, &rhs, "mul");
        Self::Mul(lhs, rhs)
    }

    pub fn sub(ctx: &TyCtx, lhs: Value, rhs: Value) -> Self {
        Self::check_arith(ctx, &lhs, &rhs, "sub");
        Self::Sub(lhs, rhs)
    }

    pub fn div(ctx: &TyCtx, lhs: Value, rhs: Value) -> Self {
        Self::check_arith(ctx, &lhs, &rhs, "div");
        Self::Div(lhs, rhs)
    }

    pub fn native_call(ctx: &TyCtx, fun: Value, args: Vec<Value>) -> Self {
        let params = fun.get_type(ctx).sig().params;
        // let params = &ctx.sigs[&name].params;

        if params.len() != args.len() {
            panic!("Native function {fun} expects {} arguments", params.len())
        }

        for (i, (arg, expected_ty)) in args.iter().zip(params).enumerate() {
            let got_ty = arg.get_type(ctx);
            if !got_ty.matches(&expected_ty) {
                panic!(
                    "Native function {fun} expects argument {i} of type {expected_ty} but got {got_ty}"
                )
            }
        }

        Self::NativeCall { fun: fun, args }
    }

    pub fn get_element_ptr(ctx: &TyCtx, ptr: Value, ty: Ty, index: usize) -> Self {
        if !ptr.get_type(ctx).is_ptr() {
            panic!("Cannot use getelementptr on non-pointer type")
        }
        let _ = ty.field(index);

        Self::GetElementPtr { ptr, ty, index }
    }

    pub fn extract(ctx: &TyCtx, value: Value, index: usize) -> Self {
        let _ = value.get_type(ctx).field(index);
        Self::Extract { value, index }
    }

    pub fn load(ctx: &TyCtx, ptr: Value, ty: Ty) -> Self {
        if !ptr.get_type(ctx).is_ptr() {
            panic!("Cannot load from non-pointer type")
        }

        Self::Load { ptr, ty }
    }

    pub fn aggregate(values: Vec<Value>) -> Self {
        Self::Struct(values)
    }

    pub fn value(v: Value) -> Self {
        Self::Value(v)
    }

    pub fn malloc_single(ty: Ty) -> Self {
        Self::malloc(ty, Const::Int(1).into())
    }

    pub fn malloc(ty: Ty, v: Value) -> Self {
        if ty.is_zero_sized() {
            Self::Value(Const::NullPtr.into())
        } else {
            Self::Malloc(ty, v)
        }
    }

    pub fn cast(ty: Ty, v: Value) -> Self {
        Self::Cast(ty, v)
    }

    pub fn union(ty: Ty, val: Value, field: usize) -> Self {
        assert!(ty.is_union());
        Self::Union(ty, val, field)
    }
}
