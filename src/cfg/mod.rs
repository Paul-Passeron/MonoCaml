use std::collections::{HashMap, HashSet};

use crate::{
    cfg::var::{CfgGlobal, CfgGlobalUse, CfgVar, CfgVarUse, VarKind},
    helpers::unique::{Unique, Use},
};
mod extract;
mod token;
mod var;

#[allow(unused)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunName(usize);

impl FunName {
    pub fn fresh() -> Self {
        Unique::<Self>::fresh()
    }

    pub fn reset() {
        Unique::<Self>::reset();
    }
}

type FunNameUse = Use<FunName>;

#[allow(unused)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(usize);

impl Label {
    pub fn fresh() -> Self {
        Unique::<Self>::fresh()
    }

    pub fn reset() {
        Unique::<Self>::reset();
    }
}

pub type LabelUse = Use<Label>;

pub enum Const {
    Int(i32),
    String(String),
    Struct(Vec<Const>),
    FunPtr(FunNameUse),
    NullPtr,
}

pub enum Value {
    Global(CfgGlobalUse),
    Var(CfgVarUse),
    Const(Const),
}

pub enum Expr {
    // Those expects two integer values
    Add(Value, Value),
    Mul(Value, Value),
    Sub(Value, Value),
    Div(Value, Value),

    // This expects a closure as first arg
    Call {
        closure: Value,
        arg: Value,
    },

    NativeCall {
        funptr: FunNameUse,
        args: Vec<Value>,
    },

    GetElementPtr {
        ptr: Value,
        index: usize,
    },

    Load {
        ptr: Value,
        ty: Ty,
    },

    Struct(Vec<Value>),
}

pub enum Ty {
    Int,
    String,
    Ptr(Box<Self>),
    Struct(Vec<Self>),
}

pub enum Instr<V: VarKind> {
    Assign(V, Expr),
    Store(Value, Ty, Value),
}

impl Instr<CfgVar> {
    pub fn into_use<T, Z>(self) -> (Option<CfgVar>, Instr<T>)
    where
        for<'a> T: VarKind + From<&'a CfgVar>,
    {
        match self {
            Instr::Assign(var, expr) => {
                let new_var = (&var).into();
                (Some(var), Instr::Assign(new_var, expr))
            }
            Instr::Store(ptr, ty, val) => (None, Instr::Store(ptr, ty, val)),
        }
    }
}

pub enum Terminator {
    Return(Option<Value>),
    Goto(LabelUse),
    Branch {
        cond: Value,
        then_bb: LabelUse,
        else_bb: LabelUse,
    },
}

pub struct BasicBlock {
    label: Label,
    instrs: Vec<Instr<CfgVarUse>>,
    terminator: Terminator,
}

pub struct Cfg {
    locals: HashMap<CfgVar, Ty>,
    blocks: Vec<BasicBlock>,
}

pub struct Sig {
    params: Vec<Ty>,
    ret: Ty,
}

pub struct Func {
    name: FunName,
    sig: Sig,
    cfg: Cfg,
}

pub struct Program {
    globals: HashMap<CfgGlobal, Const>,
    funcs: HashSet<Func>,
}

pub struct TyCtx {
    pub sigs: HashMap<FunNameUse, Sig>,
    pub globals: HashMap<CfgGlobalUse, Ty>,
    pub vars: HashMap<CfgVarUse, Ty>,
}
