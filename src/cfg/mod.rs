use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::{
    cfg::var::{
        // CfgGlobal, CfgGlobalUse,
        CfgVar,
        CfgVarUse,
        VarKind,
    },
    helpers::unique::{Unique, Use},
};
pub mod builder;
mod display;
mod extract;
mod get_type;
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
    // Global(CfgGlobalUse),
    Var(CfgVarUse),
    Const(Const),
}

pub enum Expr {
    Value(Value),

    // Those expects two integer values
    Add(Value, Value),
    Mul(Value, Value),
    Sub(Value, Value),
    Div(Value, Value),

    // This expects a closure as first arg
    Call { closure: Value, arg: Value },

    NativeCall { fun: String, args: Vec<Value> },

    GetElementPtr { ptr: Value, index: usize },

    Extract { value: Value, index: usize },

    Load { ptr: Value, ty: Ty },

    Struct(Vec<Value>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Ty {
    Int,
    String,
    Void,
    Ptr(Box<Self>),
    Struct(Vec<Self>),
    FunPtr(Sig),
}

pub enum Instr<V: VarKind> {
    Assign(V, Expr),
    Store { ptr: Value, value: Value },
}

impl Instr<CfgVar> {
    pub fn into_use<T>(self) -> (Option<CfgVar>, Instr<T>)
    where
        for<'a> T: VarKind + From<&'a CfgVar>,
    {
        match self {
            Instr::Assign(var, expr) => {
                let new_var = (&var).into();
                (Some(var), Instr::Assign(new_var, expr))
            }
            Instr::Store { ptr, value } => (None, Instr::Store { ptr, value }),
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

#[allow(unused)]
pub struct Cfg {
    locals: HashMap<CfgVar, Ty>,
    blocks: Vec<BasicBlock>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Sig {
    params: Vec<Ty>,
    ret: Box<Ty>,
}

pub struct Func {
    name: FunName,
    params: Vec<(CfgVar, Ty)>,
    ret_ty: Ty,
    cfg: Option<Cfg>,
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Func {}

impl Hash for Func {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

pub struct Program {
    // globals: HashMap<CfgGlobal, Const>,
    natives: HashMap<String, FunNameUse>,
    funcs: HashSet<Func>,
}

pub struct TyCtx {
    pub sigs: HashMap<FunNameUse, Sig>,
    // pub globals: HashMap<CfgGlobalUse, Ty>,
    pub vars: HashMap<CfgVarUse, Ty>,
    pub natives: HashMap<String, FunNameUse>,
}
