use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::{
    cfg::{
        expr::Expr,
        var::{
            // CfgGlobal, CfgGlobalUse,
            CfgVar,
            CfgVarUse,
            VarKind,
        },
    },
    helpers::unique::{Unique, Use},
};
pub mod builder;
pub mod compile;
pub mod display;
pub mod export_c;
pub mod expr;
pub mod extract;
pub mod get_type;
pub mod token;
pub mod var;

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

#[derive(Clone)]
pub enum Const {
    Int(i32),
    String(String),
    Struct(Vec<Const>),
    FunPtr(FunNameUse),
    NullPtr,
}

#[derive(Clone)]
pub enum Value {
    Var(CfgVarUse),
    Const(Const),
}

impl Value {
    pub fn constant(c: Const) -> Self {
        Self::Const(c)
    }

    pub fn variable(v: CfgVarUse) -> Self {
        Self::Var(v)
    }
}

impl Into<Value> for Const {
    fn into(self) -> Value {
        Value::Const(self)
    }
}

impl Into<Value> for CfgVarUse {
    fn into(self) -> Value {
        Value::Var(self)
    }
}

#[derive(Debug, Clone, Hash)]
pub enum Ty {
    Int,
    String,
    Void,
    Ptr(Box<Self>),
    Struct(Vec<Self>),
    FunPtr(Sig),
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.matches(other)
    }
}

impl Eq for Ty {}

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
    phis: HashMap<CfgVarUse, HashSet<CfgVarUse>>,
    instrs: Vec<Instr<CfgVarUse>>,
    terminator: Terminator,
}

#[allow(unused)]
pub struct Cfg {
    entry: LabelUse,
    locals: HashMap<CfgVar, Ty>,
    blocks: Vec<BasicBlock>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
    entry: FunNameUse,
    natives: HashMap<String, FunNameUse>,
    funcs: HashSet<Func>,
}

#[derive(Clone)]
pub struct TyCtx {
    pub sigs: HashMap<FunNameUse, Sig>,
    // pub globals: HashMap<CfgGlobalUse, Ty>,
    pub vars: HashMap<CfgVarUse, Ty>,
    pub natives: HashMap<String, FunNameUse>,
}
