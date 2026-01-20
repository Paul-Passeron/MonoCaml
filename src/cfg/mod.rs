use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::{
    backend::Backend,
    cfg::{
        expr::Expr,
        var::{CfgVar, CfgVarUse, VarKind},
    },
    helpers::unique::{Unique, Use},
};

pub mod builder;
pub mod compile;
pub mod display;
pub mod enums;
pub mod expr;
pub mod extract;
pub mod get_type;
pub mod native_funs;
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

pub type FunNameUse = Use<FunName>;

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

#[derive(Clone, PartialEq)]
pub enum Const {
    Int(i32),
    String(String),
    Struct(Vec<Const>),
    FunPtr(FunNameUse),
    NullPtr,
}

#[derive(Clone, PartialEq)]
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
    Union(Vec<Self>),
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
    instrs: Vec<Instr<CfgVarUse>>,
    terminator: Terminator,
}

impl BasicBlock {
    pub fn label(&self) -> LabelUse {
        Use::from(&self.label)
    }

    pub fn terminator(&self) -> &Terminator {
        &self.terminator
    }

    pub fn instrs(&self) -> &Vec<Instr<CfgVarUse>> {
        &self.instrs
    }
}

#[allow(unused)]
pub struct Cfg {
    entry: LabelUse,
    locals: HashMap<CfgVar, Ty>,
    blocks: Vec<BasicBlock>,
}

impl Cfg {
    pub fn entry(&self) -> &LabelUse {
        &self.entry
    }

    pub fn locals(&self) -> &HashMap<CfgVar, Ty> {
        &self.locals
    }

    pub fn blocks(&self) -> &Vec<BasicBlock> {
        &self.blocks
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Sig {
    params: Vec<Ty>,
    ret: Box<Ty>,
}

impl Sig {
    pub fn new(params: Vec<Ty>, ret: Ty) -> Self {
        for param in &params {
            if param.is_zero_sized() {
                panic!("Zero-sized parameter not allowed");
            }
        }
        Self {
            params,
            ret: Box::new(ret),
        }
    }

    pub fn ret(&self) -> &Ty {
        &self.ret
    }

    pub fn params(&self) -> &Vec<Ty> {
        &self.params
    }
}

pub struct Func {
    name: FunName,
    params: Vec<(CfgVar, Ty)>,
    ret_ty: Ty,
    cfg: Option<Cfg>,
}

impl Func {
    pub fn new(name: FunName, params: Vec<(CfgVar, Ty)>, ret_ty: Ty, cfg: Option<Cfg>) -> Self {
        for p in &params {
            if p.1.is_zero_sized() {
                panic!("Zero-sized parameter not allowed");
            }
        }
        let res = Self {
            name,
            params,
            ret_ty,
            cfg,
        };
        res
    }

    pub fn name(&self) -> FunNameUse {
        Use::from(&self.name)
    }

    pub fn params(&self) -> &Vec<(CfgVar, Ty)> {
        &self.params
    }

    pub fn ret_ty(&self) -> &Ty {
        &self.ret_ty
    }

    pub fn sig(&self) -> Sig {
        Sig::new(
            self.params().iter().map(|(_, x)| x).cloned().collect(),
            self.ret_ty().clone(),
        )
    }

    pub fn cfg(&self) -> &Option<Cfg> {
        &self.cfg
    }
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

#[allow(unused)]
pub struct Program {
    entry: FunNameUse,
    natives: HashMap<String, FunNameUse>,
    funcs: HashSet<Func>,
    boxed_types: HashMap<String, Ty>,
}

impl Program {
    pub fn entry(&self) -> FunNameUse {
        self.entry.clone()
    }

    pub fn natives(&self) -> &HashMap<String, FunNameUse> {
        &self.natives
    }

    pub fn funcs(&self) -> &HashSet<Func> {
        &self.funcs
    }

    pub fn compile<B: Backend>(&self, b: B) -> Result<B::Out, B::Err> {
        b.compile(self)
    }

    pub fn boxed_types(&self) -> &HashMap<String, Ty> {
        &self.boxed_types
    }
}

#[derive(Clone)]
pub struct TyCtx {
    pub sigs: HashMap<FunNameUse, Sig>,
    pub vars: HashMap<CfgVarUse, Ty>,
    pub natives: HashMap<String, FunNameUse>,
}
