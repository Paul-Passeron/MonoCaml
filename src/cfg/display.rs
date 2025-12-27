use std::fmt;

use crate::{
    cfg::{
        BasicBlock, Cfg, Const, Expr, FunName, Func, Instr, Label, Program, Sig, Terminator, Ty,
        Value,
        var::{CfgGlobal, CfgVar, VarKind},
    },
    helpers::unique::{Extractable, UniqueDisplayer, Use},
};

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Int => write!(f, "int"),
            Ty::String => write!(f, "string"),
            Ty::Void => write!(f, "void"),
            Ty::Ptr(ty) => write!(f, "*{ty}"),
            Ty::Struct(items) => write!(
                f,
                "{{ {} }}",
                items
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Ty::FunPtr(sig) => write!(f, "{sig}"),
        }
    }
}

impl fmt::Display for Sig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({}) -> {}",
            self.params
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            self.ret
        )
    }
}

impl<T, S> fmt::Display for Use<T, S>
where
    S: Clone,
    T: UniqueDisplayer<S>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", T::unique_displayer(self.extract()))
    }
}

impl UniqueDisplayer for CfgVar {
    fn unique_displayer(x: usize) -> String {
        format!("%{x}")
    }
}

impl UniqueDisplayer for CfgGlobal {
    fn unique_displayer(x: usize) -> String {
        format!("@{x}")
    }
}

impl UniqueDisplayer for Label {
    fn unique_displayer(x: usize) -> String {
        format!("L{x}")
    }
}

impl UniqueDisplayer for FunName {
    fn unique_displayer(x: usize) -> String {
        format!("function_{x}")
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Const::Int(x) => write!(f, "{x}"),
            Const::String(s) => write!(f, "\"{}\"", s.escape_debug()),
            Const::Struct(items) => {
                write!(
                    f,
                    "{{ {} }}",
                    items
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Const::FunPtr(n) => write!(f, "{n}"),
            Const::NullPtr => write!(f, "null"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Global(x) => write!(f, "{x}"),
            Value::Var(x) => write!(f, "{x}"),
            Value::Const(x) => write!(f, "{x}"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Add(a, b) => write!(f, "add {a} {b}"),
            Expr::Mul(a, b) => write!(f, "mul {a} {b}"),
            Expr::Sub(a, b) => write!(f, "sub {a} {b}"),
            Expr::Div(a, b) => write!(f, "div {a} {b}"),
            Expr::Call { closure, arg } => write!(f, "call {closure} {arg}"),
            Expr::NativeCall { fun, args } => write!(
                f,
                "native call {}({})",
                fun,
                args.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expr::GetElementPtr { ptr, index } => write!(f, "getelementptr {} {}", ptr, index),
            Expr::Extract { value, index } => write!(f, "getelementptr {} {}", value, index),
            Expr::Load { ptr, ty } => write!(f, "load {} as {}", ptr, ty),
            Expr::Struct(values) => write!(
                f,
                "{{ {} }}",
                values
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl<T> fmt::Display for Instr<T>
where
    T: VarKind + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Assign(val, expr) => write!(f, "{val} = {expr}"),
            Instr::Store { ptr, value } => write!(f, "store {value} at {ptr}"),
        }
    }
}

pub fn pad_text(text: &str, spaces: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let padder = " ".repeat(spaces);
    for l in text.lines() {
        writeln!(f, "{}{}", padder, l)?;
    }
    Ok(())
}

impl fmt::Display for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for block in &self.blocks {
            pad_text(&block.to_string()[..], 4, f)?;
        }

        Ok(())
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Return(value) => write!(
                f,
                "return {}",
                match value {
                    Some(x) => x.to_string(),
                    None => "".to_string(),
                }
            ),
            Terminator::Goto(lbl) => write!(f, "goto {lbl}"),
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => write!(f, "branch {} {} {}", cond, then_bb, else_bb),
        }
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lbl: Use<_> = (&self.label).into();
        writeln!(f, "{lbl}:")?;
        for instr in &self.instrs {
            writeln!(f, "    {instr}")?;
        }
        writeln!(f, "    {}", self.terminator)
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.cfg {
            Some(cfg) => writeln!(
                f,
                "func {} ({}) -> {} {{\n{}\n}}",
                self.name.0,
                self.params
                    .iter()
                    .map(|(a, b)| {
                        let v: Use<_> = a.into();
                        format!("{v}: {b}")
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
                self.ret_ty,
                cfg
            ),
            None => writeln!(
                f,
                "external func {} ({}) -> {}",
                self.name.0,
                self.params
                    .iter()
                    .map(|(a, b)| {
                        let v: Use<_> = a.into();
                        format!("{v}: {b}")
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
                self.ret_ty
            ),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (glob, cte) in &self.globals {
            let g: Use<_> = glob.into();
            writeln!(f, "{} = {}", g, cte)?;
        }
        writeln!(f, "")?;

        let mut funcs = self.funcs.iter().collect::<Vec<_>>();
        funcs.sort_by(|a, b| a.name.cmp(&b.name));
        for fu in funcs {
            write!(f, "{}\n", fu)?;
        }
        Ok(())
    }
}
