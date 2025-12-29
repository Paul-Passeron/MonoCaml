use crate::cfg::expr::Expr;
use crate::cfg::var::CfgVarUse;
use crate::cfg::{
    BasicBlock, Cfg, Const, FunNameUse, Func, Instr, Program, Sig, Terminator, Ty, Value,
};
use crate::helpers::unique::{Extractable, Use};
use std::collections::{HashMap, HashSet};
use std::fmt::Write as FmtWrite;
use std::io::Write;
use std::iter::repeat;

#[derive(Default)]
pub struct ExportC {
    pub f: String,
    pub count: usize,
    pub type_names: HashMap<Ty, String>,
    pub closure_tys: HashMap<(Sig, Ty), String>,
    pub var_aliases: HashMap<CfgVarUse, CfgVarUse>,
}

impl ExportC {
    pub fn create_aliases(&mut self, phis: HashMap<CfgVarUse, HashSet<CfgVarUse>>) {
        self.var_aliases.clear();
        self.var_aliases.extend(
            phis.into_iter()
                .map(|(x, y)| y.into_iter().zip(repeat(x)))
                .flatten(),
        );
    }

    pub fn get_phis_aux(b: &BasicBlock, s: &mut HashMap<CfgVarUse, HashSet<CfgVarUse>>) {
        s.extend(b.phis.iter().map(|(a, b)| (a.clone(), b.clone())));
    }

    pub fn set_phis(&mut self, prog: &Program) {
        let mut s = HashMap::new();
        for f in &prog.funcs {
            if let Some(cfg) = &f.cfg {
                for b in &cfg.blocks {
                    Self::get_phis_aux(b, &mut s);
                }
            }
        }
        self.create_aliases(s);
    }

    // pub fn define_type(&mut self, ty: Ty, decay: bool) {
    //     let mut ignore = false;

    //     if self.type_names.contains_key(&ty) {
    //         return;
    //     }

    //     for (other, name) in &self.type_names {
    //         if other.matches(&ty) {
    //             self.type_names.insert(ty, name.clone());
    //             return;
    //         }
    //     }

    //     let (s, wrote) = match &ty {
    //         Ty::Int => ("int".to_string(), false),
    //         Ty::String => ("const char *".to_string(), false),
    //         Ty::Void => ("void".to_string(), false),
    //         Ty::Ptr(ty) => {
    //             self.define_type(*ty.clone(), decay);
    //             (
    //                 format!(
    //                     "{}*",
    //                     if !decay {
    //                         self.type_names[ty].clone()
    //                     } else {
    //                         self.define_type(Ty::Ptr(ty.clone()), false);
    //                         ignore = true;
    //                         "void".into()
    //                     }
    //                 ),
    //                 false,
    //             )
    //         }
    //         Ty::Struct(items) => {
    //             if ty.repr_closure() {
    //                 items
    //                     .iter()
    //                     .cloned()
    //                     .for_each(|x| self.define_type(x, true));

    //                 let s = ty.field(0).sig();
    //                 let key = &(s, ty.field(1));
    //                 if self.closure_tys.contains_key(key) {
    //                     (self.closure_tys[key].clone(), false)
    //                 } else {
    //                     let name = format!("ty_{}", self.count);
    //                     writeln!(&mut self.f, "typedef struct {{").unwrap();
    //                     items.iter().cloned().enumerate().for_each(|(i, x)| {
    //                         let ty = self.get_type_name(&x);
    //                         writeln!(&mut self.f, "    {} _{i};", &ty).unwrap()
    //                     });
    //                     writeln!(&mut self.f, "}} {name};\n").unwrap();
    //                     self.closure_tys.insert(key.clone(), name.clone());
    //                     (name, true)
    //                 }
    //             } else {
    //                 items
    //                     .iter()
    //                     .cloned()
    //                     .for_each(|x| self.define_type(x, true));
    //                 writeln!(&mut self.f, "typedef struct {{").unwrap();
    //                 items.iter().cloned().enumerate().for_each(|(i, x)| {
    //                     let ty = {
    //                         let s = &self.get_type_name(&x);
    //                         if decay && s.chars().last().unwrap() == '*' {
    //                             "void*".to_string()
    //                         } else {
    //                             s.clone()
    //                         }
    //                     };
    //                     writeln!(&mut self.f, "    {} _{i};", ty).unwrap()
    //                 });
    //                 let name = format!("ty_{}", self.count);
    //                 writeln!(&mut self.f, "}} {name};\n").unwrap();
    //                 (name, true)
    //             }
    //         }
    //         Ty::FunPtr(sig) => {
    //             self.define_type(sig.ret.as_ref().clone(), decay);
    //             sig.params
    //                 .iter()
    //                 .cloned()
    //                 .for_each(|x| self.define_type(x, true));
    //             let name = format!("ty_{}", self.count);
    //             let ty = self.get_type_name(sig.ret.as_ref());
    //             let params = sig
    //                 .params
    //                 .iter()
    //                 .cloned()
    //                 .map(|x| self.get_type_name(&x))
    //                 .collect::<Vec<_>>()
    //                 .join(", ");
    //             let f = &mut self.f;
    //             writeln!(f, "typedef {} (*{})({});\n", ty, name, params).unwrap();
    //             (name, true)
    //         }
    //     };
    //     if wrote {
    //         self.count += 1;
    //     }
    //     if !ignore {
    //         self.type_names.insert(ty, s);
    //     }
    // }

    pub fn define_type(&mut self, ty: Ty, decay: bool) {
        // Normalize empty struct to void
        let ty = match &ty {
            Ty::Struct(items) if items.is_empty() => Ty::Void,
            _ => ty,
        };

        // Handle primitives - these should NEVER get generated names
        match &ty {
            Ty::Int => {
                self.type_names
                    .entry(ty)
                    .or_insert_with(|| "int".to_string());
                return;
            }
            Ty::String => {
                self.type_names
                    .entry(ty)
                    .or_insert_with(|| "const char *".to_string());
                return;
            }
            Ty::Void => {
                self.type_names
                    .entry(ty)
                    .or_insert_with(|| "void".to_string());
                return;
            }
            _ => {}
        }

        // Already defined exactly?
        if self.type_names.contains_key(&ty) {
            return;
        }

        match &ty {
            Ty::Ptr(inner) => {
                self.define_type((**inner).clone(), decay);
                let inner_name = if decay {
                    "void".to_string()
                } else {
                    self.get_type_name(inner)
                };
                self.type_names.insert(ty, format!("{}*", inner_name));
            }

            Ty::FunPtr(sig) => {
                // Define return and param types first
                self.define_type((*sig.ret).clone(), false);
                for param in &sig.params {
                    self.define_type(param.clone(), false);
                }

                // Check if we already have an equivalent function pointer type
                let ret_name = self.get_type_name(&sig.ret);
                let param_names: Vec<_> =
                    sig.params.iter().map(|p| self.get_type_name(p)).collect();

                // Look for existing equivalent
                for (existing_ty, existing_name) in &self.type_names {
                    if let Ty::FunPtr(existing_sig) = existing_ty {
                        let existing_ret = self.get_type_name(&existing_sig.ret);
                        let existing_params: Vec<_> = existing_sig
                            .params
                            .iter()
                            .map(|p| self.get_type_name(p))
                            .collect();

                        if existing_ret == ret_name && existing_params == param_names {
                            self.type_names.insert(ty, existing_name.clone());
                            return;
                        }
                    }
                }

                // Create new typedef
                let name = format!("ty_{}", self.count);
                self.count += 1;

                writeln!(
                    &mut self.f,
                    "typedef {} (*{})({});\n",
                    ret_name,
                    name,
                    param_names.join(", ")
                )
                .unwrap();

                self.type_names.insert(ty, name);
            }

            Ty::Struct(items) => {
                // Define all inner types first
                for item in items {
                    self.define_type(item.clone(), true);
                }

                // Get the string representation of field types
                let field_types: Vec<_> = items
                    .iter()
                    .map(|item| {
                        let s = self.get_type_name(item);
                        if decay && s.ends_with('*') {
                            "void*".to_string()
                        } else {
                            s
                        }
                    })
                    .collect();

                // Check for existing equivalent struct
                for (existing_ty, existing_name) in &self.type_names {
                    if let Ty::Struct(existing_items) = existing_ty {
                        if existing_items.len() == items.len() {
                            let existing_field_types: Vec<_> = existing_items
                                .iter()
                                .map(|item| {
                                    let s = self.get_type_name(item);
                                    if decay && s.ends_with('*') {
                                        "void*".to_string()
                                    } else {
                                        s
                                    }
                                })
                                .collect();

                            if existing_field_types == field_types {
                                self.type_names.insert(ty, existing_name.clone());
                                return;
                            }
                        }
                    }
                }

                // Handle closure type caching
                if ty.repr_closure() {
                    let sig = ty.field(0).sig();
                    let key = (sig.clone(), ty.field(1));
                    if let Some(existing) = self.closure_tys.get(&key) {
                        self.type_names.insert(ty, existing.clone());
                        return;
                    }
                }

                // Create new typedef
                let name = format!("ty_{}", self.count);
                self.count += 1;

                writeln!(&mut self.f, "typedef struct {{").unwrap();
                for (i, field_ty) in field_types.iter().enumerate() {
                    writeln!(&mut self.f, "    {} _{};", field_ty, i).unwrap();
                }
                writeln!(&mut self.f, "}} {};\n", name).unwrap();

                if ty.repr_closure() {
                    let sig = ty.field(0).sig();
                    let key = (sig, ty.field(1));
                    self.closure_tys.insert(key, name.clone());
                }

                self.type_names.insert(ty, name);
            }

            _ => unreachable!("Unhandled type: {:?}", ty),
        }
    }

    fn write_proto(&mut self, f: &Func, alias: Option<String>) {
        let ret_ty_name = self.get_type_name(&f.ret_ty);
        let param_ty_names = f
            .params
            .iter()
            .map(|(name, x)| {
                format!(
                    "{} {}",
                    self.get_type_name(x),
                    format!("_v{}", name.extract())
                )
            })
            .collect::<Vec<_>>();
        write!(
            &mut self.f,
            "{} {}({})",
            ret_ty_name,
            alias
                .as_ref()
                .cloned()
                .unwrap_or_else(|| format!("{}", Use::from(&f.name))),
            param_ty_names.join(", ")
        )
        .unwrap();
    }

    fn forward_declare(&mut self, f: &Func, alias: Option<String>) {
        if f.cfg.is_some() {
            self.write_proto(f, alias.clone());
            writeln!(&mut self.f, ";").unwrap();
        }
        if let Some(alias) = alias {
            writeln!(
                &mut self.f,
                "#define {}(...) {alias}(__VA_ARGS__)",
                Use::from(&f.name)
            )
            .unwrap();
        }
        writeln!(&mut self.f, "").unwrap();
    }

    fn const_as_string(c: &Const) -> String {
        match c {
            Const::Int(i) => i.to_string(),
            Const::String(s) => format!("\"{}\"", s.escape_default()),
            Const::Struct(items) => format!(
                "{{{}}}",
                items
                    .iter()
                    .map(|x| Self::const_as_string(x))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Const::FunPtr(name) => format!("{}", name),
            Const::NullPtr => format!("NULL"),
        }
    }

    fn value_as_string(&self, v: &Value) -> String {
        match v {
            Value::Var(v) => format!("_v{}", v.extract()),
            Value::Const(c) => Self::const_as_string(c),
        }
    }

    fn write_expr(&mut self, e: &Expr) {
        match e {
            Expr::Value(value) => {
                let v = self.value_as_string(value);
                write!(&mut self.f, "{}", v).unwrap()
            }
            Expr::Add(value, value1) => {
                let v1 = self.value_as_string(value);
                let v2 = self.value_as_string(value1);
                write!(&mut self.f, "{} + {}", v1, v2).unwrap()
            }
            Expr::Mul(value, value1) => {
                let v1 = self.value_as_string(value);
                let v2 = self.value_as_string(value1);
                write!(&mut self.f, "{} * {}", v1, v2).unwrap()
            }
            Expr::Sub(value, value1) => {
                let v1 = self.value_as_string(value);
                let v2 = self.value_as_string(value1);
                write!(&mut self.f, "{} - {}", v1, v2).unwrap()
            }
            Expr::Div(value, value1) => {
                let v1 = self.value_as_string(value);
                let v2 = self.value_as_string(value1);
                write!(&mut self.f, "{} / {}", v1, v2).unwrap()
            }
            Expr::NativeCall { fun, args } => {
                let v = self.value_as_string(fun);
                let vals = args
                    .iter()
                    .map(|x| self.value_as_string(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(&mut self.f, "{}({})", v, vals).unwrap()
            }
            Expr::GetElementPtr { ptr, ty, index } => {
                let val = self.value_as_string(ptr);
                let ty = self.get_type_name(ty);
                write!(&mut self.f, "(({}*){})->_{index}", ty, val,).unwrap()
            }
            Expr::Extract { value, index } => {
                let val = self.value_as_string(value);
                write!(&mut self.f, "{}._{index}", val).unwrap()
            }
            Expr::Load { ptr, ty } => {
                let val = self.value_as_string(ptr);
                let ty = self.get_type_name(ty);
                write!(&mut self.f, "*(({}*){})", ty, val,).unwrap();
            }
            Expr::Struct(values) => {
                let values = values
                    .iter()
                    .map(|x| self.value_as_string(x))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(&mut self.f, "{{{}}}", values).unwrap();
            }
            Expr::Malloc(ty, value) => {
                let ty_name = self.get_type_name(ty);
                let val = self.value_as_string(value);
                write!(&mut self.f, "malloc({} * sizeof({ty_name}))", val).unwrap();
            }
            Expr::Phi(_) => (),
        }
    }

    fn write_terminator(&mut self, t: &Terminator) {
        let padding = "        ";
        write!(&mut self.f, "{padding}").unwrap();
        match t {
            Terminator::Return(value) => {
                let ret = match value {
                    Some(v) => format!(" {}", self.value_as_string(v)),
                    None => format!(""),
                };
                writeln!(&mut self.f, "return{};", ret).unwrap()
            }
            Terminator::Goto(lbl) => writeln!(&mut self.f, "goto {lbl};").unwrap(),
            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                let c = self.value_as_string(cond);
                writeln!(&mut self.f, "if ({}) {{", c).unwrap();
                writeln!(&mut self.f, "{padding}    goto {then_bb};").unwrap();
                writeln!(&mut self.f, "{padding}}} else {{").unwrap();
                writeln!(&mut self.f, "{padding}    goto {else_bb};").unwrap();
                writeln!(&mut self.f, "{padding}}}").unwrap();
            }
        }
    }

    fn write_cfg(&mut self, cfg: &Cfg) {
        for (local, ty) in &cfg.locals {
            if ty.is_zero_sized() || self.var_aliases.contains_key(&Use::from(local)) {
                continue;
            }
            let t = self.get_type_name(ty);
            writeln!(&mut self.f, "    {} _v{};", t, local.extract()).unwrap();
        }
        writeln!(&mut self.f, "    goto {};", cfg.entry).unwrap();
        for b in &cfg.blocks {
            writeln!(&mut self.f, "    {}: {{", Use::from(&b.label)).unwrap();
            for instr in &b.instrs {
                write!(&mut self.f, "        ").unwrap();

                match instr {
                    Instr::Assign(var, expr) => {
                        let var_ty = cfg
                            .locals
                            .iter()
                            .find(|(x, _)| x.extract() == var.extract())
                            .unwrap()
                            .1
                            .clone();
                        if !matches!(expr, Expr::Phi(_)) {
                            if !var_ty.is_zero_sized() {
                                let t = self.get_type_name(&var_ty);
                                if matches!(
                                    expr,
                                    Expr::Struct(_) | Expr::Value(Value::Const(Const::Struct(_)))
                                ) {
                                    write!(&mut self.f, "_v{} = ({t})", var.extract()).unwrap();
                                    self.write_expr(expr);
                                    writeln!(&mut self.f, ";").unwrap();
                                } else {
                                    write!(&mut self.f, "_v{} = ", var.extract()).unwrap();
                                    self.write_expr(expr);
                                    writeln!(&mut self.f, ";").unwrap();
                                }
                            } else {
                                self.write_expr(expr);
                                writeln!(&mut self.f, ";").unwrap();
                            }
                        }
                    }
                    Instr::Store { ptr, value } => {
                        let v1 = self.value_as_string(ptr);
                        let v2 = self.value_as_string(value);
                        let ty = if let Value::Var(v) = value {
                            let var_ty = cfg
                                .locals
                                .iter()
                                .find(|(x, _)| x.extract() == v.extract())
                                .unwrap()
                                .1
                                .clone();
                            format!("({}*)", self.get_type_name(&var_ty))
                        } else {
                            String::new()
                        };
                        writeln!(&mut self.f, "*{}{} = {};", ty, v1, v2).unwrap()
                    }
                }
            }
            self.write_terminator(&b.terminator);
            writeln!(&mut self.f, "    }}").unwrap();
        }
    }

    fn declare(&mut self, f: &Func, alias: Option<String>) {
        self.write_proto(f, alias.clone());
        writeln!(&mut self.f, "{{").unwrap();
        self.write_cfg(f.cfg.as_ref().unwrap());
        writeln!(&mut self.f, "}}").unwrap();
    }

    fn declare_main(&mut self, entry: &FunNameUse) {
        writeln!(&mut self.f, "void start(void) {{ {entry}(); }}\n").unwrap();
    }

    fn get_type_name(&self, ty: &Ty) -> String {
        match ty {
            Ty::Int => return "int".into(),
            Ty::String => return "const char *".into(),
            Ty::Void => return "void".into(),
            Ty::Ptr(_) => (),
            Ty::Struct(items) if items.is_empty() => return "void".into(),
            Ty::FunPtr(_) => (),
            Ty::Struct(_) => (),
        }
        if self.type_names.contains_key(ty) {
            self.type_names.get(ty).unwrap().clone()
        } else {
            for (k, v) in self.type_names.iter() {
                if k.matches(ty) {
                    return v.clone();
                }
            }
            panic!("Could not find def for type {ty}");
        }
    }

    pub fn export(&mut self, prog: &Program) {
        writeln!(&mut self.f, "#include \"runtime.h\"").unwrap();

        let types = prog.get_all_types();
        for ty in types {
            self.define_type(ty, false);
        }

        let rev_map = prog
            .natives
            .iter()
            .map(|(alias, true_name)| (true_name.clone(), alias.clone()))
            .collect::<HashMap<_, _>>();

        for f in &prog.funcs {
            self.forward_declare(f, rev_map.get(&Use::from(&f.name)).cloned());
        }

        self.set_phis(prog);
        writeln!(&mut self.f, "\n").unwrap();
        for (orig, alias) in &self.var_aliases {
            let alias = self.value_as_string(&Value::Var(alias.clone()));
            let orig = self.value_as_string(&Value::Var(orig.clone()));
            writeln!(&mut self.f, "#define {} {}", orig, alias).unwrap();
        }

        self.declare_main(&prog.entry);

        for f in &prog.funcs {
            if f.cfg.is_some() {
                self.declare(f, rev_map.get(&Use::from(&f.name)).cloned());
            }
        }
    }
}

impl Cfg {
    pub fn get_all_types(&self) -> HashSet<Ty> {
        HashSet::from_iter(self.locals.values().cloned())
    }
}

fn add_type(s: &mut HashSet<Ty>, ty: &Ty) {
    if s.contains(ty) {
        return;
    }
    s.insert(ty.clone());
    match ty {
        Ty::Ptr(ty) => add_type(s, ty),
        Ty::Struct(items) => items.iter().for_each(|x| add_type(s, x)),
        Ty::FunPtr(sig) => {
            add_type(s, &sig.ret);
            sig.params.iter().for_each(|x| add_type(s, x));
        }
        _ => (),
    }
}

impl Program {
    fn get_all_types(&self) -> HashSet<Ty> {
        let mut s = HashSet::new();
        for f in &self.funcs {
            f.params.iter().for_each(|(_, x)| add_type(&mut s, x));
            add_type(&mut s, &f.ret_ty);
            f.cfg
                .iter()
                .map(|x| x.get_all_types())
                .flatten()
                .for_each(|x| add_type(&mut s, &x));
        }
        s
    }

    pub fn export_to_c(&self, f: &mut std::fs::File) {
        let mut exporter = ExportC::default();
        exporter.export(self);
        writeln!(f, "{}", exporter.f).unwrap();
    }
}
