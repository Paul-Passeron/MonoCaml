use crate::cfg::expr::Expr;
use crate::cfg::{Cfg, Const, FunNameUse, Func, Instr, Program, Sig, Terminator, Ty, Value};
use crate::helpers::unique::{Extractable, Use};
use std::collections::{HashMap, HashSet};
use std::fmt::Write as FmtWrite;
use std::io::Write;

#[derive(Default)]
pub struct ExportC {
    pub f: String,
    pub count: usize,
    pub type_names: HashMap<Ty, String>,
    pub closure_tys: HashMap<(Sig, Ty), String>,
}

impl ExportC {
    pub fn define_type(&mut self, ty: Ty, decay: bool) {
        let mut ignore = false;

        if self.type_names.contains_key(&ty) {
            return;
        }

        for (other, name) in &self.type_names {
            if other.matches(&ty) {
                self.type_names.insert(ty, name.clone());
                return;
            }
        }

        let (s, wrote) = match &ty {
            Ty::Int => ("int".to_string(), false),
            Ty::String => ("const char *".to_string(), false),
            Ty::Void => ("void".to_string(), false),
            Ty::Ptr(ty) => {
                self.define_type(*ty.clone(), decay);
                (
                    format!(
                        "{}*",
                        if !decay {
                            self.type_names[ty].clone()
                        } else {
                            self.define_type(Ty::Ptr(ty.clone()), false);
                            ignore = true;
                            "void".into()
                        }
                    ),
                    false,
                )
            }
            Ty::Struct(items) => {
                if ty.repr_closure() {
                    items
                        .iter()
                        .cloned()
                        .for_each(|x| self.define_type(x, true));

                    let s = ty.field(0).sig();
                    let key = &(s, ty.field(1));
                    if self.closure_tys.contains_key(key) {
                        (self.closure_tys[key].clone(), false)
                    } else {
                        let name = format!("ty_{}", self.count);
                        let f = &mut self.f;
                        writeln!(f, "typedef struct {{").unwrap();
                        items.iter().cloned().enumerate().for_each(|(i, x)| {
                            writeln!(f, "    {} _{i};", &self.type_names[&x]).unwrap()
                        });
                        writeln!(f, "}} {name};\n").unwrap();
                        self.closure_tys.insert(key.clone(), name.clone());
                        (name, true)
                    }
                } else {
                    items
                        .iter()
                        .cloned()
                        .for_each(|x| self.define_type(x, true));

                    let f = &mut self.f;
                    writeln!(f, "typedef struct {{").unwrap();
                    items.iter().cloned().enumerate().for_each(|(i, x)| {
                        writeln!(f, "    {} _{i};", {
                            let s = &self.type_names[&x];
                            if decay && s.chars().last().unwrap() == '*' {
                                "void*".to_string()
                            } else {
                                s.clone()
                            }
                        })
                        .unwrap()
                    });
                    let name = format!("ty_{}", self.count);
                    writeln!(f, "}} {name};\n").unwrap();
                    (name, true)
                }
            }
            Ty::FunPtr(sig) => {
                self.define_type(sig.ret.as_ref().clone(), decay);
                sig.params
                    .iter()
                    .cloned()
                    .for_each(|x| self.define_type(x, true));
                let name = format!("ty_{}", self.count);
                let f = &mut self.f;
                writeln!(
                    f,
                    "typedef {} (*{})({});\n",
                    self.type_names[sig.ret.as_ref()],
                    name,
                    sig.params
                        .iter()
                        .cloned()
                        .map(|x| self.type_names[&x].to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
                .unwrap();
                (name, true)
            }
        };
        if wrote {
            self.count += 1;
        }
        if !ignore {
            self.type_names.insert(ty, s);
        }
    }

    fn write_proto(&mut self, f: &Func, alias: Option<String>) {
        let ret_ty_name = self.type_names[&f.ret_ty].to_string();
        let param_ty_names = f
            .params
            .iter()
            .map(|(name, x)| {
                format!(
                    "{} {}",
                    self.type_names[x].to_string(),
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
            if let Some(alias) = alias {
                writeln!(
                    &mut self.f,
                    "#define {}(...) {alias}(__VA_ARGS__)",
                    Use::from(&f.name)
                )
                .unwrap();
            }
        }
        writeln!(&mut self.f, "").unwrap();
    }

    fn const_as_string(c: &Const) -> String {
        match c {
            Const::Int(i) => i.to_string(),
            Const::String(s) => format!("\"{}\"", s.escape_default()),
            Const::Struct(items) => format!(
                "{{ {} }}",
                items
                    .iter()
                    .map(|x| Self::const_as_string(x))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            Const::FunPtr(name) => format!("{}", name),
            Const::NullPtr => format!("NULL"),
        }
    }

    fn value_as_string(v: &Value) -> String {
        match v {
            Value::Var(v) => format!("_v{}", v.extract()),
            Value::Const(c) => Self::const_as_string(c),
        }
    }

    fn write_expr(&mut self, e: &Expr) {
        match e {
            Expr::Value(value) => write!(&mut self.f, "{}", Self::value_as_string(value)).unwrap(),
            Expr::Add(value, value1) => write!(
                &mut self.f,
                "{} + {}",
                Self::value_as_string(value),
                Self::value_as_string(value1)
            )
            .unwrap(),
            Expr::Mul(value, value1) => write!(
                &mut self.f,
                "{} * {}",
                Self::value_as_string(value),
                Self::value_as_string(value1)
            )
            .unwrap(),
            Expr::Sub(value, value1) => write!(
                &mut self.f,
                "{} - {}",
                Self::value_as_string(value),
                Self::value_as_string(value1)
            )
            .unwrap(),
            Expr::Div(value, value1) => write!(
                &mut self.f,
                "{} / {}",
                Self::value_as_string(value),
                Self::value_as_string(value1)
            )
            .unwrap(),
            Expr::Call { closure, arg } => write!(
                &mut self.f,
                "({}._0)({}._1, {})",
                Self::value_as_string(closure),
                Self::value_as_string(closure),
                Self::value_as_string(arg)
            )
            .unwrap(),
            Expr::NativeCall { fun, args } => write!(
                &mut self.f,
                "{fun}({})",
                args.iter()
                    .map(|x| Self::value_as_string(x))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .unwrap(),
            Expr::GetElementPtr { ptr, ty, index } => write!(
                &mut self.f,
                "(({}*){})->_{index}",
                &self.type_names[ty],
                Self::value_as_string(ptr),
            )
            .unwrap(),
            Expr::Extract { value, index } => {
                write!(&mut self.f, "{}._{index}", Self::value_as_string(value)).unwrap()
            }
            Expr::Load { ptr, ty } => write!(
                &mut self.f,
                "*(({}*){})",
                &self.type_names[ty],
                Self::value_as_string(ptr),
            )
            .unwrap(),
            Expr::Struct(values) => write!(
                &mut self.f,
                "{{ {} }}",
                values
                    .iter()
                    .map(|x| Self::value_as_string(x))
                    .collect::<Vec<_>>()
                    .join(",")
            )
            .unwrap(),
            Expr::Malloc(ty, value) => {
                let ty_name = self.type_names[ty].clone();
                write!(
                    &mut self.f,
                    "malloc({} * sizeof({ty_name}))",
                    Self::value_as_string(value)
                )
                .unwrap();
            }
        }
    }

    fn write_terminator(&mut self, t: &Terminator) {
        write!(&mut self.f, "        ").unwrap();
        match t {
            Terminator::Return(value) => writeln!(
                &mut self.f,
                "return{};",
                match value {
                    Some(v) => format!(" {}", Self::value_as_string(v)),
                    None => format!(""),
                }
            )
            .unwrap(),
            Terminator::Goto(lbl) => writeln!(&mut self.f, "goto {lbl};").unwrap(),
            Terminator::Branch { .. } => todo!(),
        }
    }

    fn write_cfg(&mut self, cfg: &Cfg) {
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

                        if !var_ty.is_void() {
                            write!(
                                &mut self.f,
                                "{} _v{} = ",
                                self.type_names[&var_ty],
                                var.extract()
                            )
                            .unwrap();
                        }
                        self.write_expr(expr);
                        writeln!(&mut self.f, ";").unwrap();
                    }
                    Instr::Store { ptr, value } => writeln!(
                        &mut self.f,
                        "*{}{} = {};",
                        if let Value::Var(v) = value {
                            let var_ty = cfg
                                .locals
                                .iter()
                                .find(|(x, _)| x.extract() == v.extract())
                                .unwrap()
                                .1
                                .clone();
                            format!("({}*)", &self.type_names[&var_ty])
                        } else {
                            String::new()
                        },
                        Self::value_as_string(ptr),
                        Self::value_as_string(value)
                    )
                    .unwrap(),
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
        writeln!(&mut self.f, "void start(void) {{ {entry}(); }}").unwrap();
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
