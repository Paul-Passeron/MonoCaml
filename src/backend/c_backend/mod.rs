use crate::backend::Backend;
use crate::cfg::expr::Expr;
use crate::cfg::var::CfgVarUse;
use crate::cfg::{Cfg, Const, FunNameUse, Func, Instr, Program, Sig, Terminator, Ty, Value};
use crate::helpers::unique::{Extractable, Use};
use std::collections::{HashMap, HashSet};
use std::fmt::Write as FmtWrite;
use std::fs::File;
use std::io::Write;
use std::path::Path;

pub struct ExportC<P: AsRef<Path>> {
    pub f: String,
    pub count: usize,
    pub type_names: HashMap<Ty, String>,
    pub decayed_type_names: HashMap<Ty, String>,
    pub closure_tys: HashMap<(Sig, Ty), String>,
    pub var_aliases: HashMap<CfgVarUse, CfgVarUse>,
    pub output_file: P,
}

impl<P: AsRef<Path>> ExportC<P> {
    /// Compute the canonical decayed representation of a type as a string.
    /// This doesn't require the type to be defined yet.
    fn canonical_decayed(&self, ty: &Ty) -> String {
        match ty {
            Ty::Int => "int".to_string(),
            Ty::String => "const char *".to_string(),
            Ty::Void => "void".to_string(),
            Ty::Struct(items) if items.is_empty() => "void".to_string(),
            Ty::Ptr(_) => "void*".to_string(),
            Ty::FunPtr(sig) => {
                let ret = self.canonical_decayed(sig.ret());
                let params: Vec<_> = sig
                    .params()
                    .iter()
                    .map(|p| self.canonical_decayed(p))
                    .collect();
                let params_str = if params.is_empty() {
                    "void".to_string()
                } else {
                    params.join(", ")
                };
                format!("{} (*)({})", ret, params_str)
            }
            Ty::Struct(items) => {
                let fields: Vec<_> = items
                    .iter()
                    .map(|item| self.canonical_decayed(item))
                    .collect();
                format!("{{{}}}", fields.join(", "))
            }
            Ty::Union(items) => {
                let fields: Vec<_> = items
                    .iter()
                    .map(|item| self.canonical_decayed(item))
                    .collect();
                format!("union {{{}}}", fields.join(", "))
            }
        }
    }

    pub fn define_type(&mut self, ty: Ty) {
        // Normalize empty struct to void
        let ty = match &ty {
            Ty::Struct(items) if items.is_empty() => Ty::Void,
            _ => ty,
        };

        // Already defined?
        if self.type_names.contains_key(&ty) {
            return;
        }

        match &ty {
            Ty::Int => {
                self.type_names.insert(ty.clone(), "int".to_string());
                self.decayed_type_names.insert(ty, "int".to_string());
            }
            Ty::String => {
                self.type_names
                    .insert(ty.clone(), "const char *".to_string());
                self.decayed_type_names
                    .insert(ty, "const char *".to_string());
            }
            Ty::Void => {
                self.type_names.insert(ty.clone(), "void".to_string());
                self.decayed_type_names.insert(ty, "void".to_string());
            }
            Ty::Ptr(inner) => {
                self.define_type((**inner).clone());
                let inner_name = self.get_type_name(inner);
                self.type_names
                    .insert(ty.clone(), format!("{}*", inner_name));
                self.decayed_type_names.insert(ty, "void*".to_string());
            }
            Ty::FunPtr(sig) => {
                // Define return and param types first (recursively)
                self.define_type((sig.ret()).clone());
                for param in sig.params() {
                    self.define_type(param.clone());
                }

                // Compute canonical decayed signature for equivalence
                let my_canonical = self.canonical_decayed(&ty);

                // Look for existing equivalent function pointer
                let existing = self
                    .type_names
                    .iter()
                    .filter_map(|(existing_ty, existing_name)| {
                        if let Ty::FunPtr(_) = existing_ty {
                            if self.canonical_decayed(existing_ty) == my_canonical {
                                return Some(existing_name.clone());
                            }
                        }
                        None
                    })
                    .next();

                if let Some(existing_name) = existing {
                    self.type_names.insert(ty.clone(), existing_name.clone());
                    self.decayed_type_names.insert(ty, existing_name);
                    return;
                }

                // Create new typedef using decayed types
                let name = format!("ty_{}", self.count);
                self.count += 1;

                let decayed_ret = self.get_decayed_type_name(&sig.ret());
                let decayed_params: Vec<_> = sig
                    .params()
                    .iter()
                    .map(|p| self.get_decayed_type_name(p))
                    .collect();

                writeln!(
                    &mut self.f,
                    "typedef {} (*{})({});\n",
                    decayed_ret,
                    name,
                    if decayed_params.is_empty() {
                        "void".to_string()
                    } else {
                        decayed_params.join(", ")
                    }
                )
                .unwrap();

                self.type_names.insert(ty.clone(), name.clone());
                self.decayed_type_names.insert(ty, name);
            }
            Ty::Struct(items) => {
                // Define all inner types first
                for item in items {
                    self.define_type(item.clone());
                }

                // Compute canonical decayed representation for equivalence
                let my_canonical = self.canonical_decayed(&ty);

                // Check for existing equivalent struct
                let existing = self
                    .type_names
                    .iter()
                    .filter_map(|(existing_ty, existing_name)| {
                        if let Ty::Struct(_) = existing_ty {
                            if self.canonical_decayed(existing_ty) == my_canonical {
                                match self.decayed_type_names.get(existing_ty) {
                                    Some(decayed_name) => {
                                        return Some((existing_name.clone(), decayed_name.clone()));
                                    }
                                    None => {
                                        return Some((existing_name.clone(), my_canonical.clone()));
                                    }
                                }
                            }
                        }
                        None
                    })
                    .next();

                if let Some((existing_name, decayed_name)) = existing {
                    self.type_names.insert(ty.clone(), existing_name);
                    self.decayed_type_names.insert(ty, decayed_name);
                    return;
                }

                // Handle closure type caching
                if ty.repr_closure() {
                    let sig = ty.field(0).sig();
                    let key = (sig.clone(), ty.field(1));
                    if let Some(existing) = self.closure_tys.get(&key) {
                        self.type_names.insert(ty.clone(), existing.clone());
                        self.decayed_type_names.insert(ty, existing.clone());
                        return;
                    }
                }

                // Create new typedef using decayed field types
                let name = format!("ty_{}", self.count);
                self.count += 1;

                let decayed_field_types: Vec<_> = items
                    .iter()
                    .map(|item| self.get_decayed_type_name(item))
                    .collect();

                writeln!(&mut self.f, "typedef struct {{").unwrap();
                for (i, field_ty) in decayed_field_types.iter().enumerate() {
                    writeln!(&mut self.f, "    {} _{};", field_ty, i).unwrap();
                }
                writeln!(&mut self.f, "}} {};\n", name).unwrap();

                if ty.repr_closure() {
                    let sig = ty.field(0).sig();
                    let key = (sig, ty.field(1));
                    self.closure_tys.insert(key, name.clone());
                }

                self.type_names.insert(ty.clone(), name.clone());
                self.decayed_type_names.insert(ty, name);
            }
            Ty::Union(items) => {
                // Define all inner types first
                for item in items {
                    self.define_type(item.clone());
                }

                // Compute canonical decayed representation for equivalence
                let my_canonical = self.canonical_decayed(&ty);

                // Check for existing equivalent struct
                let existing = self
                    .type_names
                    .iter()
                    .filter_map(|(existing_ty, existing_name)| {
                        if let Ty::Struct(_) = existing_ty {
                            if self.canonical_decayed(existing_ty) == my_canonical {
                                match self.decayed_type_names.get(existing_ty) {
                                    Some(decayed_name) => {
                                        return Some((existing_name.clone(), decayed_name.clone()));
                                    }
                                    None => {
                                        return Some((existing_name.clone(), my_canonical.clone()));
                                    }
                                }
                            }
                        }
                        None
                    })
                    .next();

                if let Some((existing_name, decayed_name)) = existing {
                    self.type_names.insert(ty.clone(), existing_name);
                    self.decayed_type_names.insert(ty, decayed_name);
                    return;
                }

                // Create new typedef using decayed field types
                let name = format!("ty_{}", self.count);
                self.count += 1;

                let decayed_field_types: Vec<_> = items
                    .iter()
                    .map(|item| self.get_decayed_type_name(item))
                    .collect();

                writeln!(&mut self.f, "typedef union {{").unwrap();
                for (i, field_ty) in decayed_field_types.iter().enumerate() {
                    writeln!(&mut self.f, "    {} _{};", field_ty, i).unwrap();
                }
                writeln!(&mut self.f, "}} {};\n", name).unwrap();

                self.type_names.insert(ty.clone(), name.clone());
                self.decayed_type_names.insert(ty, name);
            }
        }
    }

    pub fn get_type_name(&self, ty: &Ty) -> String {
        let ty = match ty {
            Ty::Struct(items) if items.is_empty() => &Ty::Void,
            _ => ty,
        };

        if let Some(name) = self.type_names.get(ty) {
            return name.clone();
        }

        match ty {
            Ty::Int => "int".to_string(),
            Ty::String => "const char *".to_string(),
            Ty::Void => "void".to_string(),
            Ty::FunPtr(_) | Ty::Ptr(_) => self.canonical_decayed(ty),
            _ => panic!("No type but there should be !"),
        }
    }

    pub fn get_decayed_type_name(&self, ty: &Ty) -> String {
        let ty = match ty {
            Ty::Struct(items) if items.is_empty() => &Ty::Void,
            _ => ty,
        };

        if let Some(name) = self.decayed_type_names.get(ty) {
            return name.clone();
        }

        match ty {
            Ty::Int => "int".to_string(),
            Ty::String => "const char *".to_string(),
            Ty::Void => "void".to_string(),
            Ty::Ptr(_) => "void*".to_string(),
            _ => panic!("No type but there should be !"),
        }
    }

    fn write_proto(&mut self, f: &Func, alias: Option<String>) {
        let ret_ty_name = self.get_decayed_type_name(f.ret_ty());
        let param_ty_names = f
            .params()
            .iter()
            .map(|(name, x)| {
                format!(
                    "{} {}",
                    self.get_decayed_type_name(x),
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
                .unwrap_or_else(|| format!("{}", f.name())),
            param_ty_names.join(", ")
        )
        .unwrap();
    }

    fn forward_declare(&mut self, f: &Func, alias: Option<String>) {
        if f.cfg().is_some() {
            self.write_proto(f, alias.clone());
            writeln!(&mut self.f, ";").unwrap();
        }
        if let Some(alias) = alias {
            writeln!(
                &mut self.f,
                "#define {}(...) {alias}(__VA_ARGS__)",
                f.name()
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
                write!(&mut self.f, "&(({}*){})->_{index}", ty, val,).unwrap()
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
            Expr::Eq(value, value1) => {
                let v1 = self.value_as_string(value);
                let v2 = self.value_as_string(value1);
                write!(&mut self.f, "{} == {}", v1, v2).unwrap()
            }
            Expr::Cast(ty, value) => {
                let ty_name = self.get_type_name(ty);
                let val = self.value_as_string(value);
                write!(&mut self.f, "*({ty_name}*)&{val}").unwrap();
            }
            Expr::Alloca(_) | Expr::Union(_, _, _) => unreachable!(),
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

    fn write_cfg(&mut self, cfg: &Cfg, params: Vec<(CfgVarUse, Ty)>) {
        for (local, ty) in cfg.locals() {
            if ty.is_zero_sized() || self.var_aliases.contains_key(&Use::from(local)) {
                continue;
            }
            let t = self.get_type_name(ty);
            writeln!(&mut self.f, "    {} _v{};", t, local.extract()).unwrap();
        }
        writeln!(&mut self.f, "    goto {};", cfg.entry()).unwrap();
        for b in cfg.blocks() {
            writeln!(&mut self.f, "    {}: {{", b.label()).unwrap();
            for instr in b.instrs() {
                write!(&mut self.f, "        ").unwrap();

                match instr {
                    Instr::Assign(var, expr) => {
                        let var_ty = cfg
                            .locals()
                            .iter()
                            .find(|(x, _)| x.extract() == var.extract())
                            .unwrap()
                            .1
                            .clone();
                        if matches!(expr, Expr::Alloca(_)) {
                            match expr {
                                Expr::Alloca(ty) => {
                                    let type_name = self.get_type_name(&ty);
                                    writeln!(&mut self.f, "{type_name} loc_{};", var.extract())
                                        .unwrap();
                                    write!(&mut self.f, "        ").unwrap();
                                    writeln!(
                                        &mut self.f,
                                        "_v{} = &loc_{};",
                                        var.extract(),
                                        var.extract()
                                    )
                                    .unwrap();
                                }
                                _ => unreachable!(),
                            }
                        } else if matches!(expr, Expr::Union(_, _, _)) {
                            match expr {
                                Expr::Union(ty, val, field) => {
                                    let val_str = self.value_as_string(val);
                                    let t = self.get_type_name(&ty.field(*field));
                                    writeln!(
                                        &mut self.f,
                                        "_v{}._{field} = ({t}){};",
                                        var.extract(),
                                        val_str
                                    )
                                    .unwrap();
                                }
                                _ => unreachable!(),
                            }
                        } else {
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
                        println!("Looking for {value}");
                        let ty = if let Value::Var(v) = value {
                            let var_ty = cfg
                                .locals()
                                .iter()
                                .map(|(x, ty)| (Use::from(x), ty))
                                .chain(params.iter().map(|(x, y)| (x.clone(), y)))
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
            self.write_terminator(b.terminator());
            writeln!(&mut self.f, "    }}").unwrap();
        }
    }

    fn declare(&mut self, f: &Func, alias: Option<String>) {
        self.write_proto(f, alias.clone());
        writeln!(&mut self.f, "{{").unwrap();
        self.write_cfg(
            f.cfg().as_ref().unwrap(),
            f.params()
                .iter()
                .map(|(x, y)| (Use::from(x), y.clone()))
                .collect(),
        );
        writeln!(&mut self.f, "}}").unwrap();
    }

    fn declare_main(&mut self, entry: &FunNameUse) {
        writeln!(&mut self.f, "void start(void) {{ {entry}(); }}\n").unwrap();
    }

    pub fn export(&mut self, prog: &Program) {
        writeln!(&mut self.f, "#include \"runtime.h\"").unwrap();
        writeln!(
            &mut self.f,
            "#pragma clang diagnostic ignored \"-Wincompatible-pointer-types\""
        )
        .unwrap();
        let types = prog.get_all_types();
        for ty in types {
            self.define_type(ty);
        }

        let rev_map = prog
            .natives()
            .iter()
            .map(|(alias, true_name)| (true_name.clone(), alias.clone()))
            .collect::<HashMap<_, _>>();

        for f in prog.funcs() {
            self.forward_declare(f, rev_map.get(&f.name()).cloned());
        }

        writeln!(&mut self.f, "\n").unwrap();
        for (orig, alias) in &self.var_aliases {
            let alias = self.value_as_string(&Value::Var(alias.clone()));
            let orig = self.value_as_string(&Value::Var(orig.clone()));
            writeln!(&mut self.f, "#define {} {}", orig, alias).unwrap();
        }

        self.declare_main(&prog.entry());

        for f in prog.funcs() {
            if f.cfg().is_some() {
                if f.name() == prog.entry() {
                    write!(&mut self.f, "__inline__ ").unwrap();
                }
                self.declare(f, rev_map.get(&f.name()).cloned());
            }
        }
    }

    pub fn new(p: P) -> Self {
        Self {
            f: Default::default(),
            count: Default::default(),
            type_names: Default::default(),
            decayed_type_names: Default::default(),
            closure_tys: Default::default(),
            var_aliases: Default::default(),
            output_file: p,
        }
    }
}

impl Cfg {
    pub fn get_all_types(&self) -> HashSet<Ty> {
        HashSet::from_iter(self.locals().values().cloned())
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
            add_type(s, &sig.ret());
            sig.params().iter().for_each(|x| add_type(s, x));
        }
        _ => (),
    }
}

impl<P: AsRef<Path>> Backend for ExportC<P> {
    type Out = ();

    type Err = ();

    fn compile(self, prog: &Program) -> Result<(), ()> {
        let mut this = self;
        this.export(prog);
        let mut f = File::create(this.output_file).map_err(|_| ())?;
        f.write_all(this.f.as_bytes()).unwrap();
        Ok(())
    }
}

impl Program {
    fn get_all_types(&self) -> HashSet<Ty> {
        let mut s = HashSet::new();
        for f in self.funcs() {
            f.params().iter().for_each(|(_, x)| add_type(&mut s, x));
            add_type(&mut s, f.ret_ty());
            f.cfg()
                .iter()
                .map(|x| x.get_all_types())
                .flatten()
                .for_each(|x| add_type(&mut s, &x));
        }
        s
    }
}
