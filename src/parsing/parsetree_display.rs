use std::fmt::{self, Display};

use crate::parsing::{
    asttypes::{
        ArgLabel, ClosedFlag, DirectionFlag, Injectivity, Loc, MutableFlag, OverrideFlag,
        PrivateFlag, RecFlag, Variance, VirtualFlag,
    },
    longident::LongIdent,
};

// Import all the types from your module
use crate::parsing::parsetree::*;

// Helper function to join items with a separator
fn join_display<T: Display>(items: &[T], sep: &str) -> String {
    items
        .iter()
        .map(|item| item.to_string())
        .collect::<Vec<_>>()
        .join(sep)
}

// Helper for displaying Loc<T>
impl<T: Display> Display for Loc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.txt)
    }
}

impl Display for LongIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LongIdent::Ident(i) => write!(f, "{}", i),
            LongIdent::Dot(loc, loc1) => write!(f, "{}.{}", loc, loc1),
            LongIdent::Apply(loc, loc1) => write!(f, "({} {})", loc, loc1),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.const_desc)
    }
}

impl Display for ConstantDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstantDesc::Integer(s, suffix) => {
                write!(f, "{}", s)?;
                if let Some(c) = suffix {
                    write!(f, "{}", c)?;
                }
                Ok(())
            }
            ConstantDesc::Char(c) => write!(f, "'{}'", c),
            ConstantDesc::String(s, _, delim) => {
                if let Some(d) = delim {
                    write!(f, "{{{}}}", d)
                } else {
                    write!(f, "\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
                }
            }
            ConstantDesc::Float(s, suffix) => {
                write!(f, "{}", s)?;
                if let Some(c) = suffix {
                    write!(f, "{}", c)?;
                }
                Ok(())
            }
        }
    }
}

impl Display for CoreType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.type_desc)
    }
}

impl Display for CoreTypeDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CoreTypeDesc::Any => write!(f, "_"),
            CoreTypeDesc::Var(s) => write!(f, "'{}", s),
            CoreTypeDesc::Arrow(label, t1, t2) => match label {
                ArgLabel::NoLabel => write!(f, "{} -> {}", t1, t2),
                ArgLabel::Labelled(s) => write!(f, "~{}:{} -> {}", s, t1, t2),
                ArgLabel::Optional(s) => write!(f, "?{}:{} -> {}", s, t1, t2),
            },
            CoreTypeDesc::Tuple(items) => {
                let parts: Vec<String> = items
                    .iter()
                    .map(|(label, ty)| {
                        if let Some(l) = label {
                            format!("~{}: {}", l, ty)
                        } else {
                            ty.to_string()
                        }
                    })
                    .collect();
                write!(f, "({})", parts.join(" * "))
            }
            CoreTypeDesc::Constr(path, args) => {
                if args.is_empty() {
                    write!(f, "{}", path)
                } else if args.len() == 1 {
                    write!(f, "{} {}", args[0], path)
                } else {
                    write!(
                        f,
                        "({})",
                        args.iter()
                            .map(|a| a.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                    write!(f, " {}", path)
                }
            }
            CoreTypeDesc::Object(fields, closed) => {
                let field_strs: Vec<String> = fields.iter().map(|f| f.to_string()).collect();
                if *closed == ClosedFlag::Closed {
                    write!(f, "< {} >", field_strs.join("; "))
                } else {
                    write!(f, "< {}; .. >", field_strs.join("; "))
                }
            }
            CoreTypeDesc::Class(path, args) => {
                if args.is_empty() {
                    write!(f, "#{}", path)
                } else {
                    write!(f, "#{}[{}]", path, join_display(args, ", "))
                }
            }
            CoreTypeDesc::Alias(ty, alias) => write!(f, "{} as '{}", ty, alias),
            CoreTypeDesc::Variant(fields, closed, labels) => {
                let field_strs: Vec<String> = fields.iter().map(|f| f.to_string()).collect();
                let mut result = String::from("[");
                if *closed == ClosedFlag::Open {
                    result.push('>');
                }
                result.push_str(&field_strs.join(" | "));
                if let Some(lbls) = labels {
                    if !lbls.is_empty() {
                        result.push_str(" > ");
                        result.push_str(&lbls.join(" "));
                    }
                }
                result.push(']');
                write!(f, "{}", result)
            }
            CoreTypeDesc::Poly(vars, ty) => {
                let var_strs: Vec<String> = vars.iter().map(|v| format!("'{}", v)).collect();
                write!(f, "type {}. {}", var_strs.join(" "), ty)
            }
            CoreTypeDesc::Package(pkg) => write!(f, "{}", pkg),
            CoreTypeDesc::Open(module, ty) => write!(f, "{}.{}", module, ty),
            CoreTypeDesc::Extension(_) => Ok(()), // Ignoring extensions
        }
    }
}

impl Display for PackageType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(module {})", self.path)?;
        if !self.constraints.is_empty() {
            write!(f, " with ")?;
            let constraints: Vec<String> = self
                .constraints
                .iter()
                .map(|(path, ty)| format!("type {} = {}", path, ty))
                .collect();
            write!(f, "{}", constraints.join(" and "))?;
        }
        Ok(())
    }
}

impl Display for ObjectField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for ObjectFieldDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjectFieldDesc::Tag(label, ty) => write!(f, "{} : {}", label, ty),
            ObjectFieldDesc::Inherit(ty) => write!(f, "{}", ty),
        }
    }
}

impl Display for RowField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for RowFieldDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RowFieldDesc::Tag(label, has_ampersand, types) => {
                write!(f, "`{}", label)?;
                if !types.is_empty() {
                    write!(f, " of ")?;
                    if *has_ampersand {
                        write!(f, "& ")?;
                    }
                    write!(f, "{}", join_display(types, " & "))?;
                }
                Ok(())
            }
            RowFieldDesc::Inherit(ty) => write!(f, "{}", ty),
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for PatternDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatternDesc::Any => write!(f, "_"),
            PatternDesc::Var(v) => write!(f, "{}", v),
            PatternDesc::Alias(p, alias) => write!(f, "{} as {}", p, alias),
            PatternDesc::Constant(c) => write!(f, "{}", c),
            PatternDesc::Interval(c1, c2) => write!(f, "{} .. {}", c1, c2),
            PatternDesc::Tuple(items, _) => {
                let parts: Vec<String> = items
                    .iter()
                    .map(|(label, pat)| {
                        if let Some(l) = label {
                            format!("~{}: {}", l, pat)
                        } else {
                            pat.to_string()
                        }
                    })
                    .collect();
                write!(f, "({})", parts.join(", "))
            }
            PatternDesc::Construct { c, args } => {
                write!(f, "{}", c)?;
                if let Some((_, pat)) = args {
                    write!(f, " {}", pat)?;
                }
                Ok(())
            }
            PatternDesc::Variant(label, pat) => {
                write!(f, "`{}", label)?;
                if let Some(p) = pat {
                    write!(f, " {}", p)?;
                }
                Ok(())
            }
            PatternDesc::Record(fields, closed) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(path, pat)| format!("{} = {}", path, pat))
                    .collect();
                if *closed == ClosedFlag::Open {
                    write!(f, "{{ {}; _ }}", field_strs.join("; "))
                } else {
                    write!(f, "{{ {} }}", field_strs.join("; "))
                }
            }
            PatternDesc::Array(pats) => write!(f, "[| {} |]", join_display(pats, "; ")),
            PatternDesc::List(pats) => write!(f, "[{}]", join_display(pats, "; ")),
            PatternDesc::Cons(head, tail) => write!(f, "{} :: {}", head, tail),
            PatternDesc::Or(p1, p2) => write!(f, "{} | {}", p1, p2),
            PatternDesc::Constraint(pat, ty) => write!(f, "({} : {})", pat, ty),
            PatternDesc::Type(path) => write!(f, "#{}", path),
            PatternDesc::Lazy(pat) => write!(f, "lazy {}", pat),
            PatternDesc::Unpack(name, pkg) => {
                if let Some(n) = name {
                    write!(f, "(module {})", n)?;
                } else {
                    write!(f, "(module _)")?;
                }
                if let Some(p) = pkg {
                    write!(f, " : {}", p)?;
                }
                Ok(())
            }
            PatternDesc::Exception(pat) => write!(f, "exception {}", pat),
            PatternDesc::Effect(eff, cont) => write!(f, "effect {} {}", eff, cont),
            PatternDesc::Extension(_) => Ok(()), // Ignoring extensions
            PatternDesc::Open(module, pat) => write!(f, "{}.{}", module, pat),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for ExpressionDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpressionDesc::Ident(id) => write!(f, "{}", id),
            ExpressionDesc::Constant(c) => write!(f, "{}", c),
            ExpressionDesc::Let {
                recursive,
                bindings,
                in_expr,
            } => {
                write!(f, "let")?;
                if *recursive == RecFlag::Recursive {
                    write!(f, " rec")?;
                }
                write!(f, " {} in {}", join_display(bindings, " and "), in_expr)
            }
            ExpressionDesc::Function {
                params,
                constraint,
                body,
            } => {
                write!(f, "fun")?;
                for param in params {
                    write!(f, " {}", param)?;
                }
                if let Some(c) = constraint {
                    write!(f, " : {}", c)?;
                }
                write!(f, " -> {}", body)
            }
            ExpressionDesc::Apply(func, args) => {
                write!(f, "{}", func)?;
                for (label, arg) in args {
                    match label {
                        ArgLabel::NoLabel => write!(f, " {}", arg)?,
                        ArgLabel::Labelled(l) => write!(f, " ~{}: {}", l, arg)?,
                        ArgLabel::Optional(l) => write!(f, " ?{}: {}", l, arg)?,
                    }
                }
                Ok(())
            }
            ExpressionDesc::Match(expr, cases) => {
                write!(f, "match {} with", expr)?;
                for (i, case) in cases.iter().enumerate() {
                    if i == 0 {
                        write!(f, " {}", case)?;
                    } else {
                        write!(f, " | {}", case)?;
                    }
                }
                Ok(())
            }
            ExpressionDesc::Try(expr, cases) => {
                write!(f, "try {} with", expr)?;
                for (i, case) in cases.iter().enumerate() {
                    if i == 0 {
                        write!(f, " {}", case)?;
                    } else {
                        write!(f, " | {}", case)?;
                    }
                }
                Ok(())
            }
            ExpressionDesc::Tuple(items) => {
                let parts: Vec<String> = items
                    .iter()
                    .map(|(label, expr)| {
                        if let Some(l) = label {
                            format!("~{}: {}", l, expr)
                        } else {
                            expr.to_string()
                        }
                    })
                    .collect();
                write!(f, "({})", parts.join(", "))
            }
            ExpressionDesc::Construct(path, expr) => {
                write!(f, "{}", path)?;
                if let Some(e) = expr {
                    write!(f, " {}", e)?;
                }
                Ok(())
            }
            ExpressionDesc::Variant(label, expr) => {
                write!(f, "`{}", label)?;
                if let Some(e) = expr {
                    write!(f, " {}", e)?;
                }
                Ok(())
            }
            ExpressionDesc::Record { fields, with_expr } => {
                if let Some(e) = with_expr {
                    write!(f, "{{ {} with ", e)?;
                } else {
                    write!(f, "{{ ")?;
                }
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(path, expr)| format!("{} = {}", path, expr))
                    .collect();
                write!(f, "{} }}", field_strs.join("; "))
            }
            ExpressionDesc::Field(expr, field) => write!(f, "{}.{}", expr, field),
            ExpressionDesc::SetField(record, field, value) => {
                write!(f, "{}.{} <- {}", record, field, value)
            }
            ExpressionDesc::Array(exprs) => write!(f, "[| {} |]", join_display(exprs, "; ")),
            ExpressionDesc::IfThenElse {
                cond,
                then_expr,
                else_expr,
            } => {
                write!(f, "if {} then {}", cond, then_expr)?;
                if let Some(e) = else_expr {
                    write!(f, " else {}", e)?;
                }
                Ok(())
            }
            ExpressionDesc::Seq(e1, e2) => write!(f, "{}; {}", e1, e2),
            ExpressionDesc::While { cond, body } => {
                write!(f, "while {} do {} done", cond, body)
            }
            ExpressionDesc::For {
                pat,
                from,
                to,
                dir,
                do_expr,
            } => {
                let dir_str = match dir {
                    DirectionFlag::Upto => "to",
                    DirectionFlag::Downto => "downto",
                };
                write!(
                    f,
                    "for {} = {} {} {} do {} done",
                    pat, from, dir_str, to, do_expr
                )
            }
            ExpressionDesc::Constraint(expr, ty) => write!(f, "({} : {})", expr, ty),
            ExpressionDesc::Coerce { e, from, to } => {
                if let Some(fr) = from {
                    write!(f, "({} :> {} : {})", e, fr, to)
                } else {
                    write!(f, "({} :> {})", e, to)
                }
            }
            ExpressionDesc::Send(expr, method) => write!(f, "{}#{}", expr, method),
            ExpressionDesc::New(path) => write!(f, "new {}", path),
            ExpressionDesc::Override(fields) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(label, expr)| format!("{} = {}", label, expr))
                    .collect();
                write!(f, "{{< {} >}}", field_strs.join("; "))
            }
            ExpressionDesc::StructItem(item, expr) => write!(f, "{}; {}", item, expr),
            ExpressionDesc::Assert(expr) => write!(f, "assert {}", expr),
            ExpressionDesc::Lazy(expr) => write!(f, "lazy {}", expr),
            ExpressionDesc::Poly(expr, ty) => {
                if let Some(t) = ty {
                    write!(f, "(fun (type a) -> ({} : {}))", expr, t)
                } else {
                    write!(f, "{}", expr)
                }
            }
            ExpressionDesc::Object(cls) => write!(f, "object {} end", cls),
            ExpressionDesc::NewType(name, expr) => write!(f, "fun (type {}) -> {}", name, expr),
            ExpressionDesc::Pack(module, pkg) => {
                write!(f, "(module {})", module)?;
                if let Some(p) = pkg {
                    write!(f, " : {}", p)?;
                }
                Ok(())
            }
            ExpressionDesc::Letop(letop) => write!(f, "{}", letop),
            ExpressionDesc::Extension(_) => Ok(()), // Ignoring extensions
            ExpressionDesc::Unreachable => write!(f, "."),
        }
    }
}

impl Display for Case {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.lhs)?;
        if let Some(guard) = &self.guard {
            write!(f, " when {}", guard)?;
        }
        write!(f, " -> {}", self.rhs)
    }
}

impl Display for Letop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "let{} {} = {}",
            self.let_.op, self.let_.pat, self.let_.exp
        )?;
        for and in &self.ands {
            write!(f, " and{} {} = {}", and.op, and.pat, and.exp)?;
        }
        write!(f, " in {}", self.body)
    }
}

impl Display for FunctionParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for FunctionParamDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionParamDesc::Val(label, default, pat) => match label {
                ArgLabel::NoLabel => write!(f, "{}", pat),
                ArgLabel::Labelled(l) => {
                    write!(f, "~{}", l)?;
                    if let Some(d) = default {
                        write!(f, ":({}={})", pat, d)
                    } else {
                        write!(f, ":{}", pat)
                    }
                }
                ArgLabel::Optional(l) => {
                    write!(f, "?{}", l)?;
                    if let Some(d) = default {
                        write!(f, ":({}={})", pat, d)
                    } else {
                        write!(f, ":{}", pat)
                    }
                }
            },
            FunctionParamDesc::NewType(name) => write!(f, "(type {})", name),
        }
    }
}

impl Display for FunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionBody::Body(expr) => write!(f, "{}", expr),
            FunctionBody::Cases(cases, _, _) => {
                write!(f, "function")?;
                for (i, case) in cases.iter().enumerate() {
                    if i == 0 {
                        write!(f, " {}", case)?;
                    } else {
                        write!(f, " | {}", case)?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl Display for TypeConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeConstraint::Constraint(ty) => write!(f, "{}", ty),
            TypeConstraint::Coerce(from, to) => {
                if let Some(fr) = from {
                    write!(f, "{} :> {}", fr, to)
                } else {
                    write!(f, "_ :> {}", to)
                }
            }
        }
    }
}

impl Display for ValueBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pat)?;
        if let Some(c) = &self.constraint {
            match c {
                ValueConstraint::Constraint { typ, .. } => write!(f, " : {}", typ)?,
                ValueConstraint::Coercion { ground, coercion } => {
                    if let Some(g) = ground {
                        write!(f, " : {} :> {}", g, coercion)?;
                    } else {
                        write!(f, " :> {}", coercion)?;
                    }
                }
            }
        }
        write!(f, " = {}", self.expr)
    }
}

// impl Display for Vec<StructureItem> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         for (i, item) in self.iter().enumerate() {
//             if i > 0 {
//                 write!(f, ";; ")?;
//             }
//             write!(f, "{}", item)?;
//         }
//         Ok(())
//     }
// }

impl Display for StructureItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for StructureItemDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StructureItemDesc::Eval(expr, _) => write!(f, "{}", expr),
            StructureItemDesc::Value(rec_flag, bindings) => {
                write!(f, "let")?;
                if *rec_flag == RecFlag::Recursive {
                    write!(f, " rec")?;
                }
                write!(f, " {}", join_display(bindings, " and "))
            }
            StructureItemDesc::Primitive(val_desc) => write!(f, "{}", val_desc),
            StructureItemDesc::Type(rec_flag, decls) => {
                write!(f, "type")?;
                if *rec_flag == RecFlag::Recursive {
                    write!(f, " rec")?;
                }
                write!(f, " {}", join_display(decls, " and "))
            }
            StructureItemDesc::TypeExt(ext) => write!(f, "{}", ext),
            StructureItemDesc::Exception(exc) => write!(f, "{}", exc),
            StructureItemDesc::Module(binding) => write!(f, "module {}", binding),
            StructureItemDesc::RecModule(bindings) => {
                write!(f, "module rec {}", join_display(bindings, " and "))
            }
            StructureItemDesc::ModType(decl) => write!(f, "module type {}", decl),
            StructureItemDesc::Open(open) => write!(f, "{}", open),
            StructureItemDesc::Class(classes) => {
                write!(f, "class {}", join_display(classes, " and "))
            }
            StructureItemDesc::ClassType(class_types) => {
                write!(f, "class type {}", join_display(class_types, " and "))
            }
            StructureItemDesc::Include(incl) => write!(f, "{}", incl),
            StructureItemDesc::Attribute(_) => Ok(()), // Ignoring attributes
            StructureItemDesc::Extension(_, _) => Ok(()), // Ignoring extensions
        }
    }
}

impl Display for ValueDescription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "val {} : {}", self.name, self.ty)?;
        if !self.prim.is_empty() {
            write!(f, " = \"{}\"", self.prim.join(" "))?;
        }
        Ok(())
    }
}

impl Display for TypeDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Type parameters
        if !self.params.is_empty() {
            if self.params.len() == 1 {
                write!(f, "{} ", self.params[0].0)?;
            } else {
                write!(f, "(")?;
                let param_strs: Vec<String> =
                    self.params.iter().map(|(ty, _)| ty.to_string()).collect();
                write!(f, "{}", param_strs.join(", "))?;
                write!(f, ") ")?;
            }
        }

        write!(f, "{}", self.name)?;

        // Manifest type
        if let Some(manifest) = &self.manifest {
            write!(f, " = ")?;
            if self.private_flag == PrivateFlag::Private {
                write!(f, "private ")?;
            }
            write!(f, "{}", manifest)?;
        }

        // Type kind
        match &self.kind {
            TypeKind::Abstract => {}
            TypeKind::Variant(constructors) => {
                if self.manifest.is_some() {
                    write!(f, " = ")?;
                }
                write!(f, " ")?;
                for (i, ctor) in constructors.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    } else if self.manifest.is_none() {
                        write!(f, "= ")?;
                    }
                    write!(f, "{}", ctor)?;
                }
            }
            TypeKind::Record(fields) => {
                if self.manifest.is_none() {
                    write!(f, " = ")?;
                }
                write!(f, " {{ {} }}", join_display(fields, "; "))?;
            }
            TypeKind::Open => write!(f, " = ..")?,
            TypeKind::External(s) => write!(f, " = \"{}\"", s)?,
        }

        // Constraints
        for (t1, t2, _) in &self.constraints {
            write!(f, " constraint {} = {}", t1, t2)?;
        }

        Ok(())
    }
}

impl Display for ConstructorDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;

        // Type variables
        if !self.vars.is_empty() {
            write!(f, " : ")?;
            for var in &self.vars {
                write!(f, "'{} ", var)?;
            }
            write!(f, ". ")?;
        }

        // Arguments
        match &self.args {
            ConstructorArguments::Tuple(types) if !types.is_empty() => {
                write!(f, " of {}", join_display(types, " * "))?;
            }
            ConstructorArguments::Record(fields) => {
                write!(f, " of {{ {} }}", join_display(fields, "; "))?;
            }
            _ => {}
        }

        // Result type
        if let Some(res) = &self.res {
            write!(f, " : {}", res)?;
        }

        Ok(())
    }
}

impl Display for LabelDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.mutable == MutableFlag::Mutable {
            write!(f, "mutable ")?;
        }
        write!(f, "{} : {}", self.name, self.ty)
    }
}

impl Display for TypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type")?;

        // Type parameters
        if !self.params.is_empty() {
            if self.params.len() == 1 {
                write!(f, " {}", self.params[0].0)?;
            } else {
                write!(f, " (")?;
                let param_strs: Vec<String> =
                    self.params.iter().map(|(ty, _)| ty.to_string()).collect();
                write!(f, "{}", param_strs.join(", "))?;
                write!(f, ")")?;
            }
        }

        write!(f, " {} +=", self.path)?;

        if self.private == PrivateFlag::Private {
            write!(f, " private")?;
        }

        for (i, ctor) in self.constructors.iter().enumerate() {
            write!(f, " ")?;
            if i > 0 {
                write!(f, "| ")?;
            }
            write!(f, "{}", ctor)?;
        }

        Ok(())
    }
}

impl Display for ExtensionConstructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        write!(f, "{}", self.kind)
    }
}

impl Display for ExtensionConstructorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionConstructorKind::Decl(vars, args, res) => {
                if !vars.is_empty() {
                    write!(f, " : ")?;
                    for var in vars {
                        write!(f, "'{} ", var)?;
                    }
                    write!(f, ". ")?;
                }

                match args {
                    ConstructorArguments::Tuple(types) if !types.is_empty() => {
                        write!(f, " of {}", join_display(types, " * "))?;
                    }
                    ConstructorArguments::Record(fields) => {
                        write!(f, " of {{ {} }}", join_display(fields, "; "))?;
                    }
                    _ => {}
                }

                if let Some(r) = res {
                    write!(f, " : {}", r)?;
                }
                Ok(())
            }
            ExtensionConstructorKind::Rebind(path) => write!(f, " = {}", path),
        }
    }
}

impl Display for TypeException {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "exception {}", self.constructor)
    }
}

impl Display for ModuleExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for ModuleExprDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModuleExprDesc::Ident(path) => write!(f, "{}", path),
            ModuleExprDesc::Structure(items) => {
                write!(f, "struct {} end", join_display(items, " "))
            }
            ModuleExprDesc::Functor(param, body) => {
                write!(f, "functor {} -> {}", param, body)
            }
            ModuleExprDesc::Apply(func, arg) => write!(f, "{}({})", func, arg),
            ModuleExprDesc::ApplyUnit(func) => write!(f, "{}()", func),
            ModuleExprDesc::Constraint(module, mtype) => write!(f, "({} : {})", module, mtype),
            ModuleExprDesc::Unpack(expr) => write!(f, "(val {})", expr),
            ModuleExprDesc::Extension(_) => Ok(()), // Ignoring extensions
        }
    }
}

impl Display for FunctorParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctorParameter::Unit => write!(f, "()"),
            FunctorParameter::Named(name, mtype) => {
                if let Some(n) = &name.txt {
                    write!(f, "({} : {})", n, mtype)
                } else {
                    write!(f, "(_ : {})", mtype)
                }
            }
        }
    }
}

impl Display for ModuleBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name.txt {
            write!(f, "{} = {}", name, self.expr)
        } else {
            write!(f, "_ = {}", self.expr)
        }
    }
}

impl Display for ModuleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for ModuleTypeDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModuleTypeDesc::Ident(path) => write!(f, "{}", path),
            ModuleTypeDesc::Signature(sig) => {
                write!(f, "sig {} end", join_display(sig, " "))
            }
            ModuleTypeDesc::Functor(param, result) => {
                write!(f, "functor {} -> {}", param, result)
            }
            ModuleTypeDesc::With(mtype, constraints) => {
                write!(f, "{} with {}", mtype, join_display(constraints, " and "))
            }
            ModuleTypeDesc::TypeOf(module) => write!(f, "module type of {}", module),
            ModuleTypeDesc::Alias(path) => write!(f, "{}", path),
            ModuleTypeDesc::Extension(_) => Ok(()), // Ignoring extensions
        }
    }
}

impl Display for WithConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WithConstraint::Type(path, decl) => write!(f, "type {} = {}", path, decl),
            WithConstraint::Module(path1, path2) => write!(f, "module {} = {}", path1, path2),
            WithConstraint::ModType(path, mtype) => write!(f, "module type {} = {}", path, mtype),
            WithConstraint::ModTypeSubst(path, mtype) => {
                write!(f, "module type {} := {}", path, mtype)
            }
            WithConstraint::TypeSubst(path, decl) => write!(f, "type {} := {}", path, decl),
            WithConstraint::ModSubst(path1, path2) => write!(f, "module {} := {}", path1, path2),
        }
    }
}

impl Display for SignatureItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for SignatureItemDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SignatureItemDesc::Value(val_desc) => write!(f, "{}", val_desc),
            SignatureItemDesc::Type(rec_flag, decls) => {
                write!(f, "type")?;
                if *rec_flag == RecFlag::Recursive {
                    write!(f, " rec")?;
                }
                write!(f, " {}", join_display(decls, " and "))
            }
            SignatureItemDesc::TypeSubst(decls) => {
                write!(f, "type {}", join_display(decls, " and "))
            }
            SignatureItemDesc::TypeExt(ext) => write!(f, "{}", ext),
            SignatureItemDesc::Exception(exc) => write!(f, "{}", exc),
            SignatureItemDesc::Module(decl) => write!(f, "module {}", decl),
            SignatureItemDesc::ModSubst(subst) => write!(f, "module {}", subst),
            SignatureItemDesc::RecModule(decls) => {
                write!(f, "module rec {}", join_display(decls, " and "))
            }
            SignatureItemDesc::ModType(decl) => write!(f, "module type {}", decl),
            SignatureItemDesc::ModTypeSubst(decl) => write!(f, "module type {}", decl),
            SignatureItemDesc::Open(open) => write!(f, "{}", open),
            SignatureItemDesc::Include(incl) => write!(f, "{}", incl),
            SignatureItemDesc::Class(classes) => {
                write!(f, "class {}", join_display(classes, " and "))
            }
            SignatureItemDesc::ClassType(class_types) => {
                write!(f, "class type {}", join_display(class_types, " and "))
            }
            SignatureItemDesc::Attribute(_) => Ok(()), // Ignoring attributes
            SignatureItemDesc::Extension(_, _) => Ok(()), // Ignoring extensions
        }
    }
}

impl Display for ModuleDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name.txt {
            write!(f, "{} : {}", name, self.ty)
        } else {
            write!(f, "_ : {}", self.ty)
        }
    }
}

impl Display for ModuleSubstitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.manifest)
    }
}

impl Display for ModuleTypeDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(ty) = &self.ty {
            write!(f, " = {}", ty)?;
        }
        Ok(())
    }
}

impl<T: Display> Display for OpenInfos<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "open")?;
        if self.overr_flag == OverrideFlag::Override {
            write!(f, "!")?;
        }
        write!(f, " {}", self.expr)
    }
}

impl<T: Display> Display for IncludeInfos<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "include {}", self.modd)
    }
}

impl Display for ClassStructure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.this)?;
        for field in &self.fields {
            write!(f, "{} ", field)?;
        }
        Ok(())
    }
}

impl Display for ClassField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for ClassFieldDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassFieldDesc::Inherit(override_flag, class_expr, alias) => {
                write!(f, "inherit")?;
                if *override_flag == OverrideFlag::Override {
                    write!(f, "!")?;
                }
                write!(f, " {}", class_expr)?;
                if let Some(a) = alias {
                    write!(f, " as {}", a)?;
                }
                Ok(())
            }
            ClassFieldDesc::Val(label, mutable, kind) => {
                write!(f, "val")?;
                if *mutable == MutableFlag::Mutable {
                    write!(f, " mutable")?;
                }
                write!(f, " {} ", label)?;
                match kind {
                    ClassFieldKind::Virtual(ty) => write!(f, ": {}", ty),
                    ClassFieldKind::Concrete(override_flag, expr) => {
                        if *override_flag == OverrideFlag::Override {
                            write!(f, "!")?;
                        }
                        write!(f, "= {}", expr)
                    }
                }
            }
            ClassFieldDesc::Method(label, private, kind) => {
                write!(f, "method")?;
                if *private == PrivateFlag::Private {
                    write!(f, " private")?;
                }
                write!(f, " {} ", label)?;
                match kind {
                    ClassFieldKind::Virtual(ty) => write!(f, ": {}", ty),
                    ClassFieldKind::Concrete(override_flag, expr) => {
                        if *override_flag == OverrideFlag::Override {
                            write!(f, "!")?;
                        }
                        write!(f, "= {}", expr)
                    }
                }
            }
            ClassFieldDesc::Constraint(t1, t2) => write!(f, "constraint {} = {}", t1, t2),
            ClassFieldDesc::Initializer(expr) => write!(f, "initializer {}", expr),
            ClassFieldDesc::Attribute(_) => Ok(()), // Ignoring attributes
            ClassFieldDesc::Extension(_) => Ok(()), // Ignoring extensions
        }
    }
}

impl Display for ClassExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for ClassExprDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassExprDesc::Constr(path, args) => {
                if args.is_empty() {
                    write!(f, "{}", path)
                } else {
                    write!(f, "{}[{}]", path, join_display(args, ", "))
                }
            }
            ClassExprDesc::Structure(structure) => {
                write!(f, "object {} end", structure)
            }
            ClassExprDesc::Fun(label, default, pattern, body) => {
                write!(f, "fun ")?;
                match label {
                    ArgLabel::NoLabel => write!(f, "{}", pattern)?,
                    ArgLabel::Labelled(l) => {
                        write!(f, "~{}", l)?;
                        if let Some(d) = default {
                            write!(f, ":({}={})", pattern, d)?;
                        } else {
                            write!(f, ":{}", pattern)?;
                        }
                    }
                    ArgLabel::Optional(l) => {
                        write!(f, "?{}", l)?;
                        if let Some(d) = default {
                            write!(f, ":({}={})", pattern, d)?;
                        } else {
                            write!(f, ":{}", pattern)?;
                        }
                    }
                }
                write!(f, " -> {}", body)
            }
            ClassExprDesc::Apply(func, args) => {
                write!(f, "{}", func)?;
                for (label, arg) in args {
                    match label {
                        ArgLabel::NoLabel => write!(f, " {}", arg)?,
                        ArgLabel::Labelled(l) => write!(f, " ~{}:{}", l, arg)?,
                        ArgLabel::Optional(l) => write!(f, " ?{}:{}", l, arg)?,
                    }
                }
                Ok(())
            }
            ClassExprDesc::Let(rec_flag, bindings, expr) => {
                write!(f, "let")?;
                if *rec_flag == RecFlag::Recursive {
                    write!(f, " rec")?;
                }
                write!(f, " {} in {}", join_display(bindings, " and "), expr)
            }
            ClassExprDesc::Constraint(expr, ty) => write!(f, "({} : {})", expr, ty),
            ClassExprDesc::Extension(_) => Ok(()), // Ignoring extensions
            ClassExprDesc::Open(open, expr) => write!(f, "{} {}", open, expr),
        }
    }
}

impl<T: Display> Display for ClassInfos<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.virt == VirtualFlag::Virtual {
            write!(f, "virtual ")?;
        }

        if !self.params.is_empty() {
            write!(f, "[")?;
            let param_strs: Vec<String> =
                self.params.iter().map(|(ty, _)| ty.to_string()).collect();
            write!(f, "{}", param_strs.join(", "))?;
            write!(f, "] ")?;
        }

        write!(f, "{} = {}", self.name, self.expr)
    }
}

impl Display for ClassType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for ClassTypeDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassTypeDesc::Constr(path, args) => {
                if args.is_empty() {
                    write!(f, "{}", path)
                } else {
                    write!(f, "{}[{}]", path, join_display(args, ", "))
                }
            }
            ClassTypeDesc::Signature(sig) => write!(f, "object {} end", sig),
            ClassTypeDesc::Arrow(label, ty, cty) => match label {
                ArgLabel::NoLabel => write!(f, "{} -> {}", ty, cty),
                ArgLabel::Labelled(l) => write!(f, "~{}:{} -> {}", l, ty, cty),
                ArgLabel::Optional(l) => write!(f, "?{}:{} -> {}", l, ty, cty),
            },
            ClassTypeDesc::Extension(_) => Ok(()), // Ignoring extensions
            ClassTypeDesc::Open(open, cty) => write!(f, "{} {}", open, cty),
        }
    }
}

impl Display for ClassSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.this)?;
        for field in &self.fields {
            write!(f, "{} ", field)?;
        }
        Ok(())
    }
}

impl Display for ClassTypeField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for ClassTypeFieldDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassTypeFieldDesc::Inherit(cty) => write!(f, "inherit {}", cty),
            ClassTypeFieldDesc::Val(label, mutable, virtual_flag, ty) => {
                write!(f, "val")?;
                if *mutable == MutableFlag::Mutable {
                    write!(f, " mutable")?;
                }
                if *virtual_flag == VirtualFlag::Virtual {
                    write!(f, " virtual")?;
                }
                write!(f, " {} : {}", label, ty)
            }
            ClassTypeFieldDesc::Method(label, private, virtual_flag, ty) => {
                write!(f, "method")?;
                if *private == PrivateFlag::Private {
                    write!(f, " private")?;
                }
                if *virtual_flag == VirtualFlag::Virtual {
                    write!(f, " virtual")?;
                }
                write!(f, " {} : {}", label, ty)
            }
            ClassTypeFieldDesc::Constraint(t1, t2) => write!(f, "constraint {} = {}", t1, t2),
            ClassTypeFieldDesc::Attribute(_) => Ok(()), // Ignoring attributes
            ClassTypeFieldDesc::Extension(_) => Ok(()), // Ignoring extensions
        }
    }
}

impl Display for TopLevelPhrase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TopLevelPhrase::Def(structure) => write!(f, "{}", join_display(structure, ";; ")),
            TopLevelPhrase::Dir(directive) => write!(f, "{}", directive),
        }
    }
}

impl Display for TopLevelDirective {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.name)?;
        if let Some(arg) = &self.arg {
            write!(f, " {}", arg)?;
        }
        Ok(())
    }
}

impl Display for DirectiveArgument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl Display for DirectiveArgumentDesc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DirectiveArgumentDesc::String(s) => write!(f, "\"{}\"", s),
            DirectiveArgumentDesc::Int(s, suffix) => {
                write!(f, "{}", s)?;
                if let Some(c) = suffix {
                    write!(f, "{}", c)?;
                }
                Ok(())
            }
            DirectiveArgumentDesc::Ident(id) => write!(f, "{}", id),
            DirectiveArgumentDesc::Bool(b) => write!(f, "{}", b),
        }
    }
}

// Placeholder implementations for types not in the provided code
// You'll need to implement these based on your actual type definitions

impl Display for ArgLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArgLabel::NoLabel => Ok(()),
            ArgLabel::Labelled(s) => write!(f, "~{}", s),
            ArgLabel::Optional(s) => write!(f, "?{}", s),
        }
    }
}

impl Display for RecFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecFlag::Nonrecursive => Ok(()),
            RecFlag::Recursive => write!(f, "rec"),
        }
    }
}

impl Display for DirectionFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DirectionFlag::Upto => write!(f, "to"),
            DirectionFlag::Downto => write!(f, "downto"),
        }
    }
}

impl Display for MutableFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MutableFlag::Immutable => Ok(()),
            MutableFlag::Mutable => write!(f, "mutable"),
        }
    }
}

impl Display for PrivateFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrivateFlag::Public => Ok(()),
            PrivateFlag::Private => write!(f, "private"),
        }
    }
}

impl Display for VirtualFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VirtualFlag::Concrete => Ok(()),
            VirtualFlag::Virtual => write!(f, "virtual"),
        }
    }
}

impl Display for OverrideFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OverrideFlag::Fresh => Ok(()),
            OverrideFlag::Override => write!(f, "!"),
        }
    }
}

impl Display for ClosedFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClosedFlag::Closed => Ok(()),
            ClosedFlag::Open => write!(f, ".."),
        }
    }
}

impl Display for Variance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variance::Covariant => write!(f, "+"),
            Variance::Contravariant => write!(f, "-"),
            Variance::NoVariance => Ok(()),
            Variance::Bivariant => write!(f, "(bivariant)"),
        }
    }
}

impl Display for Injectivity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Injectivity::Injective => write!(f, "!"),
            Injectivity::NoInjectivity => Ok(()),
        }
    }
}
