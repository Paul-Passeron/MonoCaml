use crate::parsing::{
    ast_helper::{get_default_loc, Str},
    asttypes::{Injectivity, MutableFlag, PrivateFlag, Variance},
    docstring::{add_info_attrs, get_empty_docs, get_empty_info, DocString, Docs, Info},
    location::Location,
    parsetree::{
        Attributes, ConstructorArguments, ConstructorDeclaration, CoreType, LabelDeclaration,
        TypeDeclaration, TypeKind,
    },
};

impl TypeDeclaration {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        docs: Option<Docs>,
        text: Option<Vec<DocString>>,
        params: Option<Vec<(CoreType, (Variance, Injectivity))>>,
        constraints: Option<Vec<(CoreType, CoreType, Location)>>,
        kind: Option<TypeKind>,
        priv_flag: Option<PrivateFlag>,
        manifest: Option<CoreType>,
        name: Str,
    ) -> Self {
        let mut attrs = attrs.unwrap_or_default();
        docs.unwrap_or_else(get_empty_docs).add_attrs(&mut attrs);
        DocString::add_attrs(text.unwrap_or_default(), &mut attrs);
        Self {
            name,
            params: params.unwrap_or_default(),
            constraints: constraints.unwrap_or_default(),
            kind: kind.unwrap_or(TypeKind::Abstract),
            private_flag: priv_flag.unwrap_or(PrivateFlag::Public),
            manifest,
            attributes: attrs,
            loc: loc.unwrap_or_else(get_default_loc),
        }
    }
}

impl ConstructorDeclaration {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        infos: Option<Info>,
        vars: Option<Vec<Str>>,
        args: Option<ConstructorArguments>,
        res: Option<CoreType>,
        name: Str,
    ) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let mut attributes = attrs.unwrap_or_default();
        add_info_attrs(infos.unwrap_or_else(get_empty_info), &mut attributes);
        let args = args.unwrap_or_else(|| ConstructorArguments::Tuple(vec![]));
        let vars = vars.unwrap_or_default();
        Self {
            name,
            vars,
            args,
            res,
            attributes,
            loc,
        }
    }
}

impl LabelDeclaration {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        infos: Option<Info>,
        mut_flag: Option<MutableFlag>,
        name: Str,
        ty: CoreType,
    ) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let mut attributes = attrs.unwrap_or_default();
        add_info_attrs(infos.unwrap_or_else(get_empty_info), &mut attributes);
        let mutable = mut_flag.unwrap_or(MutableFlag::Immutable);
        Self {
            name,
            mutable,
            ty,
            attributes,
            loc,
        }
    }
}
