use crate::{
    lexing::Position,
    parsing::{
        ast_helper::{Str, StrOpt},
        asttypes::RecFlag,
        docstring::{
            add_info_attrs, rhs_info, rhs_post_extra_text, rhs_post_text, rhs_pre_extra_text,
            rhs_text, Docs, Text,
        },
        location::Location,
        parsetree::{
            Attributes, ClassField, ClassTypeField, CoreType, CoreTypeDesc, DirectiveArgument,
            DirectiveArgumentDesc, Expression, Pattern, PatternDesc, SignatureItem, StructureItem,
            TopLevelDirective, TopLevelPhrase, ValueConstraint,
        },
    },
};

pub fn mktyp(loc: (Position, Position), attrs: Option<Attributes>, d: CoreTypeDesc) -> CoreType {
    CoreType::mk(Some(Location::make(loc.0, loc.1)), attrs, d)
}

pub fn mkpat(loc: (Position, Position), attrs: Option<Attributes>, d: PatternDesc) -> Pattern {
    Pattern::mk(Some(Location::make(loc.0, loc.1)), attrs, d)
}

pub struct LetBinding {
    _pattern: Pattern,
    _expr: Expression,
    _constraint: Option<ValueConstraint>,
    _is_pun: bool,
    _attributes: Attributes,
    _docs: Docs,
    _text: Text,
    _loc: Location,
}

pub struct LetBindings {
    _bindings: Vec<LetBinding>,
    _rec: RecFlag,
    _extension: StrOpt,
}

pub fn extra_text<T, F>(start: &Position, end: &Position, mut text: F, items: Vec<T>) -> Vec<T>
where
    F: FnMut(Text) -> Vec<T>,
{
    if items.is_empty() {
        let post = rhs_post_text(end);
        let post_extras = rhs_post_extra_text(end);
        let mut fst = text(post);
        fst.extend(text(post_extras).into_iter());
        fst
    } else {
        let pre_extras = rhs_pre_extra_text(start);
        let post_extras = rhs_post_extra_text(end);
        let mut res = text(pre_extras);
        res.extend(items.into_iter());
        res.extend(text(post_extras).into_iter());
        res
    }
}

pub fn extra_str(p1: &Position, p2: &Position, items: Vec<StructureItem>) -> Vec<StructureItem> {
    extra_text(p1, p2, StructureItem::text, items)
}

pub fn extra_sig(p1: &Position, p2: &Position, items: Vec<SignatureItem>) -> Vec<SignatureItem> {
    extra_text(p1, p2, SignatureItem::text, items)
}

pub fn extra_cstr(p1: &Position, p2: &Position, items: Vec<ClassField>) -> Vec<ClassField> {
    extra_text(p1, p2, ClassField::text, items)
}

pub fn extra_csig(p1: &Position, p2: &Position, items: Vec<ClassTypeField>) -> Vec<ClassTypeField> {
    extra_text(p1, p2, ClassTypeField::text, items)
}

pub fn extra_def(p1: &Position, p2: &Position, items: Vec<TopLevelPhrase>) -> Vec<TopLevelPhrase> {
    extra_text(
        p1,
        p2,
        |txt| {
            StructureItem::text(txt)
                .into_iter()
                .map(|def| TopLevelPhrase::Def(vec![def]))
                .collect()
        },
        items,
    )
}

pub fn text_str(pos: &Position) -> Vec<StructureItem> {
    StructureItem::text(rhs_text(pos))
}

pub fn text_sig(pos: &Position) -> Vec<SignatureItem> {
    SignatureItem::text(rhs_text(pos))
}

pub fn text_cstr(pos: &Position) -> Vec<ClassField> {
    ClassField::text(rhs_text(pos))
}

pub fn text_csig(pos: &Position) -> Vec<ClassTypeField> {
    ClassTypeField::text(rhs_text(pos))
}

pub fn text_def(pos: &Position) -> Vec<TopLevelPhrase> {
    text_str(pos)
        .into_iter()
        .map(|def| TopLevelPhrase::Def(vec![def]))
        .collect()
}

pub fn extra_rhs_core_type(ct: CoreType, pos: &Position) -> CoreType {
    let docs = rhs_info(pos);
    let mut ct = ct;
    add_info_attrs(docs, &mut ct.attributes);
    ct
}

pub fn pos(_x: usize) -> Position {
    Position::dummy()
}

pub fn mk_directive_arg(loc: (Position, Position), k: DirectiveArgumentDesc) -> DirectiveArgument {
    DirectiveArgument {
        desc: k,
        loc: Location::make(loc.0, loc.1),
    }
}

pub fn mk_directive(
    loc: (Position, Position),
    name: Str,
    arg: Option<DirectiveArgument>,
) -> TopLevelPhrase {
    TopLevelPhrase::Dir(TopLevelDirective {
        name,
        arg,
        loc: Location::make(loc.0, loc.1),
    })
}
