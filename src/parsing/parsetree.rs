use crate::parsing::{
    asttypes::{
        ArgLabel, ClosedFlag, DirectionFlag, Injectivity, Label, Loc, MutableFlag, OverrideFlag,
        PrivateFlag, RecFlag, Variance, VirtualFlag,
    },
    location::Location,
    longident::LongIdent,
};

pub struct Constant {
    const_desc: ConstantDesc,
    const_loc: Location,
}

pub enum ConstantDesc {
    Integer(String, Option<char>),
    Char(char),
    String(String, Location, Option<String>),
    Float(String, Option<char>),
}

pub type LocationStack = Vec<Location>;

pub struct Attribute {
    name: Loc<String>,
    payload: Payload,
    loc: Location,
}

pub type Extension = (Loc<String>, Payload);
pub type Attributes = Vec<Attribute>;

pub enum Payload {
    Str(Structure),
    Sig(Signature),
    Typ(CoreType),
    Pat(Pattern, Option<Expression>),
}

pub struct CoreType {
    type_desc: CoreTypeDesc,
    loc: Location,
    loc_stack: LocationStack,
    attributes: Attributes,
}

pub enum CoreTypeDesc {
    Any,
    Var(String),
    Arrow(ArgLabel, Box<CoreType>, Box<CoreType>),
    Tuple(Vec<(Option<String>, CoreType)>),
    Constr(Loc<LongIdent>, Vec<CoreType>),
    Object(Vec<ObjectField>, ClosedFlag),
    Class(Loc<LongIdent>, Vec<CoreType>),
    Alias(Box<CoreType>, Loc<String>),
    Variant(Vec<RowField>, ClosedFlag, Option<Vec<Label>>),
    Poly(Vec<Loc<String>>, Box<CoreType>),
    Package(PackageType),
    Open(Vec<(Loc<LongIdent>, CoreType)>),
    Extension(Box<Extension>),
}

pub struct PackageType {
    path: Loc<LongIdent>,
    constraints: Vec<(Loc<LongIdent>, CoreType)>,
    loc: Location,
    attrs: Attributes,
}

pub struct RowField {
    desc: RowFieldDesc,
    loc: Location,
    attributes: Attributes,
}

pub enum RowFieldDesc {
    Tag(Loc<Label>, bool, Vec<CoreType>),
    Inherit(CoreType),
}

pub struct ObjectField {
    desc: ObjectFieldDesc,
    loc: Location,
    attributes: Attributes,
}

pub enum ObjectFieldDesc {
    Tag(Loc<Label>, CoreType),
    Inherit(CoreType),
}

pub struct Pattern {
    desc: PatternDesc,
    loc: Location,
    loc_stack: LocationStack,
    attributes: Attributes,
}

pub enum PatternDesc {
    Any,
    Var(Loc<String>),
    Alias(Box<Pattern>, Loc<String>),
    Constant(Constant),
    Interval(Constant, Constant),
    Tuple(Vec<(Option<String>, Box<Pattern>)>, ClosedFlag),
    Construct {
        c: Loc<LongIdent>,
        args: Option<(Vec<Loc<String>>, Box<Pattern>)>,
    },
    Variant(Label, Option<Box<Pattern>>),
    Array(Vec<Pattern>),
    Constraint(Box<Pattern>, CoreType),
    Type(Loc<LongIdent>),
    Lazy(Box<Pattern>),
    Unpack(Option<Loc<String>>, Option<PackageType>),
    Exception(Box<Pattern>),
    Effect(Box<Pattern>, Box<Pattern>),
    Extension(Box<Extension>),
    Open(Loc<LongIdent>, Box<Pattern>),
}

pub struct Expression {
    desc: ExpressionDesc,
    loc: Location,
    loc_stack: LocationStack,
    attributes: Attributes,
}

pub enum ExpressionDesc {
    Ident(Loc<LongIdent>),
    Constant(Constant),
    Let {
        recursive: RecFlag,
        bindings: Vec<ValueBinding>,
        in_expr: Box<Expression>,
    },

    Function {
        params: Vec<FunctionParam>,
        constraint: Option<TypeConstraint>,
        body: FunctionBody,
    },

    Apply(Box<Expression>, Vec<(ArgLabel, Expression)>),
    Match(Box<Expression>, Vec<Case>),
    Try(Box<Expression>, Vec<Case>),
    Tuple(Vec<(Option<String>, Expression)>),
    Construct(Loc<LongIdent>, Option<Box<Expression>>),
    Variant(Label, Option<Box<Expression>>),
    Record {
        fields: Vec<(Loc<LongIdent>, Expression)>,
        with_expr: Option<Box<Expression>>,
    },
    Field(Box<Expression>, Loc<LongIdent>),
    SetField(Box<Expression>, Loc<LongIdent>, Box<Expression>),
    Array(Vec<Expression>),
    IfThenElse {
        cond: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Option<Box<Expression>>,
    },
    Seq(Box<Expression>, Box<Expression>),
    While {
        cond: Box<Expression>,
        body: Box<Expression>,
    },
    For {
        pat: Pattern,
        from: Box<Expression>,
        to: Box<Expression>,
        dir: DirectionFlag,
        do_expr: Box<Expression>,
    },
    Constraint(Box<Expression>, CoreType),
    Coerce {
        e: Box<Expression>,
        from: Option<CoreType>,
        to: CoreType,
    },
    Send(Box<Expression>, Loc<Label>),
    New(Loc<LongIdent>),
    Override(Vec<(Loc<Label>, Expression)>),
    StructItem(Box<StructureItem>, Box<Expression>),
    Assert(Box<Expression>),
    Lazy(Box<Expression>),
    Poly(Box<Expression>, Option<Box<CoreType>>),
    Object(ClassStructure),
    NewType(Loc<String>, Box<Expression>),
    Pack(ModuleExpr, Option<PackageType>),
    Letop(Letop),
    Extension(Box<Extension>),
    Unreachable,
}

pub struct Case {
    lhs: Pattern,
    guard: Option<Box<Expression>>,
    rhs: Box<Expression>,
}

pub struct Letop {
    let_: BindingOp,
    ands: Vec<BindingOp>,
    body: Box<Expression>,
}

pub struct BindingOp {
    op: Loc<String>,
    pat: Pattern,
    exp: Box<Expression>,
    loc: Location,
}

pub struct FunctionParam {
    desc: FunctionParamDesc,
    loc: Location,
}

pub enum FunctionParamDesc {
    Val(ArgLabel, Option<Expression>, Pattern),
    NewType(Loc<String>),
}

pub enum FunctionBody {
    Body(Box<Expression>),
    Cases(Vec<Case>, Location, Attributes),
}

pub enum TypeConstraint {
    Constraint(CoreType),
    Coerce(Option<CoreType>, CoreType),
}

pub struct ValueDescription {
    name: Loc<String>,
    ty: CoreType,
    prim: Vec<String>,
    attributes: Attributes,
    loc: Location,
}

pub struct TypeDeclaration {
    name: Loc<String>,
    params: Vec<(CoreType, (Variance, Injectivity))>,
    constraints: Vec<(CoreType, CoreType, Location)>,
    kind: TypeKind,
    private_flag: PrivateFlag,
    manifest: Option<CoreType>,
    attributes: Attributes,
    loc: Location,
}

pub enum TypeKind {
    Abstract,
    Variant(Vec<ConstructorDeclaration>),
    Record(Vec<LabelDeclaration>),
    Open,
    External(String),
}

pub struct LabelDeclaration {
    name: Loc<String>,
    mutable: MutableFlag,
    ty: CoreType,
    attributes: Attributes,
    loc: Location,
}

pub struct ConstructorDeclaration {
    name: Loc<String>,
    vars: Vec<Loc<String>>,
    args: ConstructorArguments,
    res: Option<CoreType>,
    attributes: Attributes,
    loc: Location,
}

pub enum ConstructorArguments {
    Tuple(Vec<CoreType>),
    Record(Vec<LabelDeclaration>),
}

pub struct TypeExtension {
    path: Loc<LongIdent>,
    params: Vec<(CoreType, Vec<(Variance, Injectivity)>)>,
    constructors: Vec<ExtensionConstructor>,
    private: PrivateFlag,
    attributes: Attributes,
    loc: Location,
}

pub struct ExtensionConstructor {
    name: Loc<String>,
    kind: ExtensionConstructorKind,
    attributes: Attributes,
    loc: Location,
}

pub enum ExtensionConstructorKind {
    Decl(Vec<Loc<String>>, ConstructorArguments, Option<CoreType>),
    Rebind(Loc<LongIdent>),
}

pub struct TypeException {
    constructor: ExtensionConstructor,
    loc: Location,
    attributes: Attributes,
}

pub struct ClassType {
    desc: ClassTypeDesc,
    loc: Location,
    attributes: Attributes,
}

pub enum ClassTypeDesc {
    Constr(Loc<LongIdent>, Vec<CoreType>),
    Signature(ClassSignature),
    Arrow(ArgLabel, CoreType, Box<ClassType>),
    Extension(Extension),
    Open(OpenDescription, Box<ClassType>),
}

pub struct ClassSignature {
    this: CoreType,
    fields: Vec<ClassTypeField>,
}

pub struct ClassTypeField {
    desc: ClassTypeFieldDesc,
    loc: Location,
    attributes: Attributes,
}

pub enum ClassTypeFieldDesc {
    Inherit(ClassType),
    Val(Loc<Label>, MutableFlag, VirtualFlag, CoreType),
    Method(Loc<Label>, PrivateFlag, VirtualFlag, CoreType),
    Constraint(CoreType, CoreType),
    Attribute(Attribute),
    Extension(Extension),
}

pub struct ClassInfos<T> {
    name: Loc<String>,
    virt: VirtualFlag,
    params: Vec<(CoreType, (Variance, Injectivity))>,
    expr: T,
    loc: Location,
    attributes: Attributes,
}

pub type ClassDescription = ClassInfos<ClassType>;

pub type ClassTypeDeclaration = ClassInfos<ClassType>;

pub struct ClassExpr {
    desc: ClassExprDesc,
    loc: Location,
    attributes: Attributes,
}

pub enum ClassExprDesc {
    Constr(Loc<LongIdent>, Vec<CoreType>),
    Structure(ClassStructure),
    Fun(ArgLabel, Option<Expression>, Pattern, Box<ClassExpr>),
    Apply(Box<ClassExpr>, Vec<(ArgLabel, Expression)>),
    Let(RecFlag, Vec<ValueBinding>, Box<ClassExpr>),
}

pub struct ClassStructure {
    this: Pattern,
    fields: Vec<ClassField>,
}

pub struct ClassField {
    desc: ClassFieldDesc,
    loc: Location,
    attributes: Attributes,
}

pub enum ClassFieldDesc {
    Inherit(OverrideFlag, ClassExpr, Loc<String>),
    Val(Loc<Label>, MutableFlag, ClassFieldKind),
    Method(Loc<Label>, PrivateFlag, ClassFieldKind),
    Constraint(CoreType, CoreType),
    Initializer(Expression),
    Attribute(Attribute),
    Extension(Extension),
}

pub enum ClassFieldKind {
    Virtual(CoreType),
    Concrete(OverrideFlag, Expression),
}

pub type ClassDeclaration = ClassInfos<ClassExpr>;

pub struct ModuleType {
    desc: ModuleTypeDesc,
    loc: Location,
    attributes: Attributes,
}

pub enum ModuleTypeDesc {
    Ident(Loc<LongIdent>),
    Signature(Signature),
    Functor(FunctorParameter, Box<ModuleType>),
    With(Box<ModuleType>, Vec<WithConstraint>),
    TypeOf(ModuleExpr),
    Alias(Loc<LongIdent>),
    Extension(Extension),
}

pub enum FunctorParameter {
    Unit,
    Named(Loc<Option<String>>, Box<ModuleType>),
}

pub type Signature = Vec<SignatureItem>;

pub struct SignatureItem {
    desc: SignatureItemDesc,
    loc: Location,
}

pub enum SignatureItemDesc {
    Value(ValueDescription),
    Type(RecFlag, Vec<TypeDeclaration>),
    TypeSubst(Vec<TypeDeclaration>),
    TypeExt(TypeExtension),
    Exception(TypeException),
    Module(ModuleDeclaration),
    ModSubst(ModuleSubstitution),
    RecModule(Vec<ModuleDeclaration>),
    ModType(ModuleTypeDeclaration),
    Open(OpenDescription),
    Include(IncludeDescription),
    Class(Vec<ClassDescription>),
    ClassType(Vec<ClassTypeDeclaration>),
    Attribute(Attribute),
    Extension(Extension),
}

pub struct ModuleDeclaration {
    name: Loc<Option<String>>,
    ty: ModuleType,
    loc: Location,
    attributes: Attributes,
}

pub struct ModuleSubstitution {
    name: Loc<String>,
    manifest: Loc<LongIdent>,
    loc: Location,
    attributes: Attributes,
}

pub struct ModuleTypeDeclaration {
    name: Loc<String>,
    ty: Option<ModuleType>,
    loc: Location,
    attributes: Attributes,
}

pub struct OpenInfos<T> {
    expr: T,
    overr_flag: OverrideFlag,
    loc: Location,
    attributes: Attributes,
}

pub type OpenDescription = OpenInfos<Loc<LongIdent>>;
pub type OpenDeclaration = OpenInfos<ModuleExpr>;

pub struct IncludeInfos<T> {
    modd: T,
    attributes: Attributes,
    loc: Location,
}

pub type IncludeDescription = IncludeInfos<ModuleType>;
pub type IncludeDeclaration = IncludeInfos<ModuleExpr>;

pub enum WithConstraint {
    Type(Loc<LongIdent>, TypeDeclaration),
    Module(Loc<LongIdent>, Loc<LongIdent>),
    ModType(Loc<LongIdent>, ModuleType),
    ModTypeSubst(Loc<LongIdent>, ModuleType),
    TypeSubst(Loc<LongIdent>, TypeDeclaration),
    ModSubst(Loc<LongIdent>, Loc<LongIdent>),
}

pub struct ModuleExpr {
    desc: ModuleExprDesc,
    loc: Location,
    attributes: Attributes,
}

pub enum ModuleExprDesc {
    Ident(Loc<LongIdent>),
    Structure(Structure),
    Functor(FunctorParameter, Box<ModuleExpr>),
    Apply(Box<ModuleExpr>, Box<ModuleExpr>),
    ApplyUnit(Box<ModuleExpr>),
    Constraint(Box<ModuleExpr>, Box<ModuleType>),
    Unpack(Box<Expression>),
    Extension(Box<Extension>),
}

pub type Structure = Vec<StructureItem>;

pub struct StructureItem {
    desc: StructureItemDesc,
    loc: Location,
}

pub enum StructureItemDesc {
    Eval(Box<Expression>, Attributes),
    Value(RecFlag, Vec<ValueBinding>),
    Primitive(ValueDescription),
    Type(RecFlag, Vec<TypeDeclaration>),
    TypeExt(TypeExtension),
    Module(ModuleBinding),
    RecModule(Vec<ModuleBinding>),
    ModType(Box<ModuleTypeDeclaration>),
    Open(OpenDeclaration),
    Class(Vec<ClassDeclaration>),
    ClassType(Vec<ClassTypeDeclaration>),
    Include(IncludeDeclaration),
    Attribute(Attribute),
    Extension(Box<Extension>, Attributes),
}

pub enum ValueConstraint {
    Constraint {
        locally_abstract_univars: Vec<Loc<String>>,
        typ: CoreType,
    },
    Coercion {
        ground: Option<CoreType>,
        coercion: CoreType,
    },
}

pub struct ValueBinding {
    pat: Pattern,
    expr: Expression,
    constraint: Option<ValueConstraint>,
    attributes: Attributes,
    loc: Location,
}

pub struct ModuleBinding {
    pat: Pattern,
    expr: ModuleExpr,
    attributes: Attributes,
    loc: Location,
}

pub enum TopLevelPhrase {
    Def(Structure),
    Dir(TopLevelDirective),
}

pub struct TopLevelDirective {
    name: Loc<String>,
    arg: Option<DirectiveArgument>,
    loc: Location,
}

pub struct DirectiveArgument {
    desc: DirectiveArgumentDesc,
    loc: Location,
}

pub enum DirectiveArgumentDesc {
    String(String),
    Int(String, Option<char>),
    Ident(LongIdent),
    Bool(bool),
}
