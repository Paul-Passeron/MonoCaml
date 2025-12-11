use crate::parsing::{
    asttypes::{
        ArgLabel, ClosedFlag, DirectionFlag, Injectivity, Label, Loc, MutableFlag, OverrideFlag,
        PrivateFlag, RecFlag, Variance, VirtualFlag,
    },
    location::Location,
    longident::LongIdent,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constant {
    pub const_desc: ConstantDesc,
    pub const_loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantDesc {
    Integer(String, Option<char>),
    Char(char),
    String(String, Location, Option<String>),
    Float(String, Option<char>),
}

pub type LocationStack = Vec<Location>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub name: Loc<String>,
    pub payload: Payload,
    pub loc: Location,
}

pub type Extension = (Loc<String>, Payload);
pub type Attributes = Vec<Attribute>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Payload {
    Str(Structure),
    Sig(Signature),
    Typ(CoreType),
    Pat(Pattern, Option<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CoreType {
    pub type_desc: CoreTypeDesc,
    pub loc: Location,
    pub loc_stack: LocationStack,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Open(Loc<LongIdent>, Box<CoreType>),
    Extension(Box<Extension>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageType {
    pub path: Loc<LongIdent>,
    pub constraints: Vec<(Loc<LongIdent>, CoreType)>,
    pub loc: Location,
    pub attrs: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RowField {
    pub desc: RowFieldDesc,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RowFieldDesc {
    Tag(Loc<Label>, bool, Vec<CoreType>),
    Inherit(CoreType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectField {
    pub desc: ObjectFieldDesc,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ObjectFieldDesc {
    Tag(Loc<Label>, CoreType),
    Inherit(CoreType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pattern {
    pub desc: PatternDesc,
    pub loc: Location,
    pub loc_stack: LocationStack,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternDesc {
    Any,
    Var(Loc<String>),
    Alias(Box<Pattern>, Loc<String>),
    Constant(Constant),
    Interval(Constant, Constant),
    Tuple(Vec<(Option<String>, Pattern)>, ClosedFlag),
    Construct {
        c: Loc<LongIdent>,
        args: Option<(Vec<Loc<String>>, Box<Pattern>)>,
    },
    Variant(Label, Option<Box<Pattern>>),
    Record(Vec<(Loc<LongIdent>, Pattern)>, ClosedFlag),
    Array(Vec<Pattern>),
    Or(Box<Pattern>, Box<Pattern>),
    Constraint(Box<Pattern>, CoreType),
    Type(Loc<LongIdent>),
    Lazy(Box<Pattern>),
    Unpack(Option<Loc<String>>, Option<PackageType>),
    Exception(Box<Pattern>),
    Effect(Box<Pattern>, Box<Pattern>),
    Extension(Box<Extension>),
    Open(Loc<LongIdent>, Box<Pattern>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expression {
    pub desc: ExpressionDesc,
    pub loc: Location,
    pub loc_stack: LocationStack,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Poly(Box<Expression>, Option<CoreType>),
    Object(ClassStructure),
    NewType(Loc<String>, Box<Expression>),
    Pack(ModuleExpr, Option<PackageType>),
    Letop(Letop),
    Extension(Box<Extension>),
    Unreachable,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Case {
    pub lhs: Pattern,
    pub guard: Option<Box<Expression>>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Letop {
    pub let_: BindingOp,
    pub ands: Vec<BindingOp>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BindingOp {
    pub op: Loc<String>,
    pub pat: Pattern,
    pub exp: Box<Expression>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionParam {
    pub desc: FunctionParamDesc,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctionParamDesc {
    Val(ArgLabel, Option<Expression>, Pattern),
    NewType(Loc<String>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctionBody {
    Body(Box<Expression>),
    Cases(Vec<Case>, Location, Attributes),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeConstraint {
    Constraint(CoreType),
    Coerce(Option<CoreType>, CoreType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueDescription {
    pub name: Loc<String>,
    pub ty: CoreType,
    pub prim: Vec<String>,
    pub attributes: Attributes,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDeclaration {
    pub name: Loc<String>,
    pub params: Vec<(CoreType, (Variance, Injectivity))>,
    pub constraints: Vec<(CoreType, CoreType, Location)>,
    pub kind: TypeKind,
    pub private_flag: PrivateFlag,
    pub manifest: Option<CoreType>,
    pub attributes: Attributes,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Abstract,
    Variant(Vec<ConstructorDeclaration>),
    Record(Vec<LabelDeclaration>),
    Open,
    External(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LabelDeclaration {
    pub name: Loc<String>,
    pub mutable: MutableFlag,
    pub ty: CoreType,
    pub attributes: Attributes,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstructorDeclaration {
    pub name: Loc<String>,
    pub vars: Vec<Loc<String>>,
    pub args: ConstructorArguments,
    pub res: Option<CoreType>,
    pub attributes: Attributes,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstructorArguments {
    Tuple(Vec<CoreType>),
    Record(Vec<LabelDeclaration>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeExtension {
    pub path: Loc<LongIdent>,
    pub params: Vec<(CoreType, Vec<(Variance, Injectivity)>)>,
    pub constructors: Vec<ExtensionConstructor>,
    pub private: PrivateFlag,
    pub attributes: Attributes,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtensionConstructor {
    pub name: Loc<String>,
    pub kind: ExtensionConstructorKind,
    pub attributes: Attributes,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExtensionConstructorKind {
    Decl(Vec<Loc<String>>, ConstructorArguments, Option<CoreType>),
    Rebind(Loc<LongIdent>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeException {
    pub constructor: ExtensionConstructor,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassType {
    pub desc: ClassTypeDesc,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClassTypeDesc {
    Constr(Loc<LongIdent>, Vec<CoreType>),
    Signature(ClassSignature),
    Arrow(ArgLabel, CoreType, Box<ClassType>),
    Extension(Extension),
    Open(OpenDescription, Box<ClassType>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassSignature {
    pub this: CoreType,
    pub fields: Vec<ClassTypeField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassTypeField {
    pub desc: ClassTypeFieldDesc,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClassTypeFieldDesc {
    Inherit(ClassType),
    Val(Loc<Label>, MutableFlag, VirtualFlag, CoreType),
    Method(Loc<Label>, PrivateFlag, VirtualFlag, CoreType),
    Constraint(CoreType, CoreType),
    Attribute(Attribute),
    Extension(Extension),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassInfos<T> {
    pub name: Loc<String>,
    pub virt: VirtualFlag,
    pub params: Vec<(CoreType, (Variance, Injectivity))>,
    pub expr: T,
    pub loc: Location,
    pub attributes: Attributes,
}

pub type ClassDescription = ClassInfos<ClassType>;

pub type ClassTypeDeclaration = ClassInfos<ClassType>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassExpr {
    pub desc: ClassExprDesc,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClassExprDesc {
    Constr(Loc<LongIdent>, Vec<CoreType>),
    Structure(ClassStructure),
    Fun(ArgLabel, Option<Expression>, Pattern, Box<ClassExpr>),
    Apply(Box<ClassExpr>, Vec<(ArgLabel, Expression)>),
    Let(RecFlag, Vec<ValueBinding>, Box<ClassExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassStructure {
    pub this: Pattern,
    pub fields: Vec<ClassField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassField {
    pub desc: ClassFieldDesc,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClassFieldDesc {
    Inherit(OverrideFlag, ClassExpr, Loc<String>),
    Val(Loc<Label>, MutableFlag, ClassFieldKind),
    Method(Loc<Label>, PrivateFlag, ClassFieldKind),
    Constraint(CoreType, CoreType),
    Initializer(Expression),
    Attribute(Attribute),
    Extension(Extension),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClassFieldKind {
    Virtual(CoreType),
    Concrete(OverrideFlag, Expression),
}

pub type ClassDeclaration = ClassInfos<ClassExpr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleType {
    pub desc: ModuleTypeDesc,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleTypeDesc {
    Ident(Loc<LongIdent>),
    Signature(Signature),
    Functor(FunctorParameter, Box<ModuleType>),
    With(Box<ModuleType>, Vec<WithConstraint>),
    TypeOf(ModuleExpr),
    Alias(Loc<LongIdent>),
    Extension(Extension),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctorParameter {
    Unit,
    Named(Loc<Option<String>>, Box<ModuleType>),
}

pub type Signature = Vec<SignatureItem>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SignatureItem {
    pub desc: SignatureItemDesc,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleDeclaration {
    pub name: Loc<Option<String>>,
    pub ty: ModuleType,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleSubstitution {
    pub name: Loc<String>,
    pub manifest: Loc<LongIdent>,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleTypeDeclaration {
    pub name: Loc<String>,
    pub ty: Option<ModuleType>,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OpenInfos<T> {
    pub expr: T,
    pub overr_flag: OverrideFlag,
    pub loc: Location,
    pub attributes: Attributes,
}

pub type OpenDescription = OpenInfos<Loc<LongIdent>>;
pub type OpenDeclaration = OpenInfos<ModuleExpr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IncludeInfos<T> {
    pub modd: T,
    pub attributes: Attributes,
    pub loc: Location,
}

pub type IncludeDescription = IncludeInfos<ModuleType>;
pub type IncludeDeclaration = IncludeInfos<ModuleExpr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WithConstraint {
    Type(Loc<LongIdent>, TypeDeclaration),
    Module(Loc<LongIdent>, Loc<LongIdent>),
    ModType(Loc<LongIdent>, ModuleType),
    ModTypeSubst(Loc<LongIdent>, ModuleType),
    TypeSubst(Loc<LongIdent>, TypeDeclaration),
    ModSubst(Loc<LongIdent>, Loc<LongIdent>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleExpr {
    pub desc: ModuleExprDesc,
    pub loc: Location,
    pub attributes: Attributes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructureItem {
    pub desc: StructureItemDesc,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueBinding {
    pub pat: Pattern,
    pub expr: Expression,
    pub constraint: Option<ValueConstraint>,
    pub attributes: Attributes,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleBinding {
    pub pat: Pattern,
    pub expr: ModuleExpr,
    pub attributes: Attributes,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TopLevelPhrase {
    Def(Structure),
    Dir(TopLevelDirective),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TopLevelDirective {
    pub name: Loc<String>,
    pub arg: Option<DirectiveArgument>,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirectiveArgument {
    pub desc: DirectiveArgumentDesc,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DirectiveArgumentDesc {
    String(String),
    Int(String, Option<char>),
    Ident(LongIdent),
    Bool(bool),
}
