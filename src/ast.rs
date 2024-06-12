/// The AST maps closely to assembly for simplicity.
use std::ptr::NonNull;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Preceding {
    Parent(NonNull<AstNode>),
    Previous(NonNull<AstNode>),
}

#[derive(Debug)]
pub struct AstNode {
    pub line: Line,
    pub preceding: Option<Preceding>,
    pub child: Option<NonNull<AstNode>>,
    pub next: Option<NonNull<AstNode>>,
}

// #[cfg(target_arch = "aarch64")]
pub use crate::aarch64::Instruction;

#[derive(Debug, Clone)]
pub enum Line {
    Assembly(Instruction),
    Source(Expression),
}

pub type Arg = Vec<Value>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Literal(Literal),
    Variable(Variable),
    Type(Type),
    Register(Register),
}
impl Value {
    pub fn literal(&self) -> Option<&Literal> {
        match self {
            Self::Literal(literal) => Some(literal),
            _ => None,
        }
    }
    pub fn variable_mut(&mut self) -> Option<&mut Variable> {
        match self {
            Self::Variable(variable) => Some(variable),
            _ => None,
        }
    }
    pub fn variable(&self) -> Option<&Variable> {
        match self {
            Self::Variable(variable) => Some(variable),
            _ => None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Variable(variable) => write!(f, "{variable}"),
            Self::Type(value_type) => write!(f, "{value_type}"),
            Self::Register(register) => write!(f, "{register}"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Register {
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X18,
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    X29,
    X30,
    X31,
}

impl TryFrom<&Identifier> for Register {
    type Error = ();
    fn try_from(Identifier(bytes): &Identifier) -> Result<Self, Self::Error> {
        Self::try_from(bytes.as_slice())
    }
}

impl TryFrom<&[u8]> for Register {
    type Error = ();
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        match bytes {
            b"x0" => Ok(Self::X0),
            b"x1" => Ok(Self::X1),
            b"x2" => Ok(Self::X2),
            b"x3" => Ok(Self::X3),
            b"x4" => Ok(Self::X4),
            b"x5" => Ok(Self::X5),
            b"x6" => Ok(Self::X6),
            b"x7" => Ok(Self::X7),
            b"x8" => Ok(Self::X8),
            b"x9" => Ok(Self::X9),
            b"x10" => Ok(Self::X10),
            b"x11" => Ok(Self::X11),
            b"x12" => Ok(Self::X12),
            b"x13" => Ok(Self::X13),
            b"x14" => Ok(Self::X14),
            b"x15" => Ok(Self::X15),
            b"x16" => Ok(Self::X16),
            b"x17" => Ok(Self::X17),
            b"x18" => Ok(Self::X18),
            b"x19" => Ok(Self::X19),
            b"x20" => Ok(Self::X20),
            b"x21" => Ok(Self::X21),
            b"x22" => Ok(Self::X22),
            b"x23" => Ok(Self::X23),
            b"x24" => Ok(Self::X24),
            b"x25" => Ok(Self::X25),
            b"x26" => Ok(Self::X26),
            b"x27" => Ok(Self::X27),
            b"x28" => Ok(Self::X28),
            b"x29" => Ok(Self::X29),
            b"x30" => Ok(Self::X30),
            b"x31" => Ok(Self::X31),
            _ => Err(()),
        }
    }
}

impl Register {
    pub fn w(&self) -> String {
        match self {
            Self::X0 => String::from("w0"),
            Self::X1 => String::from("w1"),
            Self::X2 => String::from("w2"),
            Self::X3 => String::from("w3"),
            Self::X4 => String::from("w4"),
            Self::X5 => String::from("w5"),
            Self::X6 => String::from("w6"),
            Self::X7 => String::from("w7"),
            Self::X8 => String::from("w8"),
            Self::X9 => String::from("w9"),
            Self::X10 => String::from("w10"),
            Self::X11 => String::from("w11"),
            Self::X12 => String::from("w12"),
            Self::X13 => String::from("w13"),
            Self::X14 => String::from("w14"),
            Self::X15 => String::from("w15"),
            Self::X16 => String::from("w16"),
            Self::X17 => String::from("w17"),
            Self::X18 => String::from("w18"),
            Self::X19 => String::from("w19"),
            Self::X20 => String::from("w20"),
            Self::X21 => String::from("w21"),
            Self::X22 => String::from("w22"),
            Self::X23 => String::from("w23"),
            Self::X24 => String::from("w24"),
            Self::X25 => String::from("w25"),
            Self::X26 => String::from("w26"),
            Self::X27 => String::from("w27"),
            Self::X28 => String::from("w28"),
            Self::X29 => String::from("w29"),
            Self::X30 => String::from("w30"),
            Self::X31 => String::from("w31"),
        }
    }
}

impl std::fmt::Display for Register {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::X0 => write!(f, "x0"),
            Self::X1 => write!(f, "x1"),
            Self::X2 => write!(f, "x2"),
            Self::X3 => write!(f, "x3"),
            Self::X4 => write!(f, "x4"),
            Self::X5 => write!(f, "x5"),
            Self::X6 => write!(f, "x6"),
            Self::X7 => write!(f, "x7"),
            Self::X8 => write!(f, "x8"),
            Self::X9 => write!(f, "x9"),
            Self::X10 => write!(f, "x10"),
            Self::X11 => write!(f, "x11"),
            Self::X12 => write!(f, "x12"),
            Self::X13 => write!(f, "x13"),
            Self::X14 => write!(f, "x14"),
            Self::X15 => write!(f, "x15"),
            Self::X16 => write!(f, "x16"),
            Self::X17 => write!(f, "x17"),
            Self::X18 => write!(f, "x18"),
            Self::X19 => write!(f, "x19"),
            Self::X20 => write!(f, "x20"),
            Self::X21 => write!(f, "x21"),
            Self::X22 => write!(f, "x22"),
            Self::X23 => write!(f, "x23"),
            Self::X24 => write!(f, "x24"),
            Self::X25 => write!(f, "x25"),
            Self::X26 => write!(f, "x26"),
            Self::X27 => write!(f, "x27"),
            Self::X28 => write!(f, "x28"),
            Self::X29 => write!(f, "x29"),
            Self::X30 => write!(f, "x30"),
            Self::X31 => write!(f, "x31"),
        }
    }
}

impl TryFrom<&Variable> for Register {
    type Error = ();
    fn try_from(
        Variable {
            addressing,
            identifier,
            index,
            cast: _,
        }: &Variable,
    ) -> Result<Self, Self::Error> {
        if *addressing == Addressing::Direct
            && *index == None
            && let Ok(register) = Register::try_from(identifier)
        {
            Ok(register)
        } else {
            Err(())
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Literal(Default::default())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Integer(Integer),
}

impl Literal {
    pub fn integer(&self) -> Option<&Integer> {
        match self {
            Self::Integer(integer) => Some(integer),
            _ => None,
        }
    }
    // pub fn string(&self) -> Option<&String> {
    //     match self {
    //         Self::String(string) => Some(string),
    //         _ => None,
    //     }
    // }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::String(string) => write!(f, "{string:?}"),
            Self::Integer(integer) => write!(f, "{integer}"),
        }
    }
}

pub type Integer = i128;

impl Default for Literal {
    fn default() -> Self {
        Self::Integer(Default::default())
    }
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct Variable {
    /// The `&`, `*` or `` in `&x`, `*x` or `x`.
    pub addressing: Addressing,
    /// The variable identifier e.g `x` in `&x`.
    pub identifier: Identifier,
    /// The index operation e.g. `[1]` in `x[1]`.
    pub index: Option<Box<Offset>>,
    /// The cast e.g. `:i32` in `x:i32`.
    pub cast: Option<Cast>,
}

impl From<Identifier> for Variable {
    fn from(identifier: Identifier) -> Self {
        Self {
            addressing: Addressing::Direct,
            identifier,
            index: None,
            cast: None,
        }
    }
}

impl From<&[char]> for Variable {
    fn from(chars: &[char]) -> Self {
        Self {
            addressing: Addressing::Direct,
            identifier: Identifier::from(chars),
            index: None,
            cast: None,
        }
    }
}

#[derive(Eq, PartialEq, Default, Clone, Hash, Debug)]
pub enum Addressing {
    /// &x
    Reference,
    /// x
    #[default]
    Direct,
    /// *x
    Dereference,
}

impl std::fmt::Display for Addressing {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Reference => write!(f, "&"),
            Self::Direct => Ok(()),
            Self::Dereference => write!(f, "*"),
        }
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.addressing,
            self.identifier,
            self.index.as_ref().map(|v| format!("[{v}]")).unwrap_or(String::new()),
            self.cast.as_ref().map(|t| format!(": {t}")).unwrap_or(String::new())
        )
    }
}

impl From<&str> for Variable {
    fn from(s: &str) -> Self {
        Self {
            addressing: Addressing::Direct,
            identifier: Identifier::from(s),
            index: None,
            cast: None,
        }
    }
}
#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Default, Hash)]
pub struct Identifier(pub Vec<char>);

impl PartialEq<&str> for Identifier {
    fn eq(&self, other: &&str) -> bool {
        self.0 == other.as_bytes()
    }
}

impl Identifier {
    pub fn fn_in() -> Self {
        Self(Vec::from(b"in"))
    }
    pub fn fn_out() -> Self {
        Self(Vec::from(b"out"))
    }
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn push(&mut self, x: char) {
        self.0.push(x);
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Self(s.chars().collect())
    }
}

impl From<&[char]> for Identifier {
    fn from(s: &[char]) -> Self {
        Self(Vec::from(s))
    }
}

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Identifier")
            .field(&std::str::from_utf8(&self.0))
            .finish()
    }
}
impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", std::str::from_utf8(&self.0).unwrap())
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Slice {
    pub start: Option<Offset>,
    pub stop: Option<Offset>,
}

impl std::fmt::Display for Slice {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match (&self.start, &self.stop) {
            (Some(start), Some(stop)) => write!(f, "{start}..{stop}"),
            (None, Some(stop)) => write!(f, "..{stop}"),
            (Some(start), None) => write!(f, "{start}.."),
            (None, None) => write!(f, ".."),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Offset {
    Integer(u64),
    Variable(Variable),
}

impl Default for Offset {
    fn default() -> Self {
        Self::Integer(Default::default())
    }
}

impl std::fmt::Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Integer(integer) => write!(f, "{integer}"),
            Self::Variable(variable) => write!(f, "{variable}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Cast {
    /// E.g. `x: i32`
    As(Type),
    /// E.g. `x: ?`
    ///
    /// Casts to the previous cast.
    /// ```text
    /// x:i32
    /// x:u8
    /// x:^ // Is identical to `x:i32`
    /// ```
    Prev,
}

impl std::fmt::Display for Cast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::As(cast_type) => write!(f, "{cast_type}"),
            Self::Prev => write!(f, "^"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Integer(IntegerType),
    Array(Box<Array>),
    Reference(Box<Type>),
    TypeType,
    Boolean
}

impl Default for Type {
    fn default() -> Self {
        Self::Integer(IntegerType::default())
    }
}

impl Type {
    pub fn bytes(&self) -> usize {
        use Type::*;
        match self {
            Integer(integer) => integer.bytes(),
            Reference(_) => 0,
            Array(array) => array.bytes(),
            TypeType => 0,
            Boolean => 1,
        }
    }
    /// Returns `Some(Self)` if `self` is an integer type.
    pub fn integer(&self) -> Option<&IntegerType> {
        use Type::*;
        match self {
            Integer(integer) => Some(integer),
            _ => None,
        }
    }
    pub fn reference(&self) -> Option<&Box<Type>> {
        use Type::*;
        match self {
            Reference(reference) => Some(reference),
            _ => None,
        }
    }
}

impl PartialEq<IntegerType> for Type {
    fn eq(&self, other: &IntegerType) -> bool {
        match self {
            Self::Integer(integer) => integer == other,
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Type::*;
        match self {
            Integer(integer) => write!(f, "{integer}"),
            Array(array) => write!(f, "{array}"),
            Reference(reference) => write!(f, "&{reference}"),
            TypeType => write!(f, "type"),
            Boolean => write!(f,"bool")
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum IntegerType {
    #[default]
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}

impl IntegerType {
    pub fn bytes(&self) -> usize {
        use IntegerType::*;
        match self {
            U8 => 1,
            U16 => 2,
            U32 => 4,
            U64 => 8,
            I8 => 1,
            I16 => 2,
            I32 => 4,
            I64 => 8,
        }
    }
}

impl std::fmt::Display for IntegerType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use IntegerType::*;
        match self {
            U8 => write!(f, "u8"),
            U16 => write!(f, "u16"),
            U32 => write!(f, "u32"),
            U64 => write!(f, "u64"),
            I8 => write!(f, "i8"),
            I16 => write!(f, "i16"),
            I32 => write!(f, "i32"),
            I64 => write!(f, "i64"),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Array(pub Vec<Type>);

impl Array {
    pub fn bytes(&self) -> usize {
        self.0.iter().map(|t| t.bytes()).sum()
    }
}

impl std::fmt::Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|t| t.to_string())
                .intersperse(String::from(" "))
                .collect::<String>()
        )
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Expression {
    pub op: Identifier,
    pub lhs: Arg,
    pub rhs: Nested,
    pub out: Option<Variable>,
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.lhs
                .iter()
                .map(|v| v.to_string())
                .intersperse(String::from(" "))
                .collect::<String>(),
            self.op,
            self.rhs,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Nested {
    Expression(Box<Expression>),
    Values(Arg),
}

impl std::fmt::Display for Nested {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Expression(expression) => write!(f, "{expression}"),
            Self::Values(arg) => write!(
                f,
                "{}",
                arg.iter()
                    .map(|v| v.to_string())
                    .intersperse(String::from(" "))
                    .collect::<String>()
            ),
        }
    }
}

impl Default for Nested {
    fn default() -> Self {
        Self::Values(Arg::default())
    }
}
