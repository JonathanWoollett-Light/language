/// The AST maps closely to assembly for simplicity.
#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub struct Node {
    pub statement: Statement,
    pub child: Option<usize>,
    pub next: Option<usize>,
}
impl Node {
    pub fn new(statement: Statement) -> Self {
        Self {
            statement,
            child: None,
            next: None,
        }
    }
}

use std::ptr::NonNull;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Preceding {
    Parent(NonNull<NewNode>),
    Previous(NonNull<NewNode>),
}

pub struct NewNode {
    pub statement: Statement,
    pub preceding: Option<Preceding>,
    pub child: Option<NonNull<NewNode>>,
    pub next: Option<NonNull<NewNode>>,
}
impl NewNode {
    pub fn new(statement: Statement) -> Self {
        Self {
            statement,
            preceding: None,
            child: None,
            next: None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub struct Statement {
    pub comptime: bool,
    pub op: Op,
    pub arg: Arg,
}

impl std::fmt::Display for Statement {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "todo")
    }
}

pub type Arg = Vec<Value>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Literal(Literal),
    Variable(Variable),
    Type(Type),
}
impl Value {
    pub fn literal(&self) -> Option<&Literal> {
        match self {
            Self::Literal(literal) => Some(literal),
            _ => None,
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
    pub fn string(&self) -> Option<&String> {
        match self {
            Self::String(string) => Some(string),
            _ => None,
        }
    }
}

pub type Integer = i128;

impl Default for Literal {
    fn default() -> Self {
        Self::Integer(Default::default())
    }
}

#[derive(Eq, PartialEq, Default, Clone)]
pub struct Variable {
    pub identifier: Identifier,
    pub index: Option<Box<Index>>,
}
pub type Identifier = Vec<u8>;

impl std::fmt::Debug for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Variable")
            .field("identifier", &std::str::from_utf8(&self.identifier))
            .field("index", &self.index)
            .finish()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Index {
    Slice(Slice),
    Offset(Offset),
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Slice {
    pub start: Option<Offset>,
    pub stop: Option<Offset>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Offset {
    Integer(u64),
    Variable(Variable),
}

impl Default for Offset {
    fn default() -> Self {
        Self::Integer(Default::default())
    }
}

#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub enum Intrinsic {
    #[default]
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    If(Cmp),
    Loop,
    Break,
}

impl Intrinsic {
    pub fn arithmetic_assign(x: u8) -> Option<Self> {
        match x {
            b'+' => Some(Self::AddAssign),
            b'-' => Some(Self::SubAssign),
            b'*' => Some(Self::MulAssign),
            b'/' => Some(Self::DivAssign),
            b'%' => Some(Self::RemAssign),
            _ => None,
        }
    }
    pub fn arithmetic(x: u8) -> Option<Self> {
        match x {
            b'+' => Some(Self::Add),
            b'-' => Some(Self::Sub),
            b'*' => Some(Self::Mul),
            b'/' => Some(Self::Div),
            b'%' => Some(Self::Rem),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub enum Syscall {
    #[default]
    Exit,
    Read,
    Write,
    MemfdCreate,
    FTruncate,
    Mmap,
}
impl TryFrom<&[u8]> for Syscall {
    type Error = ();
    fn try_from(x: &[u8]) -> Result<Self, Self::Error> {
        match x {
            b"write" => Ok(Self::Write),
            b"read" => Ok(Self::Read),
            b"memfd_create" => Ok(Self::MemfdCreate),
            b"ftruncate" => Ok(Self::FTruncate),
            b"mmap" => Ok(Self::Mmap),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Special {
    Assume(Cmp),
    Require(Cmp),
    Type, // Type,
}

impl Default for Special {
    fn default() -> Self {
        Self::Assume(Default::default())
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum Type {
    #[default]
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    #[allow(dead_code)]
    Array(Box<Array>),
}
impl Type {
    pub fn bytes(&self) -> usize {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
            Self::Array(array) => array.bytes(),
        }
    }
    #[cfg(not(feature = "16"))]
    pub fn possible(x: i128) -> Vec<Self> {
        const I64_MIN: i128 = i64::MIN as i128;
        const I32_MIN: i128 = i32::MIN as i128;
        const I16_MIN: i128 = i16::MIN as i128;
        const I8_MIN: i128 = i8::MIN as i128;
        const U64_MAX: i128 = u64::MAX as i128;
        const U32_MAX: i128 = u32::MAX as i128;
        const U16_MAX: i128 = u16::MAX as i128;
        const U8_MAX: i128 = u8::MAX as i128;
        const U64_EDGE: i128 = u32::MAX as i128 + 1;
        const U32_EDGE: i128 = u16::MAX as i128 + 1;
        const U16_EDGE: i128 = u8::MAX as i128 + 1;

        match x {
            I64_MIN..I32_MIN => vec![Self::I64],
            I32_MIN..I16_MIN => vec![Self::I64, Self::I32],
            I16_MIN..I8_MIN => vec![Self::I64, Self::I32, Self::I16],
            I8_MIN..0 => vec![Self::I64, Self::I32, Self::I16, Self::I8],
            0..U8_MAX => vec![
                Self::I64,
                Self::I32,
                Self::I16,
                Self::I8,
                Self::U64,
                Self::U32,
                Self::U16,
                Self::U8,
            ],
            U8_MAX => vec![
                Self::I64,
                Self::I32,
                Self::I16,
                Self::U64,
                Self::U32,
                Self::U16,
                Self::U8,
            ],
            U16_EDGE..U16_MAX => vec![
                Self::I64,
                Self::I32,
                Self::I16,
                Self::U64,
                Self::U32,
                Self::U16,
            ],
            U16_MAX => vec![Self::I64, Self::I32, Self::U64, Self::U32, Self::U16],
            U32_EDGE..U32_MAX => vec![Self::I64, Self::I32, Self::U64, Self::U32],
            U32_MAX => vec![Self::I64, Self::U64, Self::U32],
            U64_EDGE..U64_MAX => vec![Self::I64, Self::U64],
            U64_MAX => vec![Self::U64],
            _ => panic!(),
        }
    }

    // A 16bit feature that reduces the set of types to `u8`, `i8` `u16` and `i16` to make debugging easier.
    #[cfg(feature = "16")]
    pub fn possible(x: i128) -> Vec<Self> {
        const I64_MIN: i128 = i64::MIN as i128;
        const I32_MIN: i128 = i32::MIN as i128;
        const I16_MIN: i128 = i16::MIN as i128;
        const I8_MIN: i128 = i8::MIN as i128;
        const U64_MAX: i128 = u64::MAX as i128;
        const U32_MAX: i128 = u32::MAX as i128;
        const U16_MAX: i128 = u16::MAX as i128;
        const U8_MAX: i128 = u8::MAX as i128;
        const U64_EDGE: i128 = u32::MAX as i128 + 1;
        const U32_EDGE: i128 = u16::MAX as i128 + 1;
        const U16_EDGE: i128 = u8::MAX as i128 + 1;

        match x {
            I16_MIN..I8_MIN => vec![Self::I16],
            I8_MIN..0 => vec![Self::I16, Self::I8],
            0..U8_MAX => vec![Self::I16, Self::I8, Self::U16, Self::U8],
            U8_MAX => vec![Self::I16, Self::U16, Self::U8],
            U16_EDGE..U16_MAX => vec![Self::I16, Self::U16],
            U16_MAX => vec![Self::U16],
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Array {
    pub item: Type,
    pub len: usize,
}

impl Array {
    pub fn bytes(&self) -> usize {
        self.len * self.item.bytes()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum Cmp {
    Gt,
    Lt,
    #[default]
    Eq,
    Ge,
    Le,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Intrinsic(Intrinsic),
    Syscall(Syscall),
    Special(Special),
}

impl Default for Op {
    fn default() -> Self {
        Self::Intrinsic(Default::default())
    }
}
