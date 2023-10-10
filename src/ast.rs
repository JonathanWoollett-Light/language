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

#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub struct Statement {
    pub comptime: bool,
    pub op: Op,
    pub arg: Arg,
}

pub type Arg = Vec<Value>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Literal(Literal),
    Variable(Variable),
    Type(Type),
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

#[derive(Debug, Eq, PartialEq, Default, Clone)]
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
}

#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub enum Cmp {
    Gt,
    Lt,
    #[default]
    Eq,
    Ge,
    Le,
}

#[derive(Debug, Eq, PartialEq, Clone)]
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
