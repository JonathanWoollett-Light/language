/// The AST maps closely to assembly for simplicity.
use std::ptr::NonNull;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Preceding {
    Parent(NonNull<NewNode>),
    Previous(NonNull<NewNode>),
}

#[derive(Debug)]
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
    Register(Register),
}
impl Value {
    pub fn literal(&self) -> Option<&Literal> {
        match self {
            Self::Literal(literal) => Some(literal),
            _ => None,
        }
    }
    pub fn variable(&self) -> Option<&Variable> {
        match self {
            Self::Variable(variable) => Some(variable),
            _ => None,
        }
    }
    pub fn variable_mut(&mut self) -> Option<&mut Variable> {
        match self {
            Self::Variable(variable) => Some(variable),
            _ => None,
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
    W0,
    W1,
    W2,
    W3,
    W4,
    W5,
    W6,
    W7,
    W8,
    W9,
    W10,
    W11,
    W12,
    W13,
    W14,
    W15,
    W16,
    W17,
    W18,
    W19,
    W20,
    W21,
    W22,
    W23,
    W24,
    W25,
    W26,
    W27,
    W28,
    W29,
    W30,
    W31,
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
            b"w0" => Ok(Self::W0),
            b"w1" => Ok(Self::W1),
            b"w2" => Ok(Self::W2),
            b"w3" => Ok(Self::W3),
            b"w4" => Ok(Self::W4),
            b"w5" => Ok(Self::W5),
            b"w6" => Ok(Self::W6),
            b"w7" => Ok(Self::W7),
            b"w8" => Ok(Self::W8),
            b"w9" => Ok(Self::W9),
            b"w10" => Ok(Self::W10),
            b"w11" => Ok(Self::W11),
            b"w12" => Ok(Self::W12),
            b"w13" => Ok(Self::W13),
            b"w14" => Ok(Self::W14),
            b"w15" => Ok(Self::W15),
            b"w16" => Ok(Self::W16),
            b"w17" => Ok(Self::W17),
            b"w18" => Ok(Self::W18),
            b"w19" => Ok(Self::W19),
            b"w20" => Ok(Self::W20),
            b"w21" => Ok(Self::W21),
            b"w22" => Ok(Self::W22),
            b"w23" => Ok(Self::W23),
            b"w24" => Ok(Self::W24),
            b"w25" => Ok(Self::W25),
            b"w26" => Ok(Self::W26),
            b"w27" => Ok(Self::W27),
            b"w28" => Ok(Self::W28),
            b"w29" => Ok(Self::W29),
            b"w30" => Ok(Self::W30),
            _ => Err(()),
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
            Self::W0 => write!(f, "w0"),
            Self::W1 => write!(f, "w1"),
            Self::W2 => write!(f, "w2"),
            Self::W3 => write!(f, "w3"),
            Self::W4 => write!(f, "w4"),
            Self::W5 => write!(f, "w5"),
            Self::W6 => write!(f, "w6"),
            Self::W7 => write!(f, "w7"),
            Self::W8 => write!(f, "w8"),
            Self::W9 => write!(f, "w9"),
            Self::W10 => write!(f, "w10"),
            Self::W11 => write!(f, "w11"),
            Self::W12 => write!(f, "w12"),
            Self::W13 => write!(f, "w13"),
            Self::W14 => write!(f, "w14"),
            Self::W15 => write!(f, "w15"),
            Self::W16 => write!(f, "w16"),
            Self::W17 => write!(f, "w17"),
            Self::W18 => write!(f, "w18"),
            Self::W19 => write!(f, "w19"),
            Self::W20 => write!(f, "w20"),
            Self::W21 => write!(f, "w21"),
            Self::W22 => write!(f, "w22"),
            Self::W23 => write!(f, "w23"),
            Self::W24 => write!(f, "w24"),
            Self::W25 => write!(f, "w25"),
            Self::W26 => write!(f, "w26"),
            Self::W27 => write!(f, "w27"),
            Self::W28 => write!(f, "w28"),
            Self::W29 => write!(f, "w29"),
            Self::W30 => write!(f, "w30"),
            Self::W31 => write!(f, "w31"),
        }
    }
}

impl TryFrom<&Variable> for Register {
    type Error = ();
    fn try_from(Variable { identifier, index }: &Variable) -> Result<Self, Self::Error> {
        if *index == None
            && let Ok(register) = Register::try_from(identifier.as_slice())
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

#[derive(Eq, PartialEq, Default, Clone, Hash)]
pub struct Variable {
    pub identifier: Identifier,
    pub index: Option<Box<Index>>,
}

impl From<&str> for Variable {
    fn from(bytes: &str) -> Self {
        Self {
            identifier: Identifier::from(bytes.as_bytes()),
            index: None,
        }
    }
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

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Index {
    Slice(Slice),
    Offset(Offset),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Slice {
    pub start: Option<Offset>,
    pub stop: Option<Offset>,
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

#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub enum Intrinsic {
    #[default]
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    If(Cmp),
    Loop,
    Break,
    Def,
    Call,
}

impl Intrinsic {
    pub fn arithmetic_assign(x: u8) -> Option<Self> {
        match x {
            b'+' => Some(Self::AddAssign),
            b'-' => Some(Self::SubAssign),
            b'*' => Some(Self::MulAssign),
            b'/' => Some(Self::DivAssign),
            b'%' => Some(Self::RemAssign),
            b'&' => Some(Self::AndAssign),
            b'|' => Some(Self::OrAssign),
            b'^' => Some(Self::XorAssign),
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
            b'&' => Some(Self::And),
            b'|' => Some(Self::Or),
            b'^' => Some(Self::Xor),
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
            b"exit" => Ok(Self::Exit),
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
    Unreachable,
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
            0..U8_MAX => {
                vec![
                    Self::I64,
                    Self::I32,
                    Self::I16,
                    Self::I8,
                    Self::U64,
                    Self::U32,
                    Self::U16,
                    Self::U8,
                ]
            }
            U8_MAX => vec![
                Self::I64,
                Self::I32,
                Self::I16,
                Self::U64,
                Self::U32,
                Self::U16,
                Self::U8,
            ],
            U16_EDGE..U16_MAX => {
                vec![
                    Self::I64,
                    Self::I32,
                    Self::I16,
                    Self::U64,
                    Self::U32,
                    Self::U16,
                ]
            }
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
    Assembly(Assembly),
}

impl Default for Op {
    fn default() -> Self {
        Self::Intrinsic(Default::default())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Assembly {
    Svc,
    Mov,
}
impl Default for Assembly {
    fn default() -> Self {
        Self::Svc
    }
}
