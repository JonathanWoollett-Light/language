use std::iter::once;
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
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.op {
            Op::Intrinsic(Intrinsic::Assign) => match self.arg.as_slice() {
                [Value::Variable(variable), tail @ ..] => {
                    write!(
                        f,
                        "{variable} := {}",
                        tail.iter()
                            .map(|v| v.to_string())
                            .intersperse(String::from(" "))
                            .collect::<String>()
                    )
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Call) => write!(
                f,
                "{}",
                self.arg
                    .iter()
                    .map(|v| v.to_string())
                    .intersperse(String::from(" "))
                    .collect::<String>()
            ),
            Op::Special(Special::SizeOf) => match self.arg.as_slice() {
                [lhs, rhs] => write!(f, "{lhs} := sizeof {rhs}"),
                _ => todo!(),
            },
            Op::Assembly(Assembly::Mov) => match self.arg.as_slice() {
                [lhs, rhs] => write!(f, "mov {lhs} {rhs}"),
                _ => todo!(),
            },
            Op::Assembly(Assembly::Svc) => match self.arg.as_slice() {
                [value] => write!(f, "svc {value}"),
                _ => todo!(),
            },
            Op::Special(Special::Unreachable) => write!(f, "unreachable"),
            Op::Special(Special::Type) => match self.arg.as_slice() {
                [rhs] => write!(f, "{rhs}"),
                [rhs, rhs_type] => write!(f, "{rhs} : {rhs_type}"),
                [rhs, rhs_type, lhs, tail @ ..] => write!(
                    f,
                    "{rhs} : {rhs_type} := {}",
                    once(lhs)
                        .chain(tail.iter())
                        .map(|x| x.to_string())
                        .intersperse(String::from(" "))
                        .collect::<String>()
                ),
                x @ _ => todo!("{x:?}"),
            },
            Op::Intrinsic(Intrinsic::Def) => match self.arg.as_slice() {
                [x] => write!(f, "def {x}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::If(Cmp::Eq)) => match self.arg.as_slice() {
                [lhs, rhs] => write!(f, "if {lhs} = {rhs}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Add) => match self.arg.as_slice() {
                [a, b, c] => write!(f, "{a} := {b} + {c}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Sub) => match self.arg.as_slice() {
                [a, b, c] => write!(f, "{a} := {b} - {c}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Div) => match self.arg.as_slice() {
                [a, b, c] => write!(f, "{a} := {b} / {c}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Mul) => match self.arg.as_slice() {
                [a, b, c] => write!(f, "{a} := {b} * {c}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Or) => match self.arg.as_slice() {
                [a, b, c] => write!(f, "{a} := {b} | {c}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::And) => match self.arg.as_slice() {
                [a, b, c] => write!(f, "{a} := {b} & {c}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Xor) => match self.arg.as_slice() {
                [a, b, c] => write!(f, "{a} := {b} ^ {c}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::AddAssign) => match self.arg.as_slice() {
                [a, b] => write!(f, "{a} += {b}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::SubAssign) => match self.arg.as_slice() {
                [a, b] => write!(f, "{a} -= {b}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::DivAssign) => match self.arg.as_slice() {
                [a, b] => write!(f, "{a} /= {b}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::MulAssign) => match self.arg.as_slice() {
                [a, b] => write!(f, "{a} *= {b}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::OrAssign) => match self.arg.as_slice() {
                [a, b] => write!(f, "{a} |= {b}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::AndAssign) => match self.arg.as_slice() {
                [a, b] => write!(f, "{a} &= {b}"),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::XorAssign) => match self.arg.as_slice() {
                [a, b] => write!(f, "{a} ^= {b}"),
                _ => todo!(),
            },
            x @ _ => todo!("{x:?}"),
        }
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
    pub fn variable_mut(&mut self) -> Option<&mut Variable> {
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
    pub fn string(&self) -> Option<&String> {
        match self {
            Self::String(string) => Some(string),
            _ => None,
        }
    }
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
    pub addressing: Addressing,
    pub identifier: Identifier,
    pub index: Option<Box<Index>>,
}

impl From<Identifier> for Variable {
    fn from(identifier: Identifier) -> Self {
        Self {
            addressing: Addressing::Direct,
            identifier,
            index: None,
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
            "{}{}{}",
            self.addressing,
            self.identifier,
            self.index
                .as_ref()
                .map(|v| format!("[{v}]"))
                .unwrap_or(String::new())
        )
    }
}

impl From<&str> for Variable {
    fn from(s: &str) -> Self {
        Self {
            addressing: Addressing::Direct,
            identifier: Identifier::from(s),
            index: None,
        }
    }
}
#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Default, Hash)]
pub struct Identifier(pub Vec<u8>);

impl PartialEq<&str> for Identifier {
    fn eq(&self, other: &&str) -> bool {
        self.0 == other.as_bytes()
    }
}

impl Identifier {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn push(&mut self, x: u8) {
        self.0.push(x);
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Self(Vec::from(s.as_bytes()))
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
pub enum Index {
    Slice(Slice),
    Offset(Offset),
}

impl std::fmt::Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Slice(slice) => write!(f, "{slice}"),
            Self::Offset(offset) => write!(f, "{offset}"),
        }
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Special {
    Assume(Cmp),
    Require(Cmp),
    Type, // Type,
    Unreachable,
    SizeOf,
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
    Reference,
}

impl Type {
    pub fn bytes(&self) -> usize {
        match self {
            Self::Reference => 0,
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
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::Array(array) => write!(f, "{array}"),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
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
