/// The AST maps closely to assembly for simplicity.
#[derive(Debug, Eq, PartialEq, Default)]
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

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Statement {
    pub op: Op,
    pub arg: Vec<Value>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Value {
    Literal(Literal),
    Variable(Variable),
}

impl Default for Value {
    fn default() -> Self {
        Self::Literal(Default::default())
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Literal(pub u64);

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Variable(pub Vec<u8>);

#[derive(Debug, Eq, PartialEq, Default)]
pub enum Intrinsic {
    #[default]
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    // beq
    IfEq,
    // blt
    IfLt,
    // bgt
    IfGt,
}

#[derive(Debug, Eq, PartialEq, Default)]
pub enum Syscall {
    #[default]
    Exit,
    Read,
    Write,
    MemfdCreate,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Op {
    Intrinsic(Intrinsic),
    Syscall(Syscall),
}

impl Default for Op {
    fn default() -> Self {
        Self::Intrinsic(Default::default())
    }
}
