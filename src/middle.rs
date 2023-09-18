use crate::ast::*;
use std::cmp::Ordering;
use std::collections::HashMap;

// An inclusive range that supports wrapping around.
#[derive(Debug, Clone)]
pub struct MyRange<T> {
    start: T,
    end: T,
}
impl<T: Copy + Ord + num_traits::bounds::Bounded> MyRange<T> {
    fn new(start: T, end: T) -> Self {
        Self { start, end }
    }
    fn less_than(&self, x: T) -> bool {
        self.max() < x
    }
    fn greater_than(&self, x: T) -> bool {
        self.min() > x
    }
    fn max(&self) -> T {
        match self.start.cmp(&self.end) {
            Ordering::Greater => T::max_value(),
            Ordering::Equal => self.start,
            Ordering::Less => self.end,
        }
    }
    fn min(&self) -> T {
        match self.start.cmp(&self.end) {
            Ordering::Greater => T::min_value(),
            Ordering::Equal => self.end,
            Ordering::Less => self.start,
        }
    }
}

pub fn explore(nodes: &[Node]) -> Vec<State> {
    assert!(!nodes.is_empty());
    let node = &nodes[0];
    let mut end_states = Vec::new();
    let initial_states = evaluate_states(
        &State {
            values: HashMap::new(),
        },
        &node.statement,
    );
    let mut stack = initial_states
        .into_iter()
        .map(|state| GraphNode {
            state,
            child: node.child,
            next: node.next,
        })
        .collect::<Vec<_>>();

    while let Some(current) = stack.pop() {
        match (current.next, current.child) {
            (Some(next), Some(child)) => {
                for state in evaluate_states(&current.state, &nodes[next].statement) {
                    stack.push(GraphNode {
                        state,
                        child: nodes[next].child,
                        next: nodes[next].next,
                    });
                }
                for state in evaluate_states(&current.state, &nodes[child].statement) {
                    stack.push(GraphNode {
                        state,
                        child: nodes[child].child,
                        next: nodes[child].next,
                    });
                }
            }
            (Some(next), None) => {
                for state in evaluate_states(&current.state, &nodes[next].statement) {
                    stack.push(GraphNode {
                        state,
                        child: nodes[next].child,
                        next: nodes[next].next,
                    });
                }
            }
            (None, Some(child)) => {
                for state in evaluate_states(&current.state, &nodes[child].statement) {
                    stack.push(GraphNode {
                        state,
                        child: nodes[child].child,
                        next: nodes[child].next,
                    });
                }
            }
            (None, None) => end_states.push(current.state),
        }
    }
    end_states
}

fn evaluate_states(start: &State, statement: &Statement) -> Vec<State> {
    match statement {
        Statement {
            runtime: false,
            op: Op::Intrinsic(Intrinsic::Assign),
            arg
        } if let Some([Value::Variable(Variable { identifier, index: None }), Value::Literal(Literal::Integer(x))]) = arg.get(..) => {
            let possible = NewValueInteger::possible(*x);

            match start.values.get(identifier) {
                Some(NewValue::Integer(existing)) => {
                    // If the current value in contained in the set of possible values for this
                    // assignment, continue, else end as this path is invalid.
                    if let Some(x) = possible.iter().find(|x|x.type_index()==existing.type_index()) {
                        let mut state = start.clone();
                        *state.values.get_mut(identifier).unwrap() = NewValue::Integer(x.clone());
                        vec![state]
                    }
                    else {
                        Vec::new()
                    }
                },
                None => {
                    possible.into_iter().map(|p| {
                        let mut state = start.clone();
                        state.values.insert(identifier.clone(), NewValue::Integer(p.clone()));
                        state
                    }).collect()
                }
                Some(_) => panic!()
            }
        },
        Statement {
            runtime: false,
            op: Op::Intrinsic(Intrinsic::AddAssign),
            arg
        } if let Some([Value::Variable(Variable { identifier, index: None }), Value::Literal(Literal::Integer(x))]) = arg.get(..) => {
            let Some(NewValue::Integer(existing)) = start.values.get(identifier) else {
                panic!()
            };
            match existing.overflowing_add(*x) {
                Ok(b) => {
                    let mut state = start.clone();
                    let Some(NewValue::Integer(c)) = state.values.get_mut(identifier) else {
                        panic!()
                    };
                    *c = b;
                    vec![state]
                },
                Err(_) => Vec::new()
            }
        }
        Statement {
            runtime: false,
            op: Op::Special(Special::Require(Cmp::Gt)),
            arg
        } if let Some([Value::Variable(Variable { identifier, index: None }), Value::Literal(Literal::Integer(x))]) = arg.get(..) => {
            if let Some(NewValue::Integer(existing)) = start.values.get(identifier) && let Ok(true) = existing.greater_than(*x) {
                vec![start.clone()]
            }
            else {
                Vec::new()
            }
        }
        Statement {
            runtime: false,
            op: Op::Special(Special::Require(Cmp::Lt)),
            arg
        } if let Some([Value::Variable(Variable { identifier, index: None }), Value::Literal(Literal::Integer(x))]) = arg.get(..) => {
            if let Some(NewValue::Integer(existing)) = start.values.get(identifier) && let Ok(true) = existing.less_than(*x) {
                vec![start.clone()]
            }
            else {
                Vec::new()
            }
        }
        _ => todo!()
    }
}

#[derive(Debug, Clone)]
struct GraphNode {
    state: State,
    child: Option<usize>,
    next: Option<usize>,
}
#[derive(Debug, Clone)]
pub struct State {
    values: HashMap<Vec<u8>, NewValue>,
}

#[derive(Debug, Clone)]
pub enum NewValue {
    Integer(NewValueInteger),
}

#[derive(Debug, Clone)]
pub enum NewValueInteger {
    U8(MyRange<u8>),
    U16(MyRange<u16>),
    U32(MyRange<u32>),
    U64(MyRange<u64>),
    I8(MyRange<i8>),
    I16(MyRange<i16>),
    I32(MyRange<i32>),
    I64(MyRange<i64>),
}
impl NewValueInteger {
    fn type_index(&self) -> u8 {
        match self {
            Self::U8(_) => 0,
            Self::U16(_) => 1,
            Self::U32(_) => 2,
            Self::U64(_) => 3,
            Self::I8(_) => 4,
            Self::I16(_) => 5,
            Self::I32(_) => 6,
            Self::I64(_) => 7,
        }
    }
    fn less_than(&self, rhs: i128) -> Result<bool, ()> {
        match self {
            Self::U8(range) => {
                let rhs = u8::try_from(rhs).map_err(drop)?;
                Ok(range.less_than(rhs))
            }
            Self::U16(range) => {
                let rhs = u16::try_from(rhs).map_err(drop)?;
                Ok(range.less_than(rhs))
            }
            Self::U32(range) => {
                let rhs = u32::try_from(rhs).map_err(drop)?;
                Ok(range.less_than(rhs))
            }
            Self::U64(range) => {
                let rhs = u64::try_from(rhs).map_err(drop)?;
                Ok(range.less_than(rhs))
            }
            Self::I8(range) => {
                let rhs = i8::try_from(rhs).map_err(drop)?;
                Ok(range.less_than(rhs))
            }
            Self::I16(range) => {
                let rhs = i16::try_from(rhs).map_err(drop)?;
                Ok(range.less_than(rhs))
            }
            Self::I32(range) => {
                let rhs = i32::try_from(rhs).map_err(drop)?;
                Ok(range.less_than(rhs))
            }
            Self::I64(range) => {
                let rhs = i64::try_from(rhs).map_err(drop)?;
                Ok(range.less_than(rhs))
            }
        }
    }
    fn greater_than(&self, rhs: i128) -> Result<bool, ()> {
        match self {
            Self::U8(range) => {
                let rhs = u8::try_from(rhs).map_err(drop)?;
                Ok(range.greater_than(rhs))
            }
            Self::U16(range) => {
                let rhs = u16::try_from(rhs).map_err(drop)?;
                Ok(range.greater_than(rhs))
            }
            Self::U32(range) => {
                let rhs = u32::try_from(rhs).map_err(drop)?;
                Ok(range.greater_than(rhs))
            }
            Self::U64(range) => {
                let rhs = u64::try_from(rhs).map_err(drop)?;
                Ok(range.greater_than(rhs))
            }
            Self::I8(range) => {
                let rhs = i8::try_from(rhs).map_err(drop)?;
                Ok(range.greater_than(rhs))
            }
            Self::I16(range) => {
                let rhs = i16::try_from(rhs).map_err(drop)?;
                Ok(range.greater_than(rhs))
            }
            Self::I32(range) => {
                let rhs = i32::try_from(rhs).map_err(drop)?;
                Ok(range.greater_than(rhs))
            }
            Self::I64(range) => {
                let rhs = i64::try_from(rhs).map_err(drop)?;
                Ok(range.greater_than(rhs))
            }
        }
    }
    fn overflowing_add(&self, rhs: i128) -> Result<Self, ()> {
        match self {
            Self::U8(range) => {
                let rhs = u8::try_from(rhs).map_err(drop)?;
                Ok(Self::U8(MyRange::new(
                    range.start.overflowing_add(rhs).0,
                    range.end.overflowing_add(rhs).0,
                )))
            }
            Self::U16(range) => {
                let rhs = u16::try_from(rhs).map_err(drop)?;
                Ok(Self::U16(MyRange::new(
                    range.start.overflowing_add(rhs).0,
                    range.end.overflowing_add(rhs).0,
                )))
            }
            Self::U32(range) => {
                let rhs = u32::try_from(rhs).map_err(drop)?;
                Ok(Self::U32(MyRange::new(
                    range.start.overflowing_add(rhs).0,
                    range.end.overflowing_add(rhs).0,
                )))
            }
            Self::U64(range) => {
                let rhs = u64::try_from(rhs).map_err(drop)?;
                Ok(Self::U64(MyRange::new(
                    range.start.overflowing_add(rhs).0,
                    range.end.overflowing_add(rhs).0,
                )))
            }
            Self::I8(range) => {
                let rhs = i8::try_from(rhs).map_err(drop)?;
                Ok(Self::I8(MyRange::new(
                    range.start.overflowing_add(rhs).0,
                    range.end.overflowing_add(rhs).0,
                )))
            }
            Self::I16(range) => {
                let rhs = i16::try_from(rhs).map_err(drop)?;
                Ok(Self::I16(MyRange::new(
                    range.start.overflowing_add(rhs).0,
                    range.end.overflowing_add(rhs).0,
                )))
            }
            Self::I32(range) => {
                let rhs = i32::try_from(rhs).map_err(drop)?;
                Ok(Self::I32(MyRange::new(
                    range.start.overflowing_add(rhs).0,
                    range.end.overflowing_add(rhs).0,
                )))
            }
            Self::I64(range) => {
                let rhs = i64::try_from(rhs).map_err(drop)?;
                Ok(Self::I64(MyRange::new(
                    range.start.overflowing_add(rhs).0,
                    range.end.overflowing_add(rhs).0,
                )))
            }
        }
    }
    fn possible(x: i128) -> Vec<Self> {
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
            I64_MIN..I32_MIN => vec![Self::I64(MyRange::new(x as i64, x as i64))],
            I32_MIN..I16_MIN => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::I32(MyRange::new(x as i32, x as i32)),
            ],
            I16_MIN..I8_MIN => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::I32(MyRange::new(x as i32, x as i32)),
                Self::I16(MyRange::new(x as i16, x as i16)),
            ],
            I8_MIN..0 => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::I32(MyRange::new(x as i32, x as i32)),
                Self::I16(MyRange::new(x as i16, x as i16)),
                Self::I8(MyRange::new(x as i8, x as i8)),
            ],
            0..U8_MAX => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::I32(MyRange::new(x as i32, x as i32)),
                Self::I16(MyRange::new(x as i16, x as i16)),
                Self::I8(MyRange::new(x as i8, x as i8)),
                Self::U64(MyRange::new(x as u64, x as u64)),
                Self::U32(MyRange::new(x as u32, x as u32)),
                Self::U16(MyRange::new(x as u16, x as u16)),
                Self::U8(MyRange::new(x as u8, x as u8)),
            ],
            U8_MAX => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::I32(MyRange::new(x as i32, x as i32)),
                Self::I16(MyRange::new(x as i16, x as i16)),
                Self::U64(MyRange::new(x as u64, x as u64)),
                Self::U32(MyRange::new(x as u32, x as u32)),
                Self::U16(MyRange::new(x as u16, x as u16)),
                Self::U8(MyRange::new(x as u8, x as u8)),
            ],
            U16_EDGE..U16_MAX => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::I32(MyRange::new(x as i32, x as i32)),
                Self::I16(MyRange::new(x as i16, x as i16)),
                Self::U64(MyRange::new(x as u64, x as u64)),
                Self::U32(MyRange::new(x as u32, x as u32)),
                Self::U16(MyRange::new(x as u16, x as u16)),
            ],
            U16_MAX => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::I32(MyRange::new(x as i32, x as i32)),
                Self::U64(MyRange::new(x as u64, x as u64)),
                Self::U32(MyRange::new(x as u32, x as u32)),
                Self::U16(MyRange::new(x as u16, x as u16)),
            ],
            U32_EDGE..U32_MAX => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::I32(MyRange::new(x as i32, x as i32)),
                Self::U64(MyRange::new(x as u64, x as u64)),
                Self::U32(MyRange::new(x as u32, x as u32)),
            ],
            U32_MAX => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::U64(MyRange::new(x as u64, x as u64)),
                Self::U32(MyRange::new(x as u32, x as u32)),
            ],
            U64_EDGE..U64_MAX => vec![
                Self::I64(MyRange::new(x as i64, x as i64)),
                Self::U64(MyRange::new(x as u64, x as u64)),
            ],
            U64_MAX => vec![Self::U64(MyRange::new(x as u64, x as u64))],
            _ => panic!(),
        }
    }
}

pub fn optimize_nodes(nodes: &[Node]) -> Vec<Node> {
    let mut output = Vec::new();

    // Stores types for each variable.
    let mut type_map = HashMap::new();

    assert!(!nodes.is_empty());
    let mut stack = vec![0];
    while let Some(current) = stack.pop() {
        // Set node
        let node = &nodes[current];
        if let Some(next) = node.next {
            stack.push(next);
        }
        if let Some(child) = node.child {
            stack.push(child)
        }

        match &node.statement {
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::MemfdCreate),
                arg,
            } => {
                let Some(
                    [Value::Variable(Variable {
                        identifier,
                        index: _,
                    }), ..],
                ) = arg.get(..)
                else {
                    panic!()
                };
                type_map.insert(identifier, Type::FILE_DESCRIPTOR);
                output.push(node.clone());
            }
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::Mmap),
                arg,
            } => {
                let Some(
                    [Value::Variable(Variable {
                        identifier,
                        index: None,
                    }), ..],
                ) = arg.get(..)
                else {
                    panic!()
                };
                type_map.insert(identifier, Type::Pointer(Pointer { item: None }));
                output.push(node.clone());
            }
            Statement {
                runtime: _,
                op: Op::Intrinsic(Intrinsic::Assign),
                arg,
            } => {
                let Some(
                    [Value::Variable(Variable {
                        identifier,
                        index: None,
                    }), tail @ ..],
                ) = arg.get(..)
                else {
                    todo!()
                };
                match tail {
                    [] | [Value::Literal(Literal::Integer(_))] => {}
                    [Value::Literal(Literal::String(s))] => {
                        type_map.insert(
                            identifier,
                            Type::Array(Array {
                                item: Box::new(Type::Integer(Integer(8))),
                                length: s.len() as u64,
                            }),
                        );
                    }
                    _ => {
                        type_map.insert(
                            identifier,
                            Type::Array(Array {
                                item: Box::new(Type::Integer(Integer(8))),
                                length: tail.len() as u64,
                            }),
                        );
                    }
                }
                output.push(node.clone());
            }
            // E.g. `x = read 1` which reads from `STDIN` the number of bytes required for the type of `x`, in this case the type isn't specified so we need to figure it (and defaulting to `i32` if we can't).
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::Read),
                arg,
            } => {
                if let Some([x, _fd]) = arg.get(..) {
                    let Value::Variable(Variable {
                        identifier,
                        index: None,
                    }) = x
                    else {
                        todo!()
                    };

                    let val_type = if let Some(val_type) = type_map.get(&identifier) {
                        val_type.clone()
                    } else {
                        let val_type = search(identifier, current, nodes);
                        type_map.insert(identifier, val_type.clone());
                        val_type
                    };
                    let n = val_type.bytes();

                    // This is wrong, but it works for now.
                    let mut new_node = node.clone();
                    new_node
                        .statement
                        .arg
                        .push(Value::Literal(Literal::Integer(n as _)));
                    output.push(new_node);
                }
            }
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::Write),
                arg,
            } => {
                match arg.get(..) {
                    Some([_result, _fd, x]) => match x {
                        Value::Variable(Variable {
                            identifier,
                            index: None,
                        }) => {
                            let val_type = type_map.get(&identifier).unwrap();
                            let n = val_type.bytes();

                            // This is wrong, but it works for now.
                            let mut new_node = node.clone();
                            new_node
                                .statement
                                .arg
                                .push(Value::Literal(Literal::Integer(n as _)));
                            output.push(new_node);
                        }
                        Value::Variable(Variable {
                            identifier,
                            index: Some(box Index::Offset(_)),
                        }) => {
                            let val_type = type_map
                                .get(&identifier)
                                .unwrap_or_else(|| panic!("{:?}", std::str::from_utf8(identifier)));

                            let n = match val_type {
                                Type::Array(Array { item, length: _ }) => item.bytes(),
                                Type::Pointer(Pointer {
                                    item: Some(box item),
                                }) => item.bytes(),
                                _ => panic!("{:?}", val_type),
                            };

                            // This is wrong, but it works for now.
                            let mut new_node = node.clone();
                            new_node
                                .statement
                                .arg
                                .push(Value::Literal(Literal::Integer(n as _)));
                            output.push(new_node);
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
            // This is wrong, but it works for now.
            _ => output.push(node.clone()),
        }
    }
    output
}

#[derive(Debug, Clone)]
enum Type {
    Integer(Integer),
    Array(Array),
    Pointer(Pointer),
}
impl Type {
    const FILE_DESCRIPTOR: Self = Self::Integer(Integer(32));
    fn bytes(&self) -> u64 {
        self.bits().div_ceil(8)
    }
    fn bits(&self) -> u64 {
        match self {
            Self::Integer(Integer(n)) => *n,
            Self::Array(Array { item, length }) => item.bits() * length,
            Self::Pointer(_) => 8,
        }
    }
}

#[derive(Debug, Clone)]
struct Pointer {
    item: Option<Box<Type>>,
}

#[derive(Debug, Clone)]
struct Array {
    /// The type of elements in the array
    item: Box<Type>,
    /// The number of elements in the array
    length: u64,
}
/// The bits in size.
#[derive(Debug, Clone)]
struct Integer(u64);

/// Searches through the AST to figure out the type of a variable.
fn search(identifier: &[u8], start: usize, nodes: &[Node]) -> Type {
    let mut size: Option<u64> = None;

    let mut stack = vec![start];
    while let Some(current) = stack.pop() {
        // Set node
        let node = &nodes[current];
        if let Some(next) = node.next {
            stack.push(next);
        }
        if let Some(child) = node.child {
            stack.push(child)
        }

        // Look at node
        match &node.statement {
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::Read),
                arg,
            } if let Some([x,_fd,n]) = arg.get(..) && let Value::Variable(Variable { identifier: x, index: None }) = x && identifier == x => {
                if let Some(m) = size {
                    match n {
                        Value::Literal(Literal::Integer(n)) => {
                            assert_eq!(m, *n as _, "Cannot read 2 different number of bytes into a single type of 1 size.");
                        },
                        _ => todo!()
                    }
                }
                else {
                    match n {
                        Value::Literal(Literal::Integer(n)) => {
                            size = Some(*n as _);
                        },
                        _ => todo!()
                    }
                }
            },
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::MemfdCreate),
                arg,
            } if let Some([x]) = arg.get(..) && let Value::Variable(Variable { identifier: x, index: None }) = x && identifier == x => {
                assert!(size.is_none() || size == Some(32));
                return Type::FILE_DESCRIPTOR;
            },
            _ => continue,
        }
    }
    match size {
        // Default to `i32`.
        None => Type::Integer(Integer(32)),
        // Default to signed integer.
        Some(n) => Type::Integer(Integer(n)),
    }
}
