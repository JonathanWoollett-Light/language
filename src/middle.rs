use crate::ast::*;
use num_traits::bounds::Bounded;
use num_traits::identities::One;
use num_traits::identities::Zero;
use std::cmp::Ordering;
use std::collections::HashMap;



use tracing::instrument;
use tracing::info;

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// An inclusive range that supports wrapping around.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MyRange<
    T: Copy + Ord + std::ops::Sub<Output = T> + std::ops::Add<Output = T> + Bounded + Zero + One,
> {
    start: T,
    end: T,
}
impl<
        T: Copy + Ord + std::ops::Sub<Output = T> + std::ops::Add<Output = T> + Bounded + Zero + One,
    > Ord for MyRange<T>
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.len().cmp(&other.len())
    }
}

impl<
        T: Copy + Ord + std::ops::Sub<Output = T> + std::ops::Add<Output = T> + Bounded + Zero + One,
    > PartialOrd for MyRange<T>
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<
        T: Copy + Ord + std::ops::Sub<Output = T> + std::ops::Add<Output = T> + Bounded + Zero + One,
    > MyRange<T>
{
    fn len(&self) -> T {
        match self.start.cmp(&self.end) {
            Ordering::Greater => (T::max_value() - self.start) + self.end + T::one(),
            Ordering::Equal => T::zero(),
            Ordering::Less => self.end - self.start,
        }
    }
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

// Runs updates until there is no change.
#[instrument(level = "TRACE", skip(nodes))]
pub fn multi_update(nodes: &[Node]) -> Vec<Node> {
    let mut state = get_states_intersection(nodes);
    info!("state: {state:?}");
    let mut new_nodes = evaluate(nodes, &state);

    if new_nodes != nodes {
        loop {
            let prev = new_nodes;
            state = get_states_intersection(&prev);
            new_nodes = evaluate(&prev, &state);
            if prev == new_nodes {
                break;
            }
        }
    }

    // Adds the type definitions into the code
    {
        let iter_one = state
            .values
            .iter()
            .enumerate()
            .map(|(i, (key, value))| Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable {
                            identifier: key.clone(),
                            index: None,
                        }),
                        Value::Type(value.type_value()),
                    ],
                },
                child: None,
                next: Some(i + 1),
            });
        let iter_two = new_nodes.iter().map(|node| Node {
            statement: node.statement.clone(),
            child: node.child.map(|c| c + state.values.len()),
            next: node.next.map(|n| n + state.values.len()),
        });
        iter_one.chain(iter_two).collect()
    }
}

// Optimizes the code using the given state.
#[instrument(level = "TRACE", skip(nodes))]
pub fn evaluate(nodes: &[Node], state: &State) -> Vec<Node> {
    if nodes.is_empty() {
        return Vec::new();
    }
    let mut stack = vec![0];
    let mut new_nodes = Vec::new();
    // let allocated = std::collections::HashSet::new();
    while let Some(i) = stack.pop() {
        let node = &nodes[i];
        match node.statement.op {
            Op::Syscall(Syscall::Exit) => {
                new_nodes.push(Node {
                    statement: node.statement.clone(),
                    child: None,
                    next: None,
                });
            }
            // TODO Only add the asssignment if it is used.
            Op::Intrinsic(Intrinsic::Assign) => {
                if appears(nodes, node) {
                    new_nodes.push(node.clone());
                }
            }
            _ => todo!(),
        }
        // stack.push()
    }
    new_nodes
}

// Returns if a variable appears anywhere in the given nodes.
#[instrument(level = "TRACE", skip(nodes))]
pub fn appears(nodes: &[Node], current: &Node) -> bool {
    let [Value::Variable(Variable {
        identifier: current_identifier,
        index: _,
    }), ..] = current.statement.arg.as_slice()
    else {
        panic!()
    };

    let mut stack = Vec::new();
    if let Some(next) = current.next {
        stack.push(next);
    }
    if let Some(child) = current.child {
        stack.push(child);
    }
    while let Some(i) = stack.pop() {
        let node = &nodes[i];
        match node.statement.op {
            Op::Intrinsic(Intrinsic::Assign) => match node.statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                }), ..] => {
                    if identifier == current_identifier {
                        return true;
                    }
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Exit) => match node.statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                })] => {
                    if identifier == current_identifier {
                        return true;
                    }
                }
                [Value::Literal(_)] => continue,
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
    false
}

// Compute a state from exploring the code.
#[cfg_attr(test, instrument(level = "TRACE", skip(nodes)))]
pub fn get_states_intersection(nodes: &[Node]) -> State {
    let states = explore(nodes);
    let intersection = state_intersection(&states);
    intersection
}

/// Given a set of states return a state that represents the intersections of all states in the set.
#[cfg_attr(test, instrument(level = "TRACE", ret, skip(states)))]
pub fn state_intersection(states: &[State]) -> State {
    let mut intersections = HashMap::new();

    // info!("states: {states:?}");

    let Some([first, tail @ ..]) = states.get(..) else {
        panic!()
    };
    for (key, value) in first.values.iter() {
        if tail
            .iter()
            .all(|s| s.values.get(key).unwrap().type_value() == value.type_value())
        {
            intersections.insert(key.clone(), value.clone());
        }
    }
    State {
        values: intersections,
    }
}

#[cfg_attr(test, instrument(level = "TRACE", ret, skip(nodes)))]
pub fn explore(nodes: &[Node]) -> Vec<State> {
    let Some(node) = nodes.first() else {
        return Vec::new()
    }; 

    let mut end_states = Vec::new();

    let mut stack = Vec::new();
    append_nodes(
        &State {
            values: HashMap::new(),
        },
        &node,
        &mut stack,
    );
    // info!("stack: {stack:?}");

    while let Some(current) = stack.pop() {
        match (current.next, current.child) {
            (Some(next), Some(child)) => {
                append_nodes(&current.state, &nodes[next], &mut stack);
                append_nodes(&current.state, &nodes[child], &mut stack);
            }
            (Some(next), None) => append_nodes(&current.state, &nodes[next], &mut stack),
            (None, Some(child)) => append_nodes(&current.state, &nodes[child], &mut stack),
            // If a graph node is reached which has no succeding element, we managed to successfuly evaluate to a leaf of the computational graph, thus we have the full type state of the program.
            (None, None) => end_states.push(current.state),
        }
    }
    end_states
}

#[cfg_attr(test, instrument(level = "TRACE", skip(current, succeeding, stack)))]
fn append_nodes(current: &State, succeeding: &Node, stack: &mut Vec<GraphNode>) {
    let (states, exit) = evaluate_states(current, &succeeding.statement);
    for state in states {
        stack.push(GraphNode {
            state,
            child: if exit { None } else { succeeding.child },
            next: if exit { None } else { succeeding.next },
        });
    }
}

#[cfg_attr(test, instrument(level = "TRACE", skip(start, statement)))]
fn evaluate_states(start: &State, statement: &Statement) -> (Vec<State>, bool) {
    let mut exit = false;
    let states = match statement {
        Statement {
            comptime: false,
            op: Op::Syscall(Syscall::Exit),
            arg
        } => {
            exit = true;
            match arg.get(..) {
                Some([Value::Literal(Literal::Integer(_))]) => vec![start.clone()],
                Some([Value::Variable(Variable { identifier, index: None })]) => {
                    match start.values.get(identifier) {
                        Some(NewValue::Integer(NewValueInteger::I32(_))) => {
                            vec![start.clone()]
                        },
                        None => {
                            let mut state = start.clone();
                            state.values.insert(identifier.clone(), NewValue::Integer(NewValueInteger::I32(MyRange::new(i32::MIN,i32::MAX))));
                            vec![state]
                        }
                        Some(_) => Vec::new(),
                    }
                },
                _ => panic!()
            }
        }
        Statement {
            comptime: false,
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
            comptime: false,
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
            comptime: false,
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
            comptime: false,
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
    };
    (states, exit)
}

#[derive(Debug, Clone)]
struct GraphNode {
    state: State,
    child: Option<usize>,
    next: Option<usize>,
}
#[derive(Debug, Clone, Eq)]
pub struct State {
    values: HashMap<Vec<u8>, NewValue>,
}
impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        let lhs = self
            .values
            .iter()
            .map(|(_key, value)| value.cost())
            .sum::<u64>();
        let rhs = other
            .values
            .iter()
            .map(|(key, value)| value.cost())
            .sum::<u64>();
        lhs.cmp(&rhs)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.values.len() == other.values.len() &&
        self.values.iter().all(|(key,value)|matches!(other.values.get(key), Some(other_value) if value == other_value))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NewValue {
    Integer(NewValueInteger),
}
impl NewValue {
    pub fn type_value(&self) -> Type {
        match self {
            Self::Integer(int) => int.type_value(),
        }
    }
    pub fn cost(&self) -> u64 {
        match self {
            Self::Integer(int) => int.type_index() as u64,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub fn type_value(&self) -> Type {
        match self {
            Self::U8(_) => Type::U8,
            Self::U16(_) => Type::U16,
            Self::U32(_) => Type::U32,
            Self::U64(_) => Type::U64,
            Self::I8(_) => Type::I8,
            Self::I16(_) => Type::I16,
            Self::I32(_) => Type::I32,
            Self::I64(_) => Type::I64,
        }
    }
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
fn possible_integer(x: i128) -> Vec<Type> {
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
        I64_MIN..I32_MIN => vec![Type::I64],
        I32_MIN..I16_MIN => vec![Type::I64, Type::I32],
        I16_MIN..I8_MIN => vec![Type::I64, Type::I32, Type::I16],
        I8_MIN..0 => vec![Type::I64, Type::I32, Type::I16, Type::I8],
        0..U8_MAX => vec![
            Type::I64,
            Type::I32,
            Type::I16,
            Type::I8,
            Type::U64,
            Type::U32,
            Type::U16,
            Type::U8,
        ],
        U8_MAX => vec![
            Type::I64,
            Type::I32,
            Type::I16,
            Type::U64,
            Type::U32,
            Type::U16,
            Type::U8,
        ],
        U16_EDGE..U16_MAX => vec![
            Type::I64,
            Type::I32,
            Type::I16,
            Type::U64,
            Type::U32,
            Type::U16,
        ],
        U16_MAX => vec![Type::I64, Type::I32, Type::U64, Type::U32, Type::U16],
        U32_EDGE..U32_MAX => vec![Type::I64, Type::I32, Type::U64, Type::U32],
        U32_MAX => vec![Type::I64, Type::U64, Type::U32],
        U64_EDGE..U64_MAX => vec![Type::I64, Type::U64],
        U64_MAX => vec![Type::U64],
        _ => panic!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn range_len() {
        assert_eq!(
            MyRange {
                start: 0u8,
                end: 255u8
            }
            .len(),
            255u8
        );
        assert_eq!(
            MyRange {
                start: 1u8,
                end: 0u8
            }
            .len(),
            255u8
        );
        assert_eq!(
            MyRange {
                start: 11u8,
                end: 10u8
            }
            .len(),
            255u8
        );
    }
}
