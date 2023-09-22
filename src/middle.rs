use crate::ast::*;
use num_traits::bounds::Bounded;
use num_traits::identities::One;
use num_traits::identities::Zero;
use std::cmp::Ordering;
use std::collections::HashMap;

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
pub fn update(nodes: &[Node]) {
    let states = explore(nodes);
    let shared_state = state_intersection(&states);
}

/// Given a set of states return a state that represents the intersections of all states in the set.
pub fn state_intersection(states: &[State]) -> State {
    let mut intersections = HashMap::new();

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

pub fn explore(nodes: &[Node]) -> Vec<State> {
    assert!(!nodes.is_empty());
    let node = &nodes[0];
    let mut end_states = Vec::new();
    // While there is are no branches we can safetly evaluated syscalls at compile time (e.g. read),
    // when we have multiple branches and ways they might be evaluated we can no longer evaluate
    // them.
    // While this is `true` syscalls can be evalauted. It is set `false` when `evaluate_states`
    // would return multiple states.
    let mut single = true;

    let initial_states = evaluate_states(
        &State {
            values: HashMap::new(),
        },
        &node.statement,
        single
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

fn append_nodes(current: &State, succeeding: &Node, stack: &mut Vec<GraphNode>, single: &mut bool) {
    for state in evaluate_states(current, &succeeding.statement, single) {
        stack.push(GraphNode {
            state,
            child: succeeding.child,
            next: succeeding.next,
        });
    }
}

fn evaluate_states(start: &State, statement: &Statement, single: &mut bool) -> Vec<State> {
    match statement {
        Statement {
            runtime: false,
            op: Op::Intrinsic(Intrinsic::Assign),
            arg
        } if let Some([Value::Variable(Variable { identifier, index: None }), Value::Literal(Literal::Integer(x))]) = arg.get(..) => {
            let possible = NewValueInteger::possible(*x);
            
            if possible.len() > 1 {
                single = false;
            }

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
