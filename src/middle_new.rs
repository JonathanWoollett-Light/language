use crate::ast::*;
use num_traits::bounds::Bounded;
use num_traits::identities::One;
use num_traits::identities::Zero;
use std::cmp::Ordering;
use std::collections::HashMap;

use itertools::Itertools;

pub fn optimize(nodes: &[Node]) -> Vec<Node> {
    // Get all possible paths.
    let (possbile_paths, start) = explore_paths(nodes);

    // Get all complete paths.
    let complete_paths = reduce_paths(nodes, &possbile_paths, start);

    // Pick the best path.
    let (best_path, type_state) = pick_best_path(complete_paths);

    // Use the best path to apply optimizations to the nodes.
    optimize_with_path(nodes, &best_path, type_state)
}

#[derive(Debug)]
struct GraphNode {
    node: usize,
    state: TypeValueState,
    /// The options for the next node.
    next: Vec<usize>,
    /// The options for the other next node (e.g. we cannot evaluate an `if` at compile-time so we need to evaluate both paths).
    other: Vec<usize>,
    prev: Option<usize>,
}

// Applies typical optimizations. E.g. removing unused variables, unreachable code, etc.
fn optimize_with_path(
    nodes: &[Node],
    path: &[Option<TypeValueState>],
    type_state: TypeState,
) -> Vec<Node> {
    assert_eq!(nodes.len(), path.len());

    // Remove unreachable nodes
    // ---------------------------------------------------------------------------------------------
    let mut new_nodes = nodes
        .iter()
        .cloned()
        .zip(path.iter().cloned())
        .map(|(n, p)| p.map(|x| (n, x)))
        .collect::<Vec<_>>();

    // TODO This is incomplete and currently only works for simple `next` links.

    // Update node links
    // ---------------------------------------------------------------------------------------------
    {
        let mut first = 0;
        loop {
            match new_nodes.get(first) {
                None => unreachable!(),
                Some(None) => {
                    first += 1;
                }
                Some(Some(_)) => break,
            }
        }
        'outer: loop {
            let mut second = first;
            loop {
                second += 1;
                match new_nodes.get(second) {
                    None => break 'outer,
                    Some(None) => continue,
                    Some(Some(_)) => break,
                }
            }

            let (node, _state) = new_nodes[first].as_mut().unwrap();
            if let Some(next) = &mut node.next {
                *next = second;
            }
            first = second;
        }
    }

    // Remove `None` elements
    // ---------------------------------------------------------------------------------------------
    let flat = {
        let mut decrement = 0;
        for i in (0..new_nodes.len()).rev() {
            let Some((node, _state)) = &mut new_nodes[i] else {
                decrement += 1;
                continue;
            };
            if let Some(next) = &mut node.next {
                *next -= decrement;
            }
            if let Some(child) = &mut node.child {
                *child -= decrement;
            }
            decrement = 0;
        }
        new_nodes
            .into_iter()
            .filter_map(|x| x.map(|(n, _)| n))
            .collect::<Vec<_>>()
    };

    // Prepend variable definitions
    // ---------------------------------------------------------------------------------------------
    let n = type_state.len();
    let definition = (1..)
        .zip(type_state.into_iter())
        .map(|(i, (identifier, type_value))| Node {
            statement: Statement {
                comptime: false,
                op: Op::Special(Special::Type),
                arg: vec![
                    Value::Variable(Variable {
                        identifier,
                        index: None,
                    }),
                    Value::Type(type_value),
                ],
            },
            child: None,
            next: Some(i),
        });
    let original = flat.into_iter().map(|mut node| {
        if let Some(next) = &mut node.next {
            *next += n;
        }
        if let Some(child) = &mut node.child {
            *child += n;
        }
        node
    });

    definition.chain(original).collect()
}

fn pick_best_path(
    paths: Vec<Vec<Option<TypeValueState>>>,
) -> (Vec<Option<TypeValueState>>, TypeState) {
    // TODO This is a bad heuristic to pick the best path, this should be improved.

    let (mut min_len, mut min_path) = (paths[0].iter().filter(|x| x.is_some()).count(), 0);
    for (i, path) in paths.iter().enumerate().skip(1) {
        let len = path.iter().filter(|x| x.is_some()).count();
        if len < min_len {
            min_len = len;
            min_path = i;
        }
    }
    let type_state =
        paths[min_path]
            .iter()
            .filter_map(|x| x.as_ref())
            .fold(TypeState::new(), |mut acc, x| {
                for (ident, value_type) in x.iter() {
                    acc.insert(ident.clone(), value_type.type_value());
                }
                acc
            });

    (paths[min_path].clone(), type_state)
}

fn reduce_paths(
    nodes: &[Node],
    graph_nodes: &[GraphNode],
    start: usize,
) -> Vec<Vec<Option<TypeValueState>>> {
    #[derive(Debug, Clone)]
    struct Trace {
        /// The stack of `GraphNode` that need to be visited to complete the trace, these are
        /// indices into `graph_nodes`.
        stack: Vec<usize>,
        /// The type states for every `node` where `path[i]` corresponds to `nodes[i]`, this is
        /// filled as the trace pops values from `self.stack`.
        path: Vec<Option<TypeValueState>>,
    }

    // The stack of incomplete traces that need to be completed.
    let mut stack = (0..start)
        .map(|i| Trace {
            stack: vec![i],
            path: vec![None; nodes.len()],
        })
        .collect::<Vec<_>>();

    // The set of complete paths. A complete path is a set of `TypeValueState` for every `Node` that
    // could be encountered given the initial `TypeValueState`.
    let mut paths = Vec::new();

    while let Some(mut trace) = stack.pop() {
        let front = trace.stack.pop().unwrap();
        let graph = &graph_nodes[front];
        let n = graph.node;
        let node = &nodes[graph.node];

        // If this node had been previously evaluated it would have a pre-existing state. Here
        // it should not have been previously evaluated thus should not have a pre-existing state.
        assert!(trace.path[n].is_none());

        // Add next nodes that need to be evaluated to the trace stack.
        match node.statement.op {
            Op::Syscall(Syscall::Exit) => {
                assert!(graph.next.is_empty());
                assert!(graph.other.is_empty());

                // Set the state at this node.
                trace.path[n] = Some(graph.state.clone());

                if trace.stack.is_empty() {
                    paths.push(trace.path);
                }
            }
            Op::Intrinsic(Intrinsic::If(_)) => {
                if graph.next.is_empty() && graph.other.is_empty() {
                    continue;
                }
                match (graph.next.is_empty(), graph.other.is_empty()) {
                    // This condition occurs when a `require` fails.
                    (true, true) => continue,
                    (false, false) => {
                        // Set the state at this node.
                        trace.path[n] = Some(graph.state.clone());

                        for (a, b) in graph.next.iter().cartesian_product(graph.other.iter()) {
                            let mut temp = trace.clone();
                            temp.stack.push(*a);
                            temp.stack.push(*b);
                            stack.push(temp);
                        }
                    }
                    (false, true) => {
                        // This statement can be omitted so we don't set the state.

                        for a in graph.next.iter() {
                            let mut temp = trace.clone();
                            temp.stack.push(*a);
                            stack.push(temp);
                        }
                    }
                    (true, false) => {
                        // This statement can be omitted so we don't set the state.

                        for b in graph.other.iter() {
                            let mut temp = trace.clone();
                            temp.stack.push(*b);
                            stack.push(temp);
                        }
                    }
                }
            }
            Op::Special(Special::Require(_)) => {
                // This condition occurs when a `require` fails.
                if graph.next.is_empty() {
                    continue;
                }

                // This statement can be omitted so we don't set the state.

                assert!(graph.other.is_empty());
                for state in graph.next.iter() {
                    let mut temp = trace.clone();
                    temp.stack.push(*state);
                    stack.push(temp);
                }
            }
            _ => {
                // This condition occurs when a `require` fails.
                if graph.next.is_empty() {
                    continue;
                }

                // Set the state at this node.
                trace.path[n] = Some(graph.state.clone());

                assert!(graph.other.is_empty());
                for state in graph.next.iter() {
                    let mut temp = trace.clone();
                    temp.stack.push(*state);
                    stack.push(temp);
                }
            }
        }
    }

    paths
}

fn explore_paths(nodes: &[Node]) -> (Vec<GraphNode>, usize) {
    let initial_possible_states = get_possible_states(&nodes[0], &TypeValueState::new());
    let number_of_initial_states = initial_possible_states.len();

    let (mut graph_nodes, mut indices) = initial_possible_states
        .into_iter()
        .enumerate()
        .map(|(i, state)| {
            (
                GraphNode {
                    node: 0,
                    state,
                    next: Vec::new(),
                    other: Vec::new(),
                    prev: None,
                },
                i,
            )
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    while let Some(index) = indices.pop() {
        let n = graph_nodes[index].node;

        let mut append =
            |node_index: usize, graph_index: usize, graph_nodes: &mut Vec<GraphNode>| {
                get_possible_states(&nodes[node_index], &graph_nodes[graph_index].state)
                    .into_iter()
                    .map(|state| {
                        let j = graph_nodes.len();
                        indices.push(j);
                        graph_nodes.push(GraphNode {
                            node: node_index,
                            state,
                            next: Vec::new(),
                            other: Vec::new(),
                            prev: Some(graph_index),
                        });
                        j
                    })
                    .collect()
            };

        // 1. Relies on the ordering of `nodes` (it will be `child -> next -> outer`).
        // 2. Only `exit`s are allowed to have no following nodes, otherwise it is an error.
        match nodes[n].statement.op {
            Op::Intrinsic(Intrinsic::If(Cmp::Lt)) => {
                match nodes[n].statement.arg.as_slice() {
                    [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(x))] =>
                    {
                        let y = graph_nodes[index]
                            .state
                            .get(identifier)
                            .unwrap()
                            .integer()
                            .unwrap();

                        // If we know the if will be true at compile-time
                        if y.max() < *x {
                            graph_nodes[index].next = append(n + 1, index, &mut graph_nodes);
                        // See 1 & 2
                        }
                        // If we know the if will be false at compile-time
                        else if y.min() >= *x {
                            graph_nodes[index].next = if let Some(next) = nodes[n].next {
                                append(next, index, &mut graph_nodes)
                            } else {
                                // See 2
                                append(n + 1, index, &mut graph_nodes)
                            };
                        } else {
                            if let Some(child) = nodes[n].child {
                                graph_nodes[index].next = append(child, index, &mut graph_nodes);
                            }
                            graph_nodes[index].other = if let Some(next) = nodes[n].next {
                                append(next, index, &mut graph_nodes)
                            } else {
                                // See 2
                                append(n + 1, index, &mut graph_nodes)
                            };
                        }
                    }
                    _ => todo!(),
                }
            }
            // See 2
            Op::Syscall(Syscall::Exit) => continue,
            // See 1 & 2
            _ => {
                graph_nodes[index].next = append(n + 1, index, &mut graph_nodes);
            }
        }
    }

    (graph_nodes, number_of_initial_states)
}

/// Given an incoming state (`state`) and a node, outputs the possible outgoing states.
pub fn get_possible_states(node: &Node, state: &TypeValueState) -> Vec<TypeValueState> {
    let statement = &node.statement;
    match statement.op {
        Op::Syscall(Syscall::Exit) => match statement.arg.as_slice() {
            [Value::Literal(Literal::Integer(_))] => vec![state.clone()],
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::Assign) => {
            match statement.arg.as_slice() {
                [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(x))] =>
                {
                    match state.get(identifier) {
                        // Iterates over the set of integer types which could contain `x`, returning a new state for each possibility.
                        None => TypeValueInteger::possible(*x)
                            .into_iter()
                            .map(|p| {
                                let mut new_state = state.clone();
                                new_state.insert(identifier.clone(), TypeValue::Integer(p));
                                new_state
                            })
                            .collect(),
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            }
        }
        Op::Intrinsic(Intrinsic::SubAssign) => match statement.arg.as_slice() {
            [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(x))] => {
                match state.get(identifier) {
                    Some(TypeValue::Integer(y)) => match y.overflowing_sub(*x) {
                        Ok(z) => {
                            let mut new_state = state.clone();
                            *new_state
                                .get_mut(identifier)
                                .unwrap()
                                .integer_mut()
                                .unwrap() = z;
                            vec![new_state]
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::If(Cmp::Lt)) => match statement.arg.as_slice() {
            [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(_))] => {
                match state.get(identifier) {
                    Some(TypeValue::Integer(_)) => vec![state.clone()],
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Special(Special::Require(Cmp::Ge)) => match statement.arg.as_slice() {
            [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(x))] => {
                match state.get(identifier) {
                    Some(TypeValue::Integer(y)) => match y.greater_than_or_equal(*x) {
                        true => {
                            let mut new_state = state.clone();
                            let integer = new_state
                                .get_mut(identifier)
                                .unwrap()
                                .integer_mut()
                                .unwrap();
                            integer.set_min(*x).unwrap();
                            vec![new_state]
                        }
                        false => Vec::new(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Special(Special::Require(Cmp::Le)) => match statement.arg.as_slice() {
            [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(x))] => {
                match state.get(identifier) {
                    Some(TypeValue::Integer(y)) => match y.less_than_or_equal(*x) {
                        true => {
                            let mut new_state = state.clone();
                            let integer = new_state
                                .get_mut(identifier)
                                .unwrap()
                                .integer_mut()
                                .unwrap();
                            integer.set_max(*x).unwrap();
                            vec![new_state]
                        }
                        false => Vec::new(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        _ => todo!(),
    }
}

#[derive(Debug, Clone)]
pub struct TypeValueState(HashMap<Identifier, TypeValue>);
impl TypeValueState {
    fn new() -> Self {
        Self(HashMap::new())
    }
    fn iter(&self) -> std::collections::hash_map::Iter<'_, Identifier, TypeValue> {
        self.0.iter()
    }
    fn into_iter(self) -> std::collections::hash_map::IntoIter<Identifier, TypeValue> {
        self.0.into_iter()
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn get(&self, key: &Identifier) -> Option<&TypeValue> {
        self.0.get(key)
    }
    fn get_mut(&mut self, key: &Identifier) -> Option<&mut TypeValue> {
        self.0.get_mut(key)
    }
    fn insert(&mut self, key: Identifier, value: TypeValue) -> Option<TypeValue> {
        self.0.insert(key, value)
    }
}

#[derive(Debug, Clone)]
struct TypeState(HashMap<Identifier, Type>);
impl TypeState {
    fn new() -> Self {
        Self(HashMap::new())
    }
    fn iter(&self) -> std::collections::hash_map::Iter<'_, Identifier, Type> {
        self.0.iter()
    }
    fn into_iter(self) -> std::collections::hash_map::IntoIter<Identifier, Type> {
        self.0.into_iter()
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn get(&self, key: &Identifier) -> Option<&Type> {
        self.0.get(key)
    }
    fn get_mut(&mut self, key: &Identifier) -> Option<&mut Type> {
        self.0.get_mut(key)
    }
    fn insert(&mut self, key: Identifier, value: Type) -> Option<Type> {
        self.0.insert(key, value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeValue {
    Integer(TypeValueInteger),
}
#[allow(unreachable_patterns)]
impl TypeValue {
    fn integer_mut(&mut self) -> Option<&mut TypeValueInteger> {
        match self {
            Self::Integer(x) => Some(x),
            _ => None,
        }
    }
    pub fn type_value(&self) -> Type {
        match self {
            Self::Integer(int) => int.type_value(),
        }
    }
    fn integer(&self) -> Option<&TypeValueInteger> {
        match self {
            Self::Integer(integer) => Some(integer),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeValueInteger {
    U8(MyRange<u8>),
    U16(MyRange<u16>),
    U32(MyRange<u32>),
    U64(MyRange<u64>),
    I8(MyRange<i8>),
    I16(MyRange<i16>),
    I32(MyRange<i32>),
    I64(MyRange<i64>),
}
impl TypeValueInteger {
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
    fn max(&self) -> i128 {
        match self {
            Self::U8(range) => range.max() as i128,
            Self::U16(range) => range.max() as i128,
            Self::U32(range) => range.max() as i128,
            Self::U64(range) => range.max() as i128,
            Self::I8(range) => range.max() as i128,
            Self::I16(range) => range.max() as i128,
            Self::I32(range) => range.max() as i128,
            Self::I64(range) => range.max() as i128,
        }
    }
    fn min(&self) -> i128 {
        match self {
            Self::U8(range) => range.min() as i128,
            Self::U16(range) => range.min() as i128,
            Self::U32(range) => range.min() as i128,
            Self::U64(range) => range.min() as i128,
            Self::I8(range) => range.min() as i128,
            Self::I16(range) => range.min() as i128,
            Self::I32(range) => range.min() as i128,
            Self::I64(range) => range.min() as i128,
        }
    }
    fn set_min(&mut self, rhs: i128) -> Result<(), ()> {
        match self {
            Self::U8(range) => match (rhs < u8::MIN as i128, rhs > u8::MAX as i128) {
                (false, true) => Err(()),
                (true, false) => Ok(()),
                (false, false) => {
                    range.set_min(rhs as u8);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::U16(range) => match (rhs < u16::MIN as i128, rhs > u16::MAX as i128) {
                (false, true) => Err(()),
                (true, false) => Ok(()),
                (false, false) => {
                    range.set_min(rhs as u16);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::U32(range) => match (rhs < u32::MIN as i128, rhs > u32::MAX as i128) {
                (false, true) => Err(()),
                (true, false) => Ok(()),
                (false, false) => {
                    range.set_min(rhs as u32);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::U64(range) => match (rhs < u64::MIN as i128, rhs > u64::MAX as i128) {
                (false, true) => Err(()),
                (true, false) => Ok(()),
                (false, false) => {
                    range.set_min(rhs as u64);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::I8(range) => match (rhs < i8::MIN as i128, rhs > i8::MAX as i128) {
                (false, true) => Err(()),
                (true, false) => Ok(()),
                (false, false) => {
                    range.set_min(rhs as i8);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::I16(range) => match (rhs < i16::MIN as i128, rhs > i16::MAX as i128) {
                (false, true) => Err(()),
                (true, false) => Ok(()),
                (false, false) => {
                    range.set_min(rhs as i16);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::I32(range) => match (rhs < i32::MIN as i128, rhs > i32::MAX as i128) {
                (false, true) => Err(()),
                (true, false) => Ok(()),
                (false, false) => {
                    range.set_min(rhs as i32);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::I64(range) => match (rhs < i64::MIN as i128, rhs > i64::MAX as i128) {
                (false, true) => Err(()),
                (true, false) => Ok(()),
                (false, false) => {
                    range.set_min(rhs as i64);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
        }
    }
    fn set_max(&mut self, rhs: i128) -> Result<(), ()> {
        match self {
            Self::U8(range) => match (rhs < u8::MIN as i128, rhs > u8::MAX as i128) {
                (false, true) => Ok(()),
                (true, false) => Err(()),
                (false, false) => {
                    range.set_max(rhs as u8);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::U16(range) => match (rhs < u16::MIN as i128, rhs > u16::MAX as i128) {
                (false, true) => Ok(()),
                (true, false) => Err(()),
                (false, false) => {
                    range.set_max(rhs as u16);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::U32(range) => match (rhs < u32::MIN as i128, rhs > u32::MAX as i128) {
                (false, true) => Ok(()),
                (true, false) => Err(()),
                (false, false) => {
                    range.set_max(rhs as u32);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::U64(range) => match (rhs < u64::MIN as i128, rhs > u64::MAX as i128) {
                (false, true) => Ok(()),
                (true, false) => Err(()),
                (false, false) => {
                    range.set_max(rhs as u64);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::I8(range) => match (rhs < i8::MIN as i128, rhs > i8::MAX as i128) {
                (false, true) => Ok(()),
                (true, false) => Err(()),
                (false, false) => {
                    range.set_max(rhs as i8);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::I16(range) => match (rhs < i16::MIN as i128, rhs > i16::MAX as i128) {
                (false, true) => Ok(()),
                (true, false) => Err(()),
                (false, false) => {
                    range.set_max(rhs as i16);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::I32(range) => match (rhs < i32::MIN as i128, rhs > i32::MAX as i128) {
                (false, true) => Ok(()),
                (true, false) => Err(()),
                (false, false) => {
                    range.set_max(rhs as i32);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
            Self::I64(range) => match (rhs < i64::MIN as i128, rhs > i64::MAX as i128) {
                (false, true) => Ok(()),
                (true, false) => Err(()),
                (false, false) => {
                    range.set_max(rhs as i64);
                    Ok(())
                }
                (true, true) => unreachable!(),
            },
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
    fn contains(&self, rhs: i128) -> bool {
        match self {
            Self::U8(range) => match u8::try_from(rhs) {
                Err(_) => false,
                Ok(x) => range.contains(x),
            },
            Self::U16(range) => match u16::try_from(rhs) {
                Err(_) => false,
                Ok(x) => range.contains(x),
            },
            Self::U32(range) => match u32::try_from(rhs) {
                Err(_) => false,
                Ok(x) => range.contains(x),
            },
            Self::U64(range) => match u64::try_from(rhs) {
                Err(_) => false,
                Ok(x) => range.contains(x),
            },
            Self::I8(range) => match i8::try_from(rhs) {
                Err(_) => false,
                Ok(x) => range.contains(x),
            },
            Self::I16(range) => match i16::try_from(rhs) {
                Err(_) => false,
                Ok(x) => range.contains(x),
            },
            Self::I32(range) => match i32::try_from(rhs) {
                Err(_) => false,
                Ok(x) => range.contains(x),
            },
            Self::I64(range) => match i64::try_from(rhs) {
                Err(_) => false,
                Ok(x) => range.contains(x),
            },
        }
    }
    fn less_than_or_equal(&self, rhs: i128) -> bool {
        match self {
            Self::U8(range) => match (rhs < u8::MIN as i128, rhs > u8::MAX as i128) {
                (false, true) => false,
                (true, false) => true,
                (false, false) => range.less_than_or_equal(rhs as u8),
                (true, true) => unreachable!(),
            },
            Self::U16(range) => match (rhs < u16::MIN as i128, rhs > u16::MAX as i128) {
                (false, true) => false,
                (true, false) => true,
                (false, false) => range.less_than_or_equal(rhs as u16),
                (true, true) => unreachable!(),
            },
            Self::U32(range) => match (rhs < u32::MIN as i128, rhs > u32::MAX as i128) {
                (false, true) => false,
                (true, false) => true,
                (false, false) => range.less_than_or_equal(rhs as u32),
                (true, true) => unreachable!(),
            },
            Self::U64(range) => match (rhs < u64::MIN as i128, rhs > u64::MAX as i128) {
                (false, true) => false,
                (true, false) => true,
                (false, false) => range.less_than_or_equal(rhs as u64),
                (true, true) => unreachable!(),
            },
            Self::I8(range) => match (rhs < i8::MIN as i128, rhs > i8::MAX as i128) {
                (false, true) => false,
                (true, false) => true,
                (false, false) => range.less_than_or_equal(rhs as i8),
                (true, true) => unreachable!(),
            },
            Self::I16(range) => match (rhs < i16::MIN as i128, rhs > i16::MAX as i128) {
                (false, true) => false,
                (true, false) => true,
                (false, false) => range.less_than_or_equal(rhs as i16),
                (true, true) => unreachable!(),
            },
            Self::I32(range) => match (rhs < i32::MIN as i128, rhs > i32::MAX as i128) {
                (false, true) => false,
                (true, false) => true,
                (false, false) => range.less_than_or_equal(rhs as i32),
                (true, true) => unreachable!(),
            },
            Self::I64(range) => match (rhs < i64::MIN as i128, rhs > i64::MAX as i128) {
                (false, true) => false,
                (true, false) => true,
                (false, false) => range.less_than_or_equal(rhs as i64),
                (true, true) => unreachable!(),
            },
        }
    }
    fn greater_than_or_equal(&self, rhs: i128) -> bool {
        match self {
            Self::U8(range) => match (rhs < u8::MIN as i128, rhs > u8::MAX as i128) {
                (false, true) => true,
                (true, false) => false,
                (false, false) => range.greater_than_or_equal(rhs as u8),
                (true, true) => unreachable!(),
            },
            Self::U16(range) => match (rhs < u16::MIN as i128, rhs > u16::MAX as i128) {
                (false, true) => true,
                (true, false) => false,
                (false, false) => range.greater_than_or_equal(rhs as u16),
                (true, true) => unreachable!(),
            },
            Self::U32(range) => match (rhs < u32::MIN as i128, rhs > u32::MAX as i128) {
                (false, true) => true,
                (true, false) => false,
                (false, false) => range.greater_than_or_equal(rhs as u32),
                (true, true) => unreachable!(),
            },
            Self::U64(range) => match (rhs < u64::MIN as i128, rhs > u64::MAX as i128) {
                (false, true) => true,
                (true, false) => false,
                (false, false) => range.greater_than_or_equal(rhs as u64),
                (true, true) => unreachable!(),
            },
            Self::I8(range) => match (rhs < i8::MIN as i128, rhs > i8::MAX as i128) {
                (false, true) => true,
                (true, false) => false,
                (false, false) => range.greater_than_or_equal(rhs as i8),
                (true, true) => unreachable!(),
            },
            Self::I16(range) => match (rhs < i16::MIN as i128, rhs > i16::MAX as i128) {
                (false, true) => true,
                (true, false) => false,
                (false, false) => range.greater_than_or_equal(rhs as i16),
                (true, true) => unreachable!(),
            },
            Self::I32(range) => match (rhs < i32::MIN as i128, rhs > i32::MAX as i128) {
                (false, true) => true,
                (true, false) => false,
                (false, false) => range.greater_than_or_equal(rhs as i32),
                (true, true) => unreachable!(),
            },
            Self::I64(range) => match (rhs < i64::MIN as i128, rhs > i64::MAX as i128) {
                (false, true) => true,
                (true, false) => false,
                (false, false) => range.greater_than_or_equal(rhs as i64),
                (true, true) => unreachable!(),
            },
        }
    }
    fn overflowing_sub(&self, rhs: i128) -> Result<Self, ()> {
        match self {
            Self::U8(range) => {
                let rhs = u8::try_from(rhs).map_err(drop)?;
                Ok(Self::U8(MyRange::new(
                    range.start.overflowing_sub(rhs).0,
                    range.end.overflowing_sub(rhs).0,
                )))
            }
            Self::U16(range) => {
                let rhs = u16::try_from(rhs).map_err(drop)?;
                Ok(Self::U16(MyRange::new(
                    range.start.overflowing_sub(rhs).0,
                    range.end.overflowing_sub(rhs).0,
                )))
            }
            Self::U32(range) => {
                let rhs = u32::try_from(rhs).map_err(drop)?;
                Ok(Self::U32(MyRange::new(
                    range.start.overflowing_sub(rhs).0,
                    range.end.overflowing_sub(rhs).0,
                )))
            }
            Self::U64(range) => {
                let rhs = u64::try_from(rhs).map_err(drop)?;
                Ok(Self::U64(MyRange::new(
                    range.start.overflowing_sub(rhs).0,
                    range.end.overflowing_sub(rhs).0,
                )))
            }
            Self::I8(range) => {
                let rhs = i8::try_from(rhs).map_err(drop)?;
                Ok(Self::I8(MyRange::new(
                    range.start.overflowing_sub(rhs).0,
                    range.end.overflowing_sub(rhs).0,
                )))
            }
            Self::I16(range) => {
                let rhs = i16::try_from(rhs).map_err(drop)?;
                Ok(Self::I16(MyRange::new(
                    range.start.overflowing_sub(rhs).0,
                    range.end.overflowing_sub(rhs).0,
                )))
            }
            Self::I32(range) => {
                let rhs = i32::try_from(rhs).map_err(drop)?;
                Ok(Self::I32(MyRange::new(
                    range.start.overflowing_sub(rhs).0,
                    range.end.overflowing_sub(rhs).0,
                )))
            }
            Self::I64(range) => {
                let rhs = i64::try_from(rhs).map_err(drop)?;
                Ok(Self::I64(MyRange::new(
                    range.start.overflowing_sub(rhs).0,
                    range.end.overflowing_sub(rhs).0,
                )))
            }
        }
    }
    #[cfg(not(feature = "16"))]
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

    // A 16bit feature that reduces the set of types to `u8`, `i8` `u16` and `i16` to make debugging easier.
    #[cfg(feature = "16")]
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
            I16_MIN..I8_MIN => vec![Self::I16(MyRange::new(x as i16, x as i16))],
            I8_MIN..0 => vec![
                Self::I16(MyRange::new(x as i16, x as i16)),
                Self::I8(MyRange::new(x as i8, x as i8)),
            ],
            0..U8_MAX => vec![
                Self::I16(MyRange::new(x as i16, x as i16)),
                Self::I8(MyRange::new(x as i8, x as i8)),
                Self::U16(MyRange::new(x as u16, x as u16)),
                Self::U8(MyRange::new(x as u8, x as u8)),
            ],
            U8_MAX => vec![
                Self::I16(MyRange::new(x as i16, x as i16)),
                Self::U16(MyRange::new(x as u16, x as u16)),
                Self::U8(MyRange::new(x as u8, x as u8)),
            ],
            U16_EDGE..U16_MAX => vec![
                Self::I16(MyRange::new(x as i16, x as i16)),
                Self::U16(MyRange::new(x as u16, x as u16)),
            ],
            U16_MAX => vec![Self::U16(MyRange::new(x as u16, x as u16))],
            _ => panic!(),
        }
    }
    fn value(&self) -> Option<i128> {
        match self {
            Self::U8(x) => x.value().map(i128::from),
            Self::U16(x) => x.value().map(i128::from),
            Self::U32(x) => x.value().map(i128::from),
            Self::U64(x) => x.value().map(i128::from),
            Self::I8(x) => x.value().map(i128::from),
            Self::I16(x) => x.value().map(i128::from),
            Self::I32(x) => x.value().map(i128::from),
            Self::I64(x) => x.value().map(i128::from),
        }
    }
}
#[cfg(not(feature = "16"))]
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

// A 16bit feature that reduces the set of types to `u8`, `i8` `u16` and `i16` to make debugging easier.
#[cfg(feature = "16")]
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
        I16_MIN..I8_MIN => vec![Type::I16],
        I8_MIN..0 => vec![Type::I16, Type::I8],
        0..U8_MAX => vec![Type::I16, Type::I8, Type::U16, Type::U8],
        U8_MAX => vec![Type::I16, Type::U16, Type::U8],
        U16_EDGE..U16_MAX => vec![Type::I16, Type::U16],
        U16_MAX => vec![Type::U16],
        _ => panic!(),
    }
}

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
    fn greater_than_or_equal(&self, x: T) -> bool {
        self.min() >= x
    }
    fn less_than_or_equal(&self, x: T) -> bool {
        self.max() <= x
    }
    fn contains(&self, x: T) -> bool {
        self.min() <= x && self.max() >= x
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
    fn set_max(&mut self, x: T) {
        match self.start.cmp(&self.end) {
            Ordering::Greater => todo!(),
            Ordering::Equal => match x.cmp(&self.start) {
                Ordering::Greater => {}
                Ordering::Equal => {}
                Ordering::Less => todo!(),
            },
            Ordering::Less => todo!(),
        }
    }
    fn set_min(&mut self, x: T) {
        match self.start.cmp(&self.end) {
            Ordering::Greater => todo!(),
            Ordering::Equal => match x.cmp(&self.start) {
                Ordering::Greater => todo!(),
                Ordering::Equal => {}
                Ordering::Less => {}
            },
            Ordering::Less => todo!(),
        }
    }
    fn value(&self) -> Option<T> {
        if self.start == self.end {
            Some(self.start)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn abca_example() {
        let nodes = [
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                child: None,
                next: Some(1),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::SubAssign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                child: None,
                next: Some(2),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::If(Cmp::Lt)),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                child: Some(3),
                next: Some(5),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Special(Special::Require(Cmp::Ge)),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(-128)),
                    ],
                },
                child: None,
                next: Some(4),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
                child: None,
                next: None,
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Special(Special::Require(Cmp::Le)),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(255)),
                    ],
                },
                child: None,
                next: Some(6),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                child: None,
                next: None,
            },
        ];
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![Value::Variable(Variable::new("x")), Value::Type(Type::U8)]
                    },
                    child: None,
                    next: Some(1)
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(0))
                        ]
                    },
                    child: None,
                    next: Some(2)
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::SubAssign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(3)
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None
                }
            ]
        );
    }
}
