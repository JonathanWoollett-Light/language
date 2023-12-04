use crate::ast::*;
use num_traits::bounds::Bounded;
use num_traits::identities::One;
use num_traits::identities::Zero;
use std::alloc;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ptr;
use std::ptr::NonNull;

const DEFAULT_LOOP_LIMIT: usize = 100;
#[allow(dead_code)]
const UNROLL_LIMIT: usize = 4096;

pub unsafe fn build_optimized_tree(
    graph: NonNull<NewStateNode>,
) -> (NonNull<NewNode>, HashSet<Identifier>) {
    // unsafe {
    //     println!("------------checking optimization input------------");
    //     let mut stack = vec![graph];
    //     while let Some(current) = stack.pop() {
    //         println!("current: {:?}",current);
    //         println!("statement: {:?}",current.as_ref().statement);
    //         match current.as_ref().statement.as_ref().statement.arg.as_slice() {
    //             [Value::Variable(Variable { identifier, index: None }), Value::Literal(Literal::Integer(_))] => {
    //                 println!("identifier.as_ptr(): {:?}",identifier.as_ptr());
    //                 println!("identifier.capacity(): {:?}",identifier.capacity());
    //                 println!("identifier.len(): {:?}",identifier.len());
    //             }
    //             _ => {}
    //         }

    //         if let Some(one) = current.as_ref().next.0 {
    //             stack.push(one);
    //         }
    //         if let Some(two) = current.as_ref().next.1 {
    //             stack.push(two);
    //         }
    //     }
    //     println!("----------------------------------------------------");
    // }

    let mut types = HashMap::new();
    let mut read = HashSet::new();

    // The 1st step is iterating over nodes which don't diverge
    // (`next.0.is_none() || next.1.is_none()`), all these can be simply inlined.
    let mut stack = vec![graph];
    let mut first_node = graph.as_ref().statement;
    while let Some(mut current) = stack.pop() {
        // println!("current: {current:?}");
        // println!("statement: {:?}",current.as_ref().statement);

        let next_state_node_opt = match current.as_ref().next {
            (Some(x), None) | (None, Some(x)) => {
                stack.push(x);
                Some(x)
            }
            (Some(_), Some(_)) => {
                // For now we stop when hitting a diverging path.
                current.as_mut().next = (None, None); // This memory leaks the nodes.
                current.as_mut().statement.as_mut().next = None; // This memory leaks the nodes.
                current.as_mut().statement.as_mut().child = None; // This memory leaks the nodes.
                break;
            }
            (None, None) => {
                current.as_mut().statement.as_mut().next = None; // This memory leaks the nodes.
                current.as_mut().statement.as_mut().child = None; // This memory leaks the nodes.
                None
            }
        };

        // dbg!(&current.as_ref().statement.as_ref().statement.op);

        println!("op: {:?}", current.as_ref().statement.as_ref().statement.op);

        match current.as_ref().statement.as_ref().statement.op {
            Op::Intrinsic(Intrinsic::Assign) => {
                match current.as_ref().statement.as_ref().statement.arg.as_slice() {
                    [Value::Variable(Variable {
                        identifier,
                        index: None,
                    }), Value::Literal(Literal::Integer(_))] => {
                        let variable_state =
                            current.as_ref().state.get(identifier).unwrap().clone();
                        let variable_type = Type::from(variable_state);
                        let existing = types.insert(identifier, variable_type.clone());

                        // If not already declared this declares the type of the variable.
                        if existing.is_none() {
                            current.as_mut().statement.as_mut().statement.op =
                                Op::Special(Special::Type);
                            current
                                .as_mut()
                                .statement
                                .as_mut()
                                .statement
                                .arg
                                .insert(1, Value::Type(variable_type));
                        }
                    }
                    _ => todo!(),
                }
            }
            Op::Syscall(Syscall::Exit) => {
                match current.as_ref().statement.as_ref().statement.arg.as_slice() {
                    [Value::Variable(Variable {
                        identifier,
                        index: None,
                    })] => {
                        let varible_state = current.as_ref().state.get(identifier).unwrap();
                        match varible_state {
                            TypeValue::Integer(TypeValueInteger::U8(range)) => {
                                if let Some(exact) = range.value() {
                                    current.as_mut().statement.as_mut().statement.arg =
                                        vec![Value::Literal(Literal::Integer(i128::from(exact)))];
                                } else {
                                    read.insert(identifier.clone());
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    [Value::Literal(Literal::Integer(_))] => {}
                    _ => todo!(),
                }
            }
            // On the linear path, these statements can simply be removed.
            //
            // `current` is not used after `current = prev;` but it may be in the future, and I
            // don't want to obfuscate this complex logic further.
            Op::Intrinsic(Intrinsic::AddAssign) => {
                match current.as_ref().statement.as_ref().statement.arg.as_slice() {
                    [Value::Variable(Variable {
                        identifier,
                        index: None,
                    }), Value::Literal(Literal::Integer(_))] => {
                        let varible_state = current.as_ref().state.get(identifier).unwrap();
                        match varible_state {
                            TypeValue::Integer(TypeValueInteger::U8(range))
                                if let Some(exact) = range.value() =>
                            {
                                // We unwrap here since it would be an error for an add assign to be the last node.
                                let mut next_state_node = next_state_node_opt.unwrap();

                                // Update syntax node
                                {
                                    match current.as_ref().statement.as_ref().preceding {
                                        Some(Preceding::Parent(mut parent)) => {
                                            debug_assert_eq!(
                                                current.as_ref().statement.as_ref().child,
                                                None
                                            );
                                            parent.as_mut().child =
                                                current.as_ref().statement.as_ref().next;
                                        }
                                        Some(Preceding::Previous(mut previous)) => {
                                            debug_assert_eq!(
                                                current.as_ref().statement.as_ref().child,
                                                None
                                            );
                                            previous.as_mut().next =
                                                current.as_ref().statement.as_ref().next;
                                        }
                                        None => {
                                            debug_assert_eq!(
                                                current.as_ref().statement,
                                                first_node
                                            );
                                            // We unwrap here since if there is no next node, this
                                            // is both the 1st node and last node, thus removing it is an error.
                                            first_node = next_state_node.as_ref().statement;
                                        }
                                    }
                                    alloc::dealloc(
                                        current.as_ref().statement.as_ptr().cast(),
                                        alloc::Layout::new::<NewNode>(),
                                    );
                                }

                                // Update state node
                                {
                                    next_state_node.as_mut().prev = current.as_ref().prev;
                                    current.as_ref().prev.unwrap().as_mut().next =
                                        (Some(next_state_node), None);
                                    alloc::dealloc(
                                        current.as_ptr().cast(),
                                        alloc::Layout::new::<NewStateNode>(),
                                    );
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
            // `current` is not used after `current = prev;` but it may be in the future, and I
            // don't want to obfuscate this complex logic further.
            Op::Intrinsic(Intrinsic::If(Cmp::Eq)) => {
                // We unwrap here since it would be an error for an if to be the last node.
                let mut next_state_node = next_state_node_opt.unwrap();

                // Update syntax node
                {
                    match current.as_ref().statement.as_ref().preceding {
                        Some(Preceding::Parent(mut parent)) => {
                            let following = match (
                                current.as_ref().statement.as_ref().child,
                                current.as_ref().statement.as_ref().next,
                            ) {
                                (Some(x), None) | (None, Some(x)) => x,
                                (Some(x), Some(_)) if x == next_state_node.as_ref().statement => x,
                                (Some(_), Some(x)) if x == next_state_node.as_ref().statement => x,
                                _ => unreachable!(),
                            };
                            parent.as_mut().child = Some(following);
                        }
                        Some(Preceding::Previous(mut previous)) => {
                            let following = match (
                                current.as_ref().statement.as_ref().child,
                                current.as_ref().statement.as_ref().next,
                            ) {
                                (Some(x), None) | (None, Some(x)) => x,
                                (Some(x), Some(_)) if x == next_state_node.as_ref().statement => x,
                                (Some(_), Some(x)) if x == next_state_node.as_ref().statement => x,
                                _ => unreachable!(),
                            };
                            previous.as_mut().next = Some(following);
                        }
                        None => {
                            debug_assert_eq!(current.as_ref().statement, first_node);
                            first_node = next_state_node.as_ref().statement;
                        }
                    }
                    alloc::dealloc(
                        current.as_ref().statement.as_ptr().cast(),
                        alloc::Layout::new::<NewNode>(),
                    );
                }

                // Update state node
                {
                    next_state_node.as_mut().prev = current.as_ref().prev;
                    current.as_ref().prev.unwrap().as_mut().next = (Some(next_state_node), None);
                    alloc::dealloc(
                        current.as_ptr().cast(),
                        alloc::Layout::new::<NewStateNode>(),
                    );
                }
            }
            Op::Syscall(Syscall::Read) => {
                match current.as_ref().statement.as_ref().statement.arg.as_slice() {
                    [Value::Variable(Variable {
                        identifier,
                        index: None,
                    }), Value::Literal(Literal::Integer(_))] => {
                        let state =
                            Type::from(current.as_ref().state.get(identifier).unwrap().clone());

                        // println!("identifier.as_ptr(): {:?}",identifier.as_ptr());
                        // println!("identifier.capacity(): {:?}",identifier.capacity());
                        // println!("identifier.len(): {:?}",identifier.len());

                        // This will may move `identifier` so we need to re-acquire it after.
                        current
                            .as_mut()
                            .statement
                            .as_mut()
                            .statement
                            .arg
                            .insert(1, Value::Type(state));
                        let Variable { identifier, .. } = current
                            .as_ref()
                            .statement
                            .as_ref()
                            .statement
                            .arg[0]
                            .variable()
                            .unwrap();

                        // println!("identifier.as_ptr(): {:?}",identifier.as_ptr());
                        // println!("identifier.capacity(): {:?}",identifier.capacity());
                        // println!("identifier.len(): {:?}",identifier.len());
                        // assert!(false);
                        // println!("checking ident: {identifier:?}");
                        // assert!(false);
                        let ident_clone = identifier.clone();
                        // assert!(false);
                        read.insert(ident_clone);
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }

    (first_node, read)
}

pub unsafe fn finish_optimized_tree(
    new_nodes: NonNull<NewNode>,
    read: HashSet<Identifier>,
) -> NonNull<NewNode> {
    // After iterating through the full AST we now know which variables are used so can remove
    // unused variables
    let mut first = new_nodes;
    let mut stack = vec![new_nodes];
    while let Some(current) = stack.pop() {
        match current.as_ref().statement.op {
            Op::Special(Special::Type) => match current.as_ref().statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                }), Value::Type(_), Value::Literal(_)] => {
                    if !read.contains(identifier) {
                        dbg!();
                        match current.as_ref().preceding {
                            Some(Preceding::Parent(mut parent)) => {
                                parent.as_mut().child = current.as_ref().next;
                            }
                            Some(Preceding::Previous(mut previous)) => {
                                previous.as_mut().next = current.as_ref().next;
                            }
                            None => {
                                debug_assert_eq!(first, current);
                                // dbg!(&current.as_ref().child.unwrap().as_ref().statement);
                                // dbg!(&current.as_ref().statement);
                                first = current.as_ref().next.unwrap();
                            }
                        }
                        if let Some(mut next) = current.as_ref().next {
                            next.as_mut().preceding = current.as_ref().preceding;
                            stack.push(next);
                        }
                        debug_assert!(current.as_ref().child.is_none());

                        alloc::dealloc(current.as_ptr().cast(), alloc::Layout::new::<NewNode>());
                    }
                }
                _ => todo!(),
            },
            _ => {
                if let Some(next) = current.as_ref().next {
                    stack.push(next);
                }
                if let Some(child) = current.as_ref().child {
                    stack.push(child);
                }
            }
        }
    }

    first
}

// Applies typical optimizations. E.g. removing unused variables, unreachable code, etc.
pub unsafe fn optimize(graph: NonNull<NewStateNode>) -> NonNull<NewNode> {
    // Construct new optimized abstract syntax tree.
    // TODO This doesn't dealloc anything in `graph` which may be very very big. Do this deallocation.
    let (new_nodes, read) = build_optimized_tree(graph);

    dbg!(&read);

    finish_optimized_tree(new_nodes, read)
}

unsafe fn new_passback_end(mut node: NonNull<NewStateNode>, end: GraphNodeEnd) {
    debug_assert!(node.as_ref().unexplored.0.is_empty());
    debug_assert!(node.as_ref().unexplored.1.is_empty());
    debug_assert!(node.as_ref().cost.is_none());

    // TODO Simplify this.
    let type_state_cost = TypeState::from(node.as_ref().state.clone()).cost();
    node.as_mut().cost = Some(type_state_cost.saturating_add(end.cost()));

    // Backpropagate the cost to `node.prev`, deallocating nodes not in the lowest cost path.
    while let Some(mut prev) = node.as_ref().prev {
        debug_assert!(prev.as_ref().cost.is_none());

        // If `node` is from `prev.unexplored.0`.
        if let Some((i, _)) = prev
            .as_ref()
            .unexplored
            .0
            .iter()
            .enumerate()
            .find(|(_, &x)| x == node)
        {
            // Remove `node` from `prev.unexplored`.
            prev.as_mut().unexplored.0.remove(i);

            // If `prev` has previously explored another node.
            if let Some(next) = prev.as_ref().next.0 {
                // If the path following `node` is lower cost than the previously explored path
                // following `next`. Set the next node as `node`.
                if node.as_ref().cost.unwrap() < next.as_ref().cost.unwrap() {
                    prev.as_mut().next.0 = Some(node);
                    dealloc_tree(next);
                }
                // If the path following `node` is not lower cost than the previously explored path
                // following `next`.
                else {
                    dealloc_tree(node);
                }
            } else {
                prev.as_mut().next.0 = Some(node);
            }
        }
        // If `node` is from `prev.unexplored.1`.
        else if let Some((i, _)) = prev
            .as_ref()
            .unexplored
            .1
            .iter()
            .enumerate()
            .find(|(_, &x)| x == node)
        {
            // Remove `node` from `prev.unexplored`.
            prev.as_mut().unexplored.1.remove(i);

            // If `prev` has previously explored another node.
            if let Some(next) = prev.as_ref().next.1 {
                // If the path following `node` is lower cost than the previously explored path
                // following `next`. Set the next node as `node`.
                if node.as_ref().cost.unwrap() < next.as_ref().cost.unwrap() {
                    prev.as_mut().next.1 = Some(node);
                    dealloc_tree(next);
                }
                // If the path following `node` is not lower cost than the previously explored path
                // following `next`.
                else {
                    dealloc_tree(node);
                }
            } else {
                prev.as_mut().next.1 = Some(node);
            }
        } else {
            unreachable!()
        }

        // If all the paths following `prev` have been explored.
        if prev.as_ref().unexplored.0.is_empty() && prev.as_ref().unexplored.1.is_empty() {
            let prev_cost_next_one = prev
                .as_ref()
                .next
                .0
                .and_then(|i| i.as_ref().cost)
                .unwrap_or(0);
            let prev_cost_next_two = prev
                .as_ref()
                .next
                .1
                .and_then(|i| i.as_ref().cost)
                .unwrap_or(0);
            // Add the cost of the paths plus 1 for the length.
            let prev_cost = prev_cost_next_one
                .saturating_add(prev_cost_next_two)
                .saturating_add(1);
            prev.as_mut().cost = Some(prev_cost);
        }
        // If there remain unexplored paths following `prev`.
        else {
            break;
        }

        node = prev;
    }
}

#[derive(Debug)]
enum GraphNodeEnd {
    /// E.g. `exit` syscall
    Valid,
    /// E.g. any syscall other than `exit`
    Invalid,
    /// E.g. reached loop limit
    Loop,
}

impl GraphNodeEnd {
    fn cost(self) -> u64 {
        match self {
            Self::Valid => 1,
            Self::Loop => 2,
            Self::Invalid => u64::MAX,
        }
    }
}

unsafe fn new_append(
    prev: NonNull<NewStateNode>,
    scope: Option<NonNull<NewStateNode>>,
    // The statement after `prev.statement`.
    statement: NonNull<NewNode>,
    stack: &mut Vec<NonNull<NewStateNode>>,
) -> Vec<NonNull<NewStateNode>> {
    get_possible_states(&statement.as_ref().statement, &prev.as_ref().state)
        .into_iter()
        .map(|state| {
            let mut loop_limit = prev.as_ref().loop_limit.clone();
            if let Some(n) = loop_limit.last_mut() {
                *n -= 1;
            }

            let new_node = {
                let ptr = alloc::alloc(alloc::Layout::new::<NewStateNode>()).cast::<NewStateNode>();
                ptr::write(
                    ptr,
                    NewStateNode {
                        state,
                        statement,
                        prev: Some(prev),
                        next: (None, None),
                        unexplored: (Vec::new(), Vec::new()),
                        scope,
                        loop_limit,
                        cost: None,
                    },
                );
                NonNull::new(ptr).unwrap()
            };

            stack.push(new_node);

            new_node
        })
        .collect()
}

#[derive(Debug)]
pub struct NewStateNode {
    pub state: TypeValueState,
    pub statement: NonNull<NewNode>,
    pub prev: Option<NonNull<NewStateNode>>,
    /// The current best path.
    pub next: (Option<NonNull<NewStateNode>>, Option<NonNull<NewStateNode>>),
    /// The unexplored paths to evaluate.
    pub unexplored: (Vec<NonNull<NewStateNode>>, Vec<NonNull<NewStateNode>>),
    pub scope: Option<NonNull<NewStateNode>>,
    // TODO Make this `Option<u64>`
    pub loop_limit: Vec<usize>,
    pub cost: Option<u64>,
}

enum IfBool {
    True,
    False,
    Unknown,
}

unsafe fn explore_if(
    if_bool: IfBool,
    current: NonNull<NewStateNode>,
    stack: &mut Vec<NonNull<NewStateNode>>,
) -> (Vec<NonNull<NewStateNode>>, Vec<NonNull<NewStateNode>>) {
    let current_ref = current.as_ref();
    let scope = current_ref.scope;
    let ast_node = current_ref.statement.as_ref();

    match if_bool {
        IfBool::True => {
            let next_ast_node = match (ast_node.next, ast_node.child) {
                (_, Some(child)) => child,
                (Some(next), None) => next,
                (None, None) => unreachable!(),
            };
            // See 1 & 2
            (new_append(current, scope, next_ast_node, stack), Vec::new())
        }
        IfBool::False => {
            (
                if let Some(next) = ast_node.next {
                    new_append(current, scope, next, stack)
                }
                // If this AST node has no next, look for next node in parents.
                else {
                    'outer: loop {
                        let mut preceding_opt = ast_node.preceding;
                        let parent = loop {
                            match preceding_opt {
                                None => break 'outer Vec::new(),
                                Some(Preceding::Previous(previous)) => {
                                    preceding_opt = previous.as_ref().preceding;
                                }
                                Some(Preceding::Parent(parent)) => break parent,
                            }
                        };

                        // If this would exit a loop, the next statement is the 1st statement of the loop.
                        if parent.as_ref().statement.op == Op::Intrinsic(Intrinsic::Loop) {
                            debug_assert_eq!(
                                current.as_ref().scope.unwrap().as_ref().statement,
                                parent
                            );
                            let parent_child = parent.as_ref().child.unwrap();
                            break new_append(current, scope, parent_child, stack);
                        }
                        // Else if this wouldn't exit a loop, the next statement is the next statement of this parent if there is a next.
                        else if let Some(parent_next) = parent.as_ref().next {
                            break new_append(current, scope, parent_next, stack);
                        }
                    }
                },
                Vec::new(),
            )
        }
        IfBool::Unknown => {
            (
                if let Some(child) = ast_node.child {
                    new_append(current, scope, child, stack)
                } else {
                    Vec::new()
                },
                if let Some(next) = ast_node.next {
                    new_append(current, scope, next, stack)
                }
                // If this AST node has no next, look for next node in parents.
                else {
                    'outer: loop {
                        let mut preceding_opt = ast_node.preceding;
                        let parent = loop {
                            match preceding_opt {
                                None => break 'outer Vec::new(),
                                Some(Preceding::Previous(previous)) => {
                                    preceding_opt = previous.as_ref().preceding;
                                }
                                Some(Preceding::Parent(parent)) => break parent,
                            }
                        };

                        // If this would exit a loop, the next statement is the 1st statement of the loop.
                        if parent.as_ref().statement.op == Op::Intrinsic(Intrinsic::Loop) {
                            debug_assert_eq!(
                                current.as_ref().scope.unwrap().as_ref().statement,
                                parent
                            );
                            let parent_child = parent.as_ref().child.unwrap();
                            break new_append(current, scope, parent_child, stack);
                        }
                        // Else if this wouldn't exit a loop, the next statement is the next statement of this parent if there is a next.
                        else if let Some(parent_next) = parent.as_ref().next {
                            break new_append(current, scope, parent_next, stack);
                        }
                    }
                },
            )
        }
    }
}

pub unsafe fn roots(node: NonNull<NewNode>) -> Vec<NonNull<NewStateNode>> {
    get_possible_states(&node.as_ref().statement, &TypeValueState::new())
        .into_iter()
        .map(|state| {
            let ptr = alloc::alloc(alloc::Layout::new::<NewStateNode>()).cast::<NewStateNode>();
            ptr::write(
                ptr,
                NewStateNode {
                    state,
                    statement: node,
                    prev: None,
                    next: (None, None),
                    unexplored: (Vec::new(), Vec::new()),
                    scope: None,
                    loop_limit: Vec::new(),
                    cost: None,
                },
            );
            NonNull::new(ptr).unwrap()
        })
        .collect()
}

pub unsafe fn explore_node(
    mut current: NonNull<NewStateNode>,
    stack: &mut Vec<NonNull<NewStateNode>>,
) {
    let current_ref = current.as_mut();
    let ast_node = current_ref.statement.as_ref();
    let statement = &ast_node.statement;

    debug_assert!(current_ref.unexplored.0.is_empty());
    debug_assert!(current_ref.unexplored.1.is_empty());
    debug_assert!(current_ref.next.0.is_none());
    debug_assert!(current_ref.next.1.is_none());

    match statement.op {
        Op::Intrinsic(Intrinsic::If(Cmp::Eq)) => {
            match statement.arg.as_slice() {
                [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(x))] =>
                {
                    let _scope = current_ref.scope;
                    let y = current_ref
                        .state
                        .get(identifier)
                        .unwrap()
                        .integer()
                        .unwrap();

                    let if_bool = if y.value() == Some(*x) {
                        IfBool::True
                    } else if y.excludes(*x) {
                        IfBool::False
                    } else {
                        IfBool::Unknown
                    };

                    current_ref.unexplored = explore_if(if_bool, current, stack);
                }
                _ => todo!(),
            }
        }
        Op::Intrinsic(Intrinsic::If(Cmp::Lt)) => match statement.arg.as_slice() {
            [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(x))] => {
                let _scope = current_ref.scope;
                let y = current_ref
                    .state
                    .get(identifier)
                    .unwrap()
                    .integer()
                    .unwrap();

                let if_bool = if y.max() < *x {
                    IfBool::True
                } else if y.min() >= *x {
                    IfBool::False
                } else {
                    IfBool::Unknown
                };
                current_ref.unexplored = explore_if(if_bool, current, stack);
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::Loop) => {
            match statement.arg.as_slice() {
                [] => {
                    current_ref.unexplored = (
                        if let Some(child) = ast_node.child {
                            // Since `new_append` applies the same loop limit with the last
                            // element -1, we need to add a loop limit to the loop node so
                            // the children get the limit.
                            // TODO Do this in a better way.
                            current_ref.loop_limit.push(DEFAULT_LOOP_LIMIT);
                            let temp = new_append(current, Some(current), child, stack);
                            // Since the loop isn't actually in a loop we pop it after.
                            current_ref.loop_limit.pop();
                            temp
                        } else if let Some(next) = ast_node.next {
                            new_append(current, current_ref.scope, next, stack)
                        }
                        // If this AST node has no next, look for next node in parents.
                        else {
                            'outer: loop {
                                let mut preceding_opt = ast_node.preceding;
                                let parent = loop {
                                    match preceding_opt {
                                        None => break 'outer Vec::new(),
                                        Some(Preceding::Previous(previous)) => {
                                            preceding_opt = previous.as_ref().preceding;
                                        }
                                        Some(Preceding::Parent(parent)) => break parent,
                                    }
                                };

                                // If this would exit a loop, the next statement is the 1st statement of the loop.
                                if parent.as_ref().statement.op == Op::Intrinsic(Intrinsic::Loop) {
                                    debug_assert_eq!(
                                        current_ref.scope.unwrap().as_ref().statement,
                                        parent
                                    );
                                    let parent_child = parent.as_ref().child.unwrap();
                                    break new_append(
                                        current,
                                        current_ref.scope,
                                        parent_child,
                                        stack,
                                    );
                                }
                                // Else if this wouldn't exit a loop, the next statement is the next statement of this parent if there is a next.
                                else if let Some(parent_next) = parent.as_ref().next {
                                    break new_append(
                                        current,
                                        current_ref.scope,
                                        parent_next,
                                        stack,
                                    );
                                }
                            }
                        },
                        Vec::new(),
                    )
                }
                _ => todo!(),
            }
        }
        Op::Intrinsic(Intrinsic::Break) => match statement.arg.as_slice() {
            [] => {
                let prev_scope_graph_node = current_ref.scope.unwrap();
                let scope_node = prev_scope_graph_node.as_ref().statement;
                current.as_mut().unexplored = (
                    if let Some(next) = scope_node.as_ref().next {
                        let scope = prev_scope_graph_node.as_ref().scope;
                        new_append(current, scope, next, stack)
                    } else {
                        'outer: loop {
                            let mut preceding_opt = ast_node.preceding;
                            let parent = loop {
                                match preceding_opt {
                                    None => break 'outer Vec::new(),
                                    Some(Preceding::Previous(previous)) => {
                                        preceding_opt = previous.as_ref().preceding;
                                    }
                                    Some(Preceding::Parent(parent)) => break parent,
                                }
                            };

                            // If this would exit a loop, the next statement is the 1st statement of the loop.
                            if parent.as_ref().statement.op == Op::Intrinsic(Intrinsic::Loop) {
                                debug_assert_eq!(
                                    current_ref.scope.unwrap().as_ref().statement,
                                    parent
                                );
                                let parent_child = parent.as_ref().child.unwrap();
                                break new_append(current, current_ref.scope, parent_child, stack);
                            }
                            // Else if this wouldn't exit a loop, the next statement is the next statement of this parent if there is a next.
                            else if let Some(parent_next) = parent.as_ref().next {
                                break new_append(current, current_ref.scope, parent_next, stack);
                            }
                        }
                    },
                    Vec::new(),
                );
            }
            _ => todo!(),
        },
        // See 2
        Op::Syscall(Syscall::Exit) => {}
        // See 1 & 2
        _ => {
            let scope = current_ref.scope;
            current_ref.unexplored =
                (
                    new_append(current, scope, ast_node.next.unwrap(), stack),
                    Vec::new(),
                );
        }
    }
}

pub unsafe fn close_path(current: NonNull<NewStateNode>) {
    if current.as_ref().unexplored.0.is_empty() && current.as_ref().unexplored.1.is_empty() {
        if current.as_ref().statement.as_ref().statement.op == Op::Syscall(Syscall::Exit) {
            new_passback_end(current, GraphNodeEnd::Valid);
        } else {
            new_passback_end(current, GraphNodeEnd::Invalid);
        }
    } else if let Some(0) = current.as_ref().loop_limit.last() {
        new_passback_end(current, GraphNodeEnd::Loop);
    }
}

pub enum Explore {
    Finished(NonNull<NewStateNode>),
    Current(NonNull<NewStateNode>),
}

pub struct Explorer<'a> {
    roots: &'a [NonNull<NewStateNode>],
    stack: Vec<NonNull<NewStateNode>>,
}

impl<'a> Explorer<'a> {
    pub unsafe fn new(roots: &'a [NonNull<NewStateNode>]) -> Self {
        Self {
            roots,
            stack: Vec::from(roots),
        }
    }
    pub unsafe fn next(&mut self) -> Explore {
        if let Some(current) = self.stack.pop() {
            explore_node(current, &mut self.stack);
            close_path(current);
            Explore::Current(current)
        } else {
            Explore::Finished(pick_path(self.roots))
        }
    }
}

pub unsafe fn pick_path(roots: &[NonNull<NewStateNode>]) -> NonNull<NewStateNode> {
    let mut iter = roots.iter().copied();
    let mut best = iter.next().unwrap();
    for root in iter {
        // Deallocate all nodes now longer in the lowest cost path.
        if root.as_ref().cost.unwrap() < best.as_ref().cost.unwrap() {
            dealloc_tree(best);
            best = root;
        } else {
            dealloc_tree(root);
        };
    }
    best
}

unsafe fn dealloc_tree(first: NonNull<NewStateNode>) {
    let mut stack = vec![first];
    while let Some(cursor) = stack.pop() {
        let cursor_ref = cursor.as_ref();
        debug_assert!(cursor_ref.unexplored.0.is_empty());
        debug_assert!(cursor_ref.unexplored.1.is_empty());
        if let Some(a) = cursor_ref.next.0 {
            stack.push(a);
        }
        if let Some(b) = cursor_ref.next.1 {
            stack.push(b);
        }
        alloc::dealloc(cursor.as_ptr().cast(), alloc::Layout::new::<NewStateNode>());
    }
}

/// Given an incoming state (`state`) and a node, outputs the possible outgoing states.
fn get_possible_states(statement: &Statement, state: &TypeValueState) -> Vec<TypeValueState> {
    match statement.op {
        Op::Special(Special::Type) => match statement.arg.as_slice() {
            [Value::Variable(Variable {
                identifier,
                index: None,
            }), Value::Type(x)] => match state.get(identifier) {
                // You can't redefine a static.
                Some(_) => Vec::new(),
                None => {
                    let mut new_state = state.clone();
                    new_state.insert(identifier.clone(), TypeValue::from(x.clone()));
                    vec![new_state]
                }
            },
            _ => todo!(),
        },
        Op::Syscall(Syscall::Exit) => match statement.arg.as_slice() {
            // TODO Check the variable for `_integer` fits into i32.
            [Value::Literal(Literal::Integer(_integer))] => vec![state.clone()],
            // TODO Check the variable for `identifier` fits into i32.
            [Value::Variable(Variable {
                identifier,
                index: None,
            })] => match state.get(identifier) {
                Some(TypeValue::Integer(integer)) => match integer {
                    TypeValueInteger::I8(_) => vec![state.clone()],
                    TypeValueInteger::U8(_) => vec![state.clone()],
                    TypeValueInteger::I16(_) => vec![state.clone()],
                    TypeValueInteger::U16(_) => vec![state.clone()],
                    TypeValueInteger::I32(_) => vec![state.clone()],
                    TypeValueInteger::U32(x) if x.max() <= (i32::MAX as u32) => vec![state.clone()],
                    TypeValueInteger::I64(x)
                        if x.max() <= (i32::MAX as i64) && x.min() >= (i32::MIN as i64) =>
                    {
                        vec![state.clone()]
                    }
                    TypeValueInteger::U64(x) if x.max() <= (i32::MAX as u64) => vec![state.clone()],
                    _ => Vec::new(),
                },
                None => Vec::new(),
                _ => todo!(),
            },
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::Assign) => match statement.arg.as_slice() {
            [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(x))] => {
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
            [Value::Variable(Variable { identifier, .. }), outer @ Value::Literal(Literal::Integer(first)), tail @ ..] => {
                match state.get(identifier) {
                    None => {
                        let possible = tail.iter().fold(Type::possible(*first), |acc, x| {
                            let next = Type::possible(*x.literal().unwrap().integer().unwrap());
                            acc.into_iter().filter(|y| next.contains(y)).collect()
                        });
                        possible
                            .into_iter()
                            .map(|item| {
                                let mut new_state = state.clone();
                                let values = std::iter::repeat(item.clone())
                                    .zip(
                                        std::iter::once(outer)
                                            .chain(tail)
                                            .map(|v| *v.literal().unwrap().integer().unwrap()),
                                    )
                                    .map(TypeValue::from)
                                    .collect::<Vec<_>>();

                                new_state.insert(
                                    identifier.clone(),
                                    TypeValue::Array(TypeValueArray { item, values }),
                                );
                                new_state
                            })
                            .collect()
                    }
                    _ => todo!(),
                }
            }
            [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::String(s))] => {
                match state.get(identifier) {
                    None => {
                        let mut new_state = state.clone();
                        let values = std::iter::repeat(Type::U8)
                            .zip(s.as_bytes().iter().map(|v| *v as i128))
                            .map(TypeValue::from)
                            .collect::<Vec<_>>();

                        new_state.insert(
                            identifier.clone(),
                            TypeValue::Array(TypeValueArray {
                                item: Type::U8,
                                values,
                            }),
                        );
                        vec![new_state]
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
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
        Op::Intrinsic(Intrinsic::AddAssign) => match statement.arg.as_slice() {
            [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(x))] => {
                match state.get(identifier) {
                    Some(TypeValue::Integer(y)) => match y.overflowing_add(*x) {
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
        Op::Intrinsic(Intrinsic::If(_)) => match statement.arg.as_slice() {
            [Value::Literal(Literal::Integer(_)), Value::Variable(Variable { identifier, .. })]
            | [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(_))] => {
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
        Op::Special(Special::Require(Cmp::Le)) => {
            match statement.arg.as_slice() {
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
            }
        }
        Op::Syscall(Syscall::Read) => match statement.arg.as_slice() {
            [Value::Variable(Variable { identifier, .. }), Value::Literal(Literal::Integer(_))] => {
                match state.get(identifier) {
                    // Iterates over the set of integer types which could contain `x`, returning a new state for each possibility.
                    None => TypeValueInteger::any()
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
        },
        Op::Syscall(Syscall::Write) => match statement.arg.as_slice() {
            [Value::Variable(Variable {
                identifier: empty, ..
            }), Value::Literal(Literal::Integer(_)), Value::Variable(Variable { identifier, .. })]
                if empty == b"_" =>
            {
                match state.get(identifier) {
                    // All defined values have known sizes so can be written.
                    Some(_) => vec![state.clone()],
                    // You cannot write an undefined value.
                    None => Vec::new(),
                }
            }
            _ => todo!(),
        },
        Op::Syscall(Syscall::MemfdCreate) => match statement.arg.as_slice() {
            [Value::Variable(Variable { identifier, .. })] => match state.get(identifier) {
                None => [
                    TypeValueInteger::I32(MyRange::any()),
                    TypeValueInteger::I64(MyRange::any()),
                ]
                .into_iter()
                .map(|p| {
                    let mut new_state = state.clone();
                    new_state.insert(identifier.clone(), TypeValue::Integer(p));
                    new_state
                })
                .collect(),
                _ => todo!(),
            },
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::Loop) => {
            match statement.arg.as_slice() {
                [] => vec![state.clone()],
                [Value::Literal(Literal::Integer(integer))] if u64::try_from(*integer).is_ok() => {
                    vec![state.clone()]
                }
                _ => todo!(),
            }
        }
        Op::Intrinsic(Intrinsic::Break) => match statement.arg.as_slice() {
            [] => vec![state.clone()],
            _ => todo!(),
        },
        _ => todo!(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeValueState(HashMap<Identifier, TypeValue>);

#[allow(dead_code)]
impl TypeValueState {
    pub fn new() -> Self {
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
impl<const N: usize> From<[(Identifier, TypeValue); N]> for TypeValueState {
    fn from(arr: [(Identifier, TypeValue); N]) -> Self {
        Self(HashMap::from(arr))
    }
}

#[derive(Debug, Clone)]
struct TypeState(HashMap<Identifier, Type>);

impl From<TypeValueState> for TypeState {
    fn from(x: TypeValueState) -> Self {
        Self(x.0.into_iter().map(|(i, x)| (i, Type::from(x))).collect())
    }
}

#[allow(dead_code)]
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
    fn cost(&self) -> u64 {
        self.0
            .iter()
            .fold(0, |acc, (_, x)| acc.saturating_add(x.cost()))
    }
}

impl Type {
    fn cost(&self) -> u64 {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
            Self::I8 => 3,
            Self::I16 => 5,
            Self::I32 => 9,
            Self::I64 => 17,
            Self::Array(box array) => array.cost(),
        }
    }
}
impl Array {
    fn cost(&self) -> u64 {
        self.item.cost().saturating_mul(self.len as u64)
    }
}

impl From<TypeValue> for Type {
    fn from(x: TypeValue) -> Self {
        match x {
            TypeValue::Integer(int) => Type::from(int),
            TypeValue::Array(array) => Type::from(array),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeValue {
    Integer(TypeValueInteger),
    Array(TypeValueArray),
}

#[allow(unreachable_patterns)]
impl TypeValue {
    fn integer_mut(&mut self) -> Option<&mut TypeValueInteger> {
        match self {
            Self::Integer(x) => Some(x),
            _ => None,
        }
    }

    fn integer(&self) -> Option<&TypeValueInteger> {
        match self {
            Self::Integer(integer) => Some(integer),
            _ => None,
        }
    }
}

impl From<(Type, i128)> for TypeValue {
    fn from((x, y): (Type, i128)) -> TypeValue {
        match x {
            Type::U8 => TypeValue::Integer(TypeValueInteger::U8(MyRange::from(y as u8))),
            Type::U16 => TypeValue::Integer(TypeValueInteger::U16(MyRange::from(y as u16))),
            Type::U32 => TypeValue::Integer(TypeValueInteger::U32(MyRange::from(y as u32))),
            Type::U64 => TypeValue::Integer(TypeValueInteger::U64(MyRange::from(y as u64))),
            Type::I8 => TypeValue::Integer(TypeValueInteger::I8(MyRange::from(y as i8))),
            Type::I16 => TypeValue::Integer(TypeValueInteger::I16(MyRange::from(y as i16))),
            Type::I32 => TypeValue::Integer(TypeValueInteger::I32(MyRange::from(y as i32))),
            Type::I64 => TypeValue::Integer(TypeValueInteger::I64(MyRange::from(y as i64))),
            Type::Array(_) => unreachable!(),
        }
    }
}

impl From<Type> for TypeValue {
    fn from(x: Type) -> TypeValue {
        match x {
            Type::U8 => TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
            Type::U16 => TypeValue::Integer(TypeValueInteger::U16(MyRange::any())),
            Type::U32 => TypeValue::Integer(TypeValueInteger::U32(MyRange::any())),
            Type::U64 => TypeValue::Integer(TypeValueInteger::U64(MyRange::any())),
            Type::I8 => TypeValue::Integer(TypeValueInteger::I8(MyRange::any())),
            Type::I16 => TypeValue::Integer(TypeValueInteger::I16(MyRange::any())),
            Type::I32 => TypeValue::Integer(TypeValueInteger::I32(MyRange::any())),
            Type::I64 => TypeValue::Integer(TypeValueInteger::I64(MyRange::any())),
            Type::Array(box Array { item, len }) => TypeValue::Array(TypeValueArray {
                item: item.clone(),
                values: std::iter::repeat(TypeValue::from(item)).take(len).collect(),
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeValueArray {
    item: Type,
    values: Vec<TypeValue>,
}

impl From<TypeValueArray> for Type {
    fn from(x: TypeValueArray) -> Self {
        Type::Array(Box::new(Array {
            item: x.item.clone(),
            len: x.values.len(),
        }))
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

impl From<TypeValueInteger> for Type {
    fn from(x: TypeValueInteger) -> Self {
        match x {
            TypeValueInteger::U8(_) => Type::U8,
            TypeValueInteger::U16(_) => Type::U16,
            TypeValueInteger::U32(_) => Type::U32,
            TypeValueInteger::U64(_) => Type::U64,
            TypeValueInteger::I8(_) => Type::I8,
            TypeValueInteger::I16(_) => Type::I16,
            TypeValueInteger::I32(_) => Type::I32,
            TypeValueInteger::I64(_) => Type::I64,
        }
    }
}

#[allow(dead_code)]
impl TypeValueInteger {
    pub fn any() -> [Self; 8] {
        [
            Self::I64(MyRange::any()),
            Self::I32(MyRange::any()),
            Self::I16(MyRange::any()),
            Self::I8(MyRange::any()),
            Self::U64(MyRange::any()),
            Self::U32(MyRange::any()),
            Self::U16(MyRange::any()),
            Self::U8(MyRange::any()),
        ]
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

    fn excludes(&self, rhs: i128) -> bool {
        match self {
            Self::U8(range) => match u8::try_from(rhs) {
                Err(_) => true,
                Ok(x) => range.excludes(x),
            },
            Self::U16(range) => match u16::try_from(rhs) {
                Err(_) => true,
                Ok(x) => range.excludes(x),
            },
            Self::U32(range) => match u32::try_from(rhs) {
                Err(_) => true,
                Ok(x) => range.excludes(x),
            },
            Self::U64(range) => match u64::try_from(rhs) {
                Err(_) => true,
                Ok(x) => range.excludes(x),
            },
            Self::I8(range) => match i8::try_from(rhs) {
                Err(_) => true,
                Ok(x) => range.excludes(x),
            },
            Self::I16(range) => match i16::try_from(rhs) {
                Err(_) => true,
                Ok(x) => range.excludes(x),
            },
            Self::I32(range) => match i32::try_from(rhs) {
                Err(_) => true,
                Ok(x) => range.excludes(x),
            },
            Self::I64(range) => match i64::try_from(rhs) {
                Err(_) => true,
                Ok(x) => range.excludes(x),
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
#[allow(dead_code)]
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
    > From<T> for MyRange<T>
{
    fn from(x: T) -> Self {
        Self { start: x, end: x }
    }
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

#[allow(dead_code)]
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
    fn excludes(&self, x: T) -> bool {
        self.min() > x || self.max() < x
    }
    pub fn any() -> Self {
        Self::new(T::min_value(), T::max_value())
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
