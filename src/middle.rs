#![allow(dead_code)]

use crate::ast::*;
use num_traits::bounds::Bounded;
use num_traits::identities::One;
use num_traits::identities::Zero;
use num_traits::ops::checked::CheckedDiv;
use num_traits::ops::overflowing::OverflowingAdd;
use num_traits::ops::overflowing::OverflowingMul;
use num_traits::ops::overflowing::OverflowingSub;
use num_traits::Signed;
use num_traits::Unsigned;
use std::alloc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ptr;
use std::ptr::NonNull;
use std::rc::Rc;

const DEFAULT_LOOP_LIMIT: usize = 100;
#[allow(dead_code)]
const UNROLL_LIMIT: usize = 4096;

unsafe fn remove_node(
    current: NonNull<NewStateNode>,
    first_state_node: &mut Option<NonNull<NewStateNode>>,
) -> Option<NonNull<NewStateNode>> {
    let rtn = match current.as_ref().next {
        (Some(mut next_node), None) | (None, Some(mut next_node)) => {
            // x.as_mut().statement.as_mut().preceding = current.as_ref().statement.as_ref().preceding;
            // x.as_mut().prev.unwrap().as_mut().next = (current.as_ref().prev,None);

            // Update previous value of next state node and syntax node.
            next_node.as_mut().prev = current.as_ref().prev;
            next_node.as_mut().statement.as_mut().preceding =
                current.as_ref().statement.as_ref().preceding;

            // Update next value of previous state node and syntax node.
            match (
                current.as_ref().statement.as_ref().preceding,
                current.as_ref().prev,
            ) {
                (Some(Preceding::Parent(mut parent)), Some(mut prev_node)) => {
                    debug_assert_eq!(prev_node.as_ref().statement, parent);
                    prev_node.as_mut().next = (Some(next_node), None);
                    parent.as_mut().child = Some(next_node.as_ref().statement);
                }
                (Some(Preceding::Previous(mut previous)), Some(mut prev_node)) => {
                    debug_assert_eq!(prev_node.as_ref().statement, previous);
                    prev_node.as_mut().next = (Some(next_node), None);
                    previous.as_mut().next = Some(next_node.as_ref().statement);
                }
                (None, None) => {
                    debug_assert_eq!(Some(current), *first_state_node);
                    *first_state_node = Some(next_node);
                    todo!();
                }
                _ => unreachable!(),
            }

            Some(next_node)
        }
        // This is checked before this function is called.
        (Some(_), Some(_)) => unreachable!(),
        (None, None) => {
            match (
                current.as_ref().statement.as_ref().preceding,
                current.as_ref().prev,
            ) {
                (Some(Preceding::Parent(mut parent)), Some(mut prev_node)) => {
                    debug_assert_eq!(prev_node.as_ref().statement, parent);
                    prev_node.as_mut().next = (None, None);
                    parent.as_mut().child = None;
                }
                (Some(Preceding::Previous(mut previous)), Some(mut prev_node)) => {
                    debug_assert_eq!(prev_node.as_ref().statement, previous);
                    prev_node.as_mut().next = (None, None);
                    previous.as_mut().next = None;
                }
                (None, None) => {
                    debug_assert_eq!(Some(current), *first_state_node);
                    *first_state_node = None;
                    todo!();
                }
                _ => unreachable!(),
            }

            None
        }
    };

    alloc::dealloc(
        current.as_ref().statement.as_ptr().cast(),
        alloc::Layout::new::<NewNode>(),
    );
    alloc::dealloc(
        current.as_ptr().cast(),
        alloc::Layout::new::<NewStateNode>(),
    );

    rtn
}

pub unsafe fn build_optimized_tree(
    graph: NonNull<NewStateNode>,
) -> (Option<NonNull<NewNode>>, HashSet<Variable>) {
    let mut types = HashMap::<Variable, Type>::new();
    let mut read = HashSet::<Variable>::new();

    // The 1st step is iterating over nodes which don't diverge
    // (`next.0.is_none() || next.1.is_none()`), all these can be simply inlined.
    let mut next_state_node = Some(graph);
    let mut first_state_node = Some(graph);

    #[cfg(debug_assertions)]
    let mut checker = 0;
    while let Some(mut current) = next_state_node {
        #[cfg(debug_assertions)]
        {
            assert!(checker < 100);
            checker += 1;
        }

        // Assert only 1 possible next;
        debug_assert!(current
            .as_ref()
            .next
            .0
            .and(current.as_ref().next.1)
            .is_none());

        // println!("------------checking optimization input------------");
        // let mut check_stack = vec![first_state_node.unwrap()];
        // let mut check_counter = 0;
        // while let Some(check_current) = check_stack.pop() {
        //     println!();
        //     println!("{:?}: {:?} {:?} {:?}", check_current, check_current.as_ref().prev, check_current.as_ref().statement, check_current.as_ref().next);
        //     println!("{:?}: {:?} {:?} {:?} {:?}", check_current.as_ref().statement, check_current.as_ref().statement.as_ref().statement.op, check_current.as_ref().statement.as_ref().preceding, check_current.as_ref().statement.as_ref().child, check_current.as_ref().statement.as_ref().next);

        //     if let Some(one) = check_current.as_ref().next.0 {
        //         check_stack.push(one);
        //     }
        //     if let Some(two) = check_current.as_ref().next.1 {
        //         check_stack.push(two);
        //     }
        //     check_counter += 1;
        //     if check_counter > 2 {
        //         break;
        //     }
        // }
        // println!("-----------------------------------------------");

        // println!("current: {current:?}");
        // println!("statement: {:?}",current.as_ref().statement);

        // dbg!(&current.as_ref().statement.as_ref().statement.op);

        // println!("op: {:?}", current.as_ref().statement.as_ref().statement.op);

        let slice = current.as_ref().statement.as_ref().statement.arg.as_slice();
        match current.as_ref().statement.as_ref().statement.op {
            Op::Intrinsic(Intrinsic::Assign) => match slice {
                [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap()
                        .clone();
                    let variable_type = Type::from(variable_state);
                    let existing = types.insert(variable.clone(), variable_type.clone());

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
                    } else {
                        todo!()
                    }

                    // Set next node.
                    debug_assert!(current
                        .as_ref()
                        .next
                        .0
                        .and(current.as_ref().next.1)
                        .is_none());
                    next_state_node = current.as_ref().next.0.or(current.as_ref().next.1);
                }
                [Value::Variable(variable), Value::Literal(Literal::String(_))] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap()
                        .clone();
                    let variable_type = Type::from(variable_state);
                    let existing = types.insert(variable.clone(), variable_type.clone());

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
                    } else {
                        todo!()
                    }

                    // Set next node.
                    debug_assert!(current
                        .as_ref()
                        .next
                        .0
                        .and(current.as_ref().next.1)
                        .is_none());
                    next_state_node = current.as_ref().next.0.or(current.as_ref().next.1);
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Exit) => match slice {
                [Value::Variable(variable)] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap();
                    match variable_state {
                        TypeValue::Integer(TypeValueInteger::U8(range)) => {
                            if let Some(exact) = range.value() {
                                current.as_mut().statement.as_mut().statement.arg =
                                    vec![Value::Literal(Literal::Integer(i128::from(exact)))];
                            } else {
                                read.insert(variable.clone());
                            }

                            // Set next node.
                            {
                                debug_assert!(current
                                    .as_ref()
                                    .next
                                    .0
                                    .and(current.as_ref().next.1)
                                    .is_none());
                                // Exits mark the end of execution, their shouldn't be a following node.
                                debug_assert_eq!(current.as_ref().next, (None, None));

                                // Update syntax node
                                debug_assert_eq!(current.as_mut().statement.as_mut().child, None);
                                if let Some(next_syntax_node) =
                                    current.as_ref().statement.as_ref().next
                                {
                                    dealloc_syntax(next_syntax_node);
                                    current.as_mut().statement.as_mut().next = None;
                                }

                                // Set next node
                                next_state_node = None;
                            }
                        }
                        _ => todo!(),
                    }
                }
                [Value::Literal(Literal::Integer(_))] => {
                    // Set next node.
                    {
                        debug_assert!(current
                            .as_ref()
                            .next
                            .0
                            .and(current.as_ref().next.1)
                            .is_none());
                        // Exits mark the end of execution, their shouldn't be a following node.
                        debug_assert_eq!(current.as_ref().next, (None, None));

                        // Update syntax node
                        debug_assert_eq!(current.as_mut().statement.as_mut().child, None);
                        if let Some(next_syntax_node) = current.as_ref().statement.as_ref().next {
                            dealloc_syntax(next_syntax_node);
                            current.as_mut().statement.as_mut().next = None;
                        }

                        // Set next node
                        next_state_node = None;
                    }
                }
                _ => todo!(),
            },
            // On the linear path, these statements can simply be removed.
            Op::Intrinsic(Intrinsic::AddAssign) => match slice {
                [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap();
                    match variable_state {
                        TypeValue::Integer(TypeValueInteger::U8(range))
                            if range.value().is_some() =>
                        {
                            // Removes node and sets next node.
                            next_state_node = remove_node(current, &mut first_state_node);
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            // On the linear path, these statements can simply be removed.
            Op::Intrinsic(Intrinsic::SubAssign) => match slice {
                [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap();
                    match variable_state {
                        TypeValue::Integer(TypeValueInteger::U8(range))
                            if range.value().is_some() =>
                        {
                            // Removes node and sets next node.
                            next_state_node = remove_node(current, &mut first_state_node);
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            // On the linear path, these statements can simply be removed.
            Op::Intrinsic(Intrinsic::MulAssign) => match slice {
                [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap();
                    match variable_state {
                        TypeValue::Integer(TypeValueInteger::U8(range))
                            if range.value().is_some() =>
                        {
                            // Removes node and sets next node.
                            next_state_node = remove_node(current, &mut first_state_node);
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            // On the linear path, these statements can simply be removed.
            Op::Intrinsic(Intrinsic::DivAssign) => match slice {
                [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap();
                    match variable_state {
                        TypeValue::Integer(TypeValueInteger::U8(range))
                            if range.value().is_some() =>
                        {
                            // Removes node and sets next node.
                            next_state_node = remove_node(current, &mut first_state_node);
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::AndAssign) => match slice {
                [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap();
                    match variable_state {
                        TypeValue::Integer(TypeValueInteger::U8(range))
                            if range.value().is_some() =>
                        {
                            // Removes node and sets next node.
                            next_state_node = remove_node(current, &mut first_state_node);
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::OrAssign) => match slice {
                [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap();
                    match variable_state {
                        TypeValue::Integer(TypeValueInteger::U8(range))
                            if range.value().is_some() =>
                        {
                            // Removes node and sets next node.
                            next_state_node = remove_node(current, &mut first_state_node);
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::XorAssign) => match slice {
                [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap();
                    match variable_state {
                        TypeValue::Integer(TypeValueInteger::U8(range))
                            if range.value().is_some() =>
                        {
                            // Removes node and sets next node.
                            next_state_node = remove_node(current, &mut first_state_node);
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            // `current` is not used after `current = prev;` but it may be in the future, and I
            // don't want to obfuscate this complex logic further.
            Op::Intrinsic(Intrinsic::If(Cmp::Eq)) => {
                debug_assert!(current
                    .as_ref()
                    .next
                    .0
                    .and(current.as_ref().next.1)
                    .is_none());
                // We can unwrap here since it would be an error for an if to be the last node.
                let mut if_next_state_node =
                    current.as_ref().next.0.or(current.as_ref().next.1).unwrap();

                // Update syntax node
                {
                    match current.as_ref().statement.as_ref().preceding {
                        Some(Preceding::Parent(mut parent)) => {
                            let following = match (
                                current.as_ref().statement.as_ref().child,
                                current.as_ref().statement.as_ref().next,
                            ) {
                                (Some(x), None) | (None, Some(x)) => x,
                                (Some(x), Some(_))
                                    if x == if_next_state_node.as_ref().statement =>
                                {
                                    x
                                }
                                (Some(_), Some(x))
                                    if x == if_next_state_node.as_ref().statement =>
                                {
                                    x
                                }
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
                                (Some(x), Some(_))
                                    if x == if_next_state_node.as_ref().statement =>
                                {
                                    x
                                }
                                (Some(_), Some(x))
                                    if x == if_next_state_node.as_ref().statement =>
                                {
                                    x
                                }
                                _ => unreachable!(),
                            };
                            previous.as_mut().next = Some(following);
                        }
                        None => {
                            debug_assert_eq!(Some(current), first_state_node);
                            first_state_node = Some(if_next_state_node);
                        }
                    }
                    if_next_state_node.as_mut().statement.as_mut().preceding =
                        current.as_ref().statement.as_ref().preceding;

                    alloc::dealloc(
                        current.as_ref().statement.as_ptr().cast(),
                        alloc::Layout::new::<NewNode>(),
                    );
                }

                // Update state node
                {
                    if_next_state_node.as_mut().prev = current.as_ref().prev;
                    current.as_ref().prev.unwrap().as_mut().next = (Some(if_next_state_node), None);
                    alloc::dealloc(
                        current.as_ptr().cast(),
                        alloc::Layout::new::<NewStateNode>(),
                    );
                }

                // Set next node.
                next_state_node = Some(if_next_state_node);
            }
            Op::Syscall(Syscall::Read) => match slice {
                [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                    let state = Type::from(
                        current
                            .as_ref()
                            .state
                            .get(&TypeKey::from(variable))
                            .unwrap()
                            .clone(),
                    );

                    // This will may move `identifier` so we need to re-acquire it after.
                    current
                        .as_mut()
                        .statement
                        .as_mut()
                        .statement
                        .arg
                        .insert(1, Value::Type(state));
                    let variable = current.as_ref().statement.as_ref().statement.arg[0]
                        .variable()
                        .unwrap();

                    read.insert(variable.clone());

                    // Set next node.
                    debug_assert!(current
                        .as_ref()
                        .next
                        .0
                        .and(current.as_ref().next.1)
                        .is_none());
                    next_state_node = current.as_ref().next.0.or(current.as_ref().next.1);
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Write) => match slice {
                [Value::Literal(Literal::Integer(_)), Value::Variable(variable)] => {
                    debug_assert!(current
                        .as_ref()
                        .state
                        .contains_key(&TypeKey::from(variable)));
                    read.insert(variable.clone());

                    // Set next node.
                    debug_assert!(current
                        .as_ref()
                        .next
                        .0
                        .and(current.as_ref().next.1)
                        .is_none());
                    next_state_node = current.as_ref().next.0.or(current.as_ref().next.1);
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Add) => match slice {
                [value @ Value::Variable(variable), Value::Variable(_), Value::Variable(_)] => {
                    // let variable_state = (
                    //     current.as_ref().state.get(out),
                    //     current.as_ref().state.get(rhs).unwrap(),
                    //     current.as_ref().state.get(lhs).unwrap()
                    // );
                    let variable_state = current
                        .as_ref()
                        .state
                        .get(&TypeKey::from(variable))
                        .unwrap();
                    let variable_type = Type::from(variable_state.clone());
                    match variable_state {
                        TypeValue::Integer(TypeValueInteger::U8(a)) if let Some(x) = a.value() => {
                            let existing = types.insert(variable.clone(), variable_type.clone());

                            // If not already declared this declares the type of the variable.
                            if existing.is_none() {
                                current.as_mut().statement.as_mut().statement.op =
                                    Op::Special(Special::Type);
                                current.as_mut().statement.as_mut().statement.arg = vec![
                                    value.clone(),
                                    Value::Type(variable_type),
                                    Value::Literal(Literal::Integer(x as _)),
                                ];
                            } else {
                                todo!()
                            }

                            // Set next node.
                            debug_assert!(current
                                .as_ref()
                                .next
                                .0
                                .and(current.as_ref().next.1)
                                .is_none());
                            next_state_node = current.as_ref().next.0.or(current.as_ref().next.1);
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            Op::Assembly(Assembly::Mov) => match slice {
                [Value::Register(_), Value::Literal(Literal::Integer(_))] => {
                    // Set next node.
                    debug_assert!(current
                        .as_ref()
                        .next
                        .0
                        .or(current.as_ref().next.1)
                        .is_some());
                    next_state_node = current.as_ref().next.0.or(current.as_ref().next.1);
                }
                [Value::Register(_), Value::Variable(variable)] => {
                    match current.as_ref().state.get(&TypeKey::from(variable)) {
                        Some(TypeValue::Integer(TypeValueInteger::U64(range)))
                            if let Some(integer) = range.value() =>
                        {
                            current.as_mut().statement.as_mut().statement.arg[1] =
                                Value::Literal(Literal::Integer(integer as i128));
                        }
                        Some(TypeValue::Integer(TypeValueInteger::U32(range)))
                            if let Some(integer) = range.value() =>
                        {
                            current.as_mut().statement.as_mut().statement.arg[1] =
                                Value::Literal(Literal::Integer(integer as i128));
                        }
                        Some(TypeValue::Integer(TypeValueInteger::U16(range)))
                            if let Some(integer) = range.value() =>
                        {
                            current.as_mut().statement.as_mut().statement.arg[1] =
                                Value::Literal(Literal::Integer(integer as i128));
                        }
                        Some(TypeValue::Integer(TypeValueInteger::U8(range)))
                            if let Some(integer) = range.value() =>
                        {
                            current.as_mut().statement.as_mut().statement.arg[1] =
                                Value::Literal(Literal::Integer(integer as i128));
                        }
                        Some(TypeValue::Integer(TypeValueInteger::I64(range)))
                            if let Some(integer) = range.value() =>
                        {
                            current.as_mut().statement.as_mut().statement.arg[1] =
                                Value::Literal(Literal::Integer(integer as i128));
                        }
                        Some(TypeValue::Integer(TypeValueInteger::I32(range)))
                            if let Some(integer) = range.value() =>
                        {
                            current.as_mut().statement.as_mut().statement.arg[1] =
                                Value::Literal(Literal::Integer(integer as i128));
                        }
                        Some(TypeValue::Integer(TypeValueInteger::I16(range)))
                            if let Some(integer) = range.value() =>
                        {
                            current.as_mut().statement.as_mut().statement.arg[1] =
                                Value::Literal(Literal::Integer(integer as i128));
                        }
                        _ => todo!(),
                    }

                    // Set next node.
                    debug_assert!(current
                        .as_ref()
                        .next
                        .0
                        .or(current.as_ref().next.1)
                        .is_some());
                    next_state_node = current.as_ref().next.0.or(current.as_ref().next.1);
                }
                _ => todo!(),
            },
            Op::Assembly(Assembly::Svc) => match slice {
                [Value::Literal(Literal::Integer(_))] => {
                    // Set next node.
                    debug_assert!(current
                        .as_ref()
                        .next
                        .0
                        .or(current.as_ref().next.1)
                        .is_some());
                    next_state_node = current.as_ref().next.0.or(current.as_ref().next.1);
                }
                _ => todo!(),
            },
            Op::Special(Special::Unreachable) => {
                assert!(slice.is_empty());
                // As execution ends, we return no valid states.
                debug_assert!(current
                    .as_ref()
                    .next
                    .0
                    .and(current.as_ref().next.1)
                    .is_none());
                // Exits mark the end of execution, their shouldn't be a following node.
                debug_assert_eq!(current.as_ref().next, (None, None));

                // Update syntax node
                debug_assert_eq!(current.as_mut().statement.as_mut().child, None);
                if let Some(next_syntax_node) = current.as_ref().statement.as_ref().next {
                    dealloc_syntax(next_syntax_node);
                    current.as_mut().statement.as_mut().next = None;
                }

                // Set next node
                next_state_node = None;
            }
            Op::Special(Special::SizeOf) => match slice {
                [Value::Variable(_), Value::Variable(sized)] => {
                    let sized_state = current.as_ref().state.get(&TypeKey::from(sized)).unwrap();
                    let sized_type = Type::from(sized_state.clone());
                    current.as_mut().statement.as_mut().statement.arg[1] =
                        Value::Literal(Literal::Integer(sized_type.bytes() as i128));
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    (first_state_node.map(|s| s.as_ref().statement), read)
}

pub unsafe fn finish_optimized_tree(
    new_nodes: NonNull<NewNode>,
    read: HashSet<Variable>,
) -> Option<NonNull<NewNode>> {
    // After iterating through the full AST we now know which variables are used so can remove
    // unused variables
    let mut first = Some(new_nodes);
    let mut stack = vec![new_nodes];
    while let Some(current) = stack.pop() {
        match current.as_ref().statement.op {
            Op::Special(Special::Type) => match current.as_ref().statement.arg.as_slice() {
                [Value::Variable(variable), Value::Type(_), Value::Literal(_)] => {
                    dbg!("hit here");
                    if !read.contains(variable) {
                        dbg!("hit here");
                        println!("bruh: {:?}", current.as_ref().preceding);

                        match current.as_ref().preceding {
                            Some(Preceding::Parent(mut parent)) => {
                                parent.as_mut().child = current.as_ref().next;
                            }
                            Some(Preceding::Previous(mut previous)) => {
                                dbg!("hit this?");
                                println!("bruh: {:?}", previous.as_ref());
                                previous.as_mut().next = current.as_ref().next;
                            }
                            None => {
                                dbg!("hit here");
                                debug_assert_eq!(first, Some(current));
                                // dbg!(&current.as_ref().child.unwrap().as_ref().statement);
                                // dbg!(&current.as_ref().statement);
                                first = current.as_ref().next;
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

    finish_optimized_tree(new_nodes.unwrap(), read).unwrap()
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

#[derive(Debug, Eq, PartialEq, Default, Clone, Hash)]
struct VariableAlias {
    identifier: Identifier,
    index: Option<Box<Index>>,
}
impl From<&str> for VariableAlias {
    fn from(s:&str) -> Self {
        Self {
            identifier: Vec::from(s.as_bytes()),
            index: None
        }
    }
}
impl From<Variable> for VariableAlias {
    fn from(variable: Variable) -> Self {
        Self {
            identifier: variable.identifier,
            index: variable.index,
        }
    }
}
impl From<VariableAlias> for Variable {
    fn from(alias: VariableAlias) -> Self {
        Self {
            addressing: Addressing::Direct,
            identifier: alias.identifier,
            index: alias.index,
        }
    }
}

pub unsafe fn inline_functions(node: NonNull<NewNode>) -> NonNull<NewNode> {
    let mut first = None;

    // The map from function identifiers their first node.
    let mut functions = HashMap::new();

    // An iterator yielding unique identifiers.
    const N: u8 = b'z' - b'a';
    let mut identifier_iterator = (0..).map(|index| {
        (0..index / N)
            .map(|_| b'z')
            .chain(std::iter::once((index % N) + b'a'))
            .collect::<Vec<_>>()
    });


    let mut stack = vec![(
        node,
        None,
        None,
        Rc::new(RefCell::new(HashMap::<VariableAlias, VariableAlias>::new())),
    )];

    while let Some((current, mut preceding, next_carry, variable_map)) = stack.pop() {
        // dbg!(&current.as_ref().statement.op);

        match current.as_ref().statement.op {
            // Function definition
            Op::Intrinsic(Intrinsic::Def) => match current.as_ref().statement.arg.as_slice() {
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                })] => {
                    // dbg!("hit this one");
                    // Insert function definition.
                    let pre_existing =
                        functions.insert(identifier, current.as_ref().child.unwrap());
                    assert!(pre_existing.is_none());

                    if let Some(next) = current.as_ref().next {
                        stack.push((next, preceding, next_carry, variable_map));
                    }
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Assign) => match current.as_ref().statement.arg.as_slice() {
                // Function call
                [Value::Variable(
                    lhs @ Variable {
                        addressing: Addressing::Direct,
                        ..
                    },
                ), Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier: rhs,
                    index: None,
                }), tail @ ..]
                    if let Some(def) = functions.get(rhs) =>
                {
                    debug_assert!(next_carry.is_none());
                    let function = functions.get(rhs).unwrap();

                    let head_iter = std::iter::once({
                        let lhs_alias = VariableAlias::from(lhs.clone());
                        let existing = variable_map.borrow().get(&lhs_alias).cloned();
                        let new_lhs = existing.unwrap_or_else(|| {
                            let new_lhs = VariableAlias {
                                identifier: identifier_iterator.next().unwrap(),
                                index: None,
                            };
                            variable_map
                                .borrow_mut()
                                .insert(lhs_alias.clone(), new_lhs.clone());
                            new_lhs
                        });
                        (
                            VariableAlias {
                                identifier: Vec::from(b"out"),
                                index: None,
                            },
                            new_lhs,
                        )
                    });
                    let tail_iter = tail
                        .iter()
                        .enumerate()
                        .map(|(i, old_value)| {
                            let new_variable_identiier = identifier_iterator.next().unwrap();
                            // Create new node.
                            let dst =
                                alloc::alloc(alloc::Layout::new::<NewNode>()).cast::<NewNode>();
                            std::ptr::write(
                                dst,
                                NewNode {
                                    statement: Statement {
                                        comptime: false,
                                        op: Op::Intrinsic(Intrinsic::Assign),
                                        arg: vec![
                                            Value::Variable(Variable::from(
                                                new_variable_identiier.clone(),
                                            )),
                                            old_value.clone(),
                                        ],
                                    },
                                    preceding,
                                    child: None,
                                    next: None,
                                },
                            );
                            let new = NonNull::new(dst).unwrap();

                            // Update preceding
                            match preceding {
                                Some(Preceding::Previous(mut previous)) => {
                                    debug_assert!(previous.as_ref().next.is_none());
                                    previous.as_mut().next = Some(new);
                                }
                                Some(Preceding::Parent(mut parent)) => {
                                    debug_assert!(parent.as_ref().child.is_none());
                                    parent.as_mut().child = Some(new);
                                }
                                None => {
                                    first = Some(new);
                                }
                            }

                            preceding = Some(Preceding::Previous(new));

                            (
                                VariableAlias {
                                    identifier: Vec::from(b"in"),
                                    index: Some(Box::new(Index::Offset(Offset::Integer(
                                        i as u64,
                                    )))),
                                },
                                VariableAlias::from(new_variable_identiier),
                            )
                    });
                    let new_variable_map =
                        Rc::new(RefCell::new(head_iter.chain(tail_iter).collect()));
                    stack.push((
                        *function,
                        preceding,
                        current.as_ref().next.map(|n| (n, variable_map.clone())),
                        new_variable_map,
                    ));
                }
                [Value::Variable(Variable { addressing: Addressing::Direct, .. }), ..] => {
                    // Create new node.
                    let dst = alloc::alloc(alloc::Layout::new::<NewNode>()).cast::<NewNode>();
                    ptr::copy(current.as_ptr(), dst, 1);
                    let mut new = NonNull::new(dst).unwrap();

                    let [Value::Variable(variable), tail @ ..] =
                        new.as_mut().statement.arg.as_mut_slice()
                    else {
                        unreachable!()
                    };

                    // Update new preceding.
                    match preceding {
                        Some(Preceding::Parent(mut parent)) => {
                            debug_assert!(parent.as_ref().child.is_none());
                            parent.as_mut().child = Some(new);
                        }
                        Some(Preceding::Previous(mut previous)) => {
                            debug_assert!(previous.as_ref().next.is_none());
                            previous.as_mut().next = Some(new);
                        }
                        None => {
                            first = Some(new);
                        }
                    }

                    // Update new variable
                    let new_identifier = identifier_iterator.next().unwrap();
                    let new_variable = VariableAlias {
                        identifier: new_identifier,
                        index: None,
                    };
                    let res = variable_map
                        .borrow_mut()
                        .insert(VariableAlias::from(variable.clone()), new_variable.clone());
                    assert!(res.is_none()); // Assert isn't pre-existing
                    *variable = Variable {
                        addressing: Addressing::Direct,
                        identifier: new_variable.identifier,
                        index: None,
                    };

                    // Update existing variables
                    for tail_variable in tail.iter_mut().filter_map(Value::variable_mut) {
                        
                        let VariableAlias { identifier, index } = variable_map
                            .borrow()
                            .get(&VariableAlias::from(tail_variable.clone()))
                            .unwrap()
                            .clone();
                        *tail_variable = Variable {
                            addressing: tail_variable.addressing.clone(),
                            identifier,
                            index,
                        };
                    }

                    debug_assert!(current.as_ref().child.is_none());
                    if let Some(next) = current.as_ref().next {
                        stack.push((
                            next,
                            Some(Preceding::Previous(new)),
                            next_carry,
                            variable_map,
                        ));
                        new.as_mut().next = None;
                    } else if let Some((next, old_variable_map)) = next_carry {
                        stack.push((next, Some(Preceding::Previous(new)), None, old_variable_map));
                        new.as_mut().next = None;
                    }
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Call) => match current.as_ref().statement.arg.as_slice() {
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier: rhs,
                    index: None,
                }), tail @ ..] => {
                    debug_assert!(next_carry.is_none());
                    let function = functions.get(rhs).unwrap();

                    let iter = tail
                        .iter()
                        .enumerate()
                        .map(|(i, old_value)| match old_value {
                            Value::Literal(literal) => {
                                let new_variable = VariableAlias {
                                    identifier: identifier_iterator.next().unwrap(),
                                    index: None,
                                };

                                // Create new node.
                                let dst =
                                    alloc::alloc(alloc::Layout::new::<NewNode>()).cast::<NewNode>();
                                std::ptr::write(
                                    dst,
                                    NewNode {
                                        statement: Statement {
                                            comptime: false,
                                            op: Op::Intrinsic(Intrinsic::Assign),
                                            arg: vec![
                                                Value::Variable(Variable::from(
                                                    new_variable.clone(),
                                                )),
                                                Value::Literal(literal.clone()),
                                            ],
                                        },
                                        preceding,
                                        child: None,
                                        next: None,
                                    },
                                );
                                let new = NonNull::new(dst).unwrap();

                                // Update preceding
                                match preceding {
                                    Some(Preceding::Previous(mut previous)) => {
                                        debug_assert!(previous.as_ref().next.is_none());
                                        previous.as_mut().next = Some(new);
                                    }
                                    Some(Preceding::Parent(mut parent)) => {
                                        debug_assert!(parent.as_ref().child.is_none());
                                        parent.as_mut().child = Some(new);
                                    }
                                    None => {
                                        first = Some(new);
                                    }
                                }

                                preceding = Some(Preceding::Previous(new));

                                (
                                    VariableAlias {
                                        identifier: Vec::from(b"in"),
                                        index: Some(Box::new(Index::Offset(Offset::Integer(
                                            i as u64,
                                        )))),
                                    },
                                    new_variable,
                                )
                            }
                            Value::Variable(old_variable) => (
                                VariableAlias {
                                    identifier: Vec::from(b"in"),
                                    index: Some(Box::new(Index::Offset(Offset::Integer(i as u64)))),
                                },
                                variable_map
                                    .borrow()
                                    .get(&VariableAlias::from(old_variable.clone()))
                                    .unwrap()
                                    .clone(),
                            ),
                            Value::Type(_) => todo!(),
                            Value::Register(_) => todo!(),
                        });
                    let new_variable_map = Rc::new(RefCell::new(iter.collect()));
                    stack.push((
                        *function,
                        preceding,
                        current.as_ref().next.map(|n| (n, variable_map.clone())),
                        new_variable_map,
                    ));
                }
                _ => todo!(),
            },
            Op::Special(Special::Type) => match current.as_ref().statement.arg.as_slice() {
                [Value::Variable(_), ..] => {
                    // Create new node.
                    let dst = alloc::alloc(alloc::Layout::new::<NewNode>()).cast::<NewNode>();
                    ptr::copy(current.as_ptr(), dst, 1);
                    let mut new = NonNull::new(dst).unwrap();

                    let [Value::Variable(variable), tail @ ..] =
                        new.as_mut().statement.arg.as_mut_slice()
                    else {
                        unreachable!()
                    };

                    // Update new preceding.
                    match preceding {
                        Some(Preceding::Parent(mut parent)) => {
                            debug_assert!(parent.as_ref().child.is_none());
                            parent.as_mut().child = Some(new);
                        }
                        Some(Preceding::Previous(mut previous)) => {
                            debug_assert!(previous.as_ref().next.is_none());
                            previous.as_mut().next = Some(new);
                        }
                        None => {
                            first = Some(new);
                        }
                    }

                    // Update new variable
                    let new_identifier = identifier_iterator.next().unwrap();
                    let new_variable = VariableAlias {
                        identifier: new_identifier,
                        index: None,
                    };
                    let res = variable_map
                        .borrow_mut()
                        .insert(VariableAlias::from(variable.clone()), new_variable.clone());
                    assert!(res.is_none()); // Assert isn't pre-existing
                    *variable = Variable::from(new_variable);

                    // Update existing variables
                    for tail_variable in tail.iter_mut().filter_map(Value::variable_mut) {
                        let VariableAlias { identifier, index } = variable_map
                            .borrow()
                            .get(&VariableAlias::from(tail_variable.clone()))
                            .unwrap()
                            .clone();
                        *tail_variable = Variable {
                            addressing: tail_variable.addressing.clone(),
                            identifier,
                            index,
                        };
                    }

                    // Update child/next.
                    debug_assert!(current.as_ref().child.is_none());
                    if let Some(next) = current.as_ref().next {
                        stack.push((
                            next,
                            Some(Preceding::Previous(new)),
                            next_carry,
                            variable_map,
                        ));
                        new.as_mut().next = None;
                    } else if let Some((next, old_variable_map)) = next_carry {
                        stack.push((next, Some(Preceding::Previous(new)), None, old_variable_map));
                        new.as_mut().next = None;
                    }
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Read) => match current.as_ref().statement.arg.as_slice() {
                [Value::Variable(_), ..] => {
                    // TODO Support case where lhs already exists.

                    // Create new node.
                    let dst = alloc::alloc(alloc::Layout::new::<NewNode>()).cast::<NewNode>();
                    ptr::copy(current.as_ptr(), dst, 1);
                    let mut new = NonNull::new(dst).unwrap();

                    let [Value::Variable(variable), tail @ ..] =
                        new.as_mut().statement.arg.as_mut_slice()
                    else {
                        unreachable!()
                    };

                    // Update new preceding.
                    match preceding {
                        Some(Preceding::Parent(mut parent)) => {
                            debug_assert!(parent.as_ref().child.is_none());
                            parent.as_mut().child = Some(new);
                        }
                        Some(Preceding::Previous(mut previous)) => {
                            debug_assert!(previous.as_ref().next.is_none());
                            previous.as_mut().next = Some(new);
                        }
                        None => {
                            first = Some(new);
                        }
                    }

                    // Update new variable
                    let new_identifier = identifier_iterator.next().unwrap();
                    let new_variable = VariableAlias {
                        identifier: new_identifier,
                        index: None,
                    };
                    let res = variable_map
                        .borrow_mut()
                        .insert(VariableAlias::from(variable.clone()), new_variable.clone());
                    assert!(res.is_none()); // Assert isn't pre-existing
                    *variable = Variable::from(new_variable);

                    // Update existing variables
                    for tail_variable in tail.iter_mut().filter_map(Value::variable_mut) {
                        let VariableAlias { identifier, index } = variable_map
                            .borrow()
                            .get(&VariableAlias::from(tail_variable.clone()))
                            .unwrap()
                            .clone();
                        *tail_variable = Variable {
                            addressing: tail_variable.addressing.clone(),
                            identifier,
                            index,
                        };
                    }

                    // Go next statement.
                    debug_assert!(current.as_ref().child.is_none());
                    if let Some(next) = current.as_ref().next {
                        stack.push((
                            next,
                            Some(Preceding::Previous(new)),
                            next_carry,
                            variable_map,
                        ));
                        new.as_mut().next = None;
                    } else if let Some((next, old_variable_map)) = next_carry {
                        stack.push((next, Some(Preceding::Previous(new)), None, old_variable_map));
                        new.as_mut().next = None;
                    }
                }
                _ => todo!(),
            },
            Op::Special(Special::SizeOf) => match current.as_ref().statement.arg.as_slice() {
                [Value::Variable(_), Value::Variable(_)] => {
                    // TODO Support case where lhs already exists.

                    // Create new node.
                    let dst = alloc::alloc(alloc::Layout::new::<NewNode>()).cast::<NewNode>();
                    ptr::copy(current.as_ptr(), dst, 1);
                    let mut new = NonNull::new(dst).unwrap();

                    let [Value::Variable(lhs), Value::Variable(rhs)] =
                        new.as_mut().statement.arg.as_mut_slice()
                    else {
                        unreachable!()
                    };

                    // Update new preceding.
                    match preceding {
                        Some(Preceding::Parent(mut parent)) => {
                            debug_assert!(parent.as_ref().child.is_none());
                            parent.as_mut().child = Some(new);
                        }
                        Some(Preceding::Previous(mut previous)) => {
                            debug_assert!(previous.as_ref().next.is_none());
                            previous.as_mut().next = Some(new);
                        }
                        None => {
                            first = Some(new);
                        }
                    }

                    // Update new variable
                    let new_identifier = identifier_iterator.next().unwrap();
                    let new_variable = VariableAlias {
                        identifier: new_identifier,
                        index: None,
                    };
                    let res = variable_map
                        .borrow_mut()
                        .insert(VariableAlias::from(lhs.clone()), new_variable.clone());
                    assert!(res.is_none()); // Assert isn't pre-existing
                    *lhs = Variable::from(new_variable);

                    // Update right hande side
                    let VariableAlias { identifier, index } = variable_map
                        .borrow()
                        .get(&VariableAlias::from(rhs.clone()))
                        .unwrap()
                        .clone();
                    *rhs = Variable {
                        addressing: rhs.addressing.clone(),
                        identifier,
                        index,
                    };

                    // Go next statement.
                    debug_assert!(current.as_ref().child.is_none());
                    if let Some(next) = current.as_ref().next {
                        stack.push((
                            next,
                            Some(Preceding::Previous(new)),
                            next_carry,
                            variable_map,
                        ));
                        new.as_mut().next = None;
                    } else if let Some((next, old_variable_map)) = next_carry {
                        stack.push((next, Some(Preceding::Previous(new)), None, old_variable_map));
                        new.as_mut().next = None;
                    }
                }
                x @ _ => todo!("{x:?}"),
            },
            // Else
            _ => {
                // Create new node.
                let dst = alloc::alloc(alloc::Layout::new::<NewNode>()).cast::<NewNode>();
                ptr::copy(current.as_ptr(), dst, 1);
                let mut new = NonNull::new(dst).unwrap();

                // Update new preceding.
                new.as_mut().preceding = preceding;
                match preceding {
                    Some(Preceding::Parent(mut parent)) => {
                        debug_assert!(parent.as_ref().child.is_none());
                        parent.as_mut().child = Some(new);
                    }
                    Some(Preceding::Previous(mut previous)) => {
                        debug_assert!(previous.as_ref().next.is_none());
                        previous.as_mut().next = Some(new);
                    }
                    None => {
                        first = Some(new);
                    }
                }

                // Update existing variables.
                for variable in new
                    .as_mut()
                    .statement
                    .arg
                    .iter_mut()
                    .filter_map(Value::variable_mut)
                {
                    let var = VariableAlias::from(variable.clone());
                    // println!("var: {var:?}");
                    // println!("variable_map: {variable_map:?}");
                    let VariableAlias { identifier, index } =
                        variable_map.borrow().get(&var).unwrap().clone();
                    *variable = Variable {
                        addressing: variable.addressing.clone(),
                        identifier,
                        index,
                    };
                }

                // Update child/next.
                if let Some(child) = current.as_ref().child {
                    stack.push((
                        child,
                        Some(Preceding::Parent(new)),
                        None,
                        variable_map.clone(),
                    ));
                    new.as_mut().child = None;
                }
                if let Some(next) = current.as_ref().next {
                    stack.push((
                        next,
                        Some(Preceding::Previous(new)),
                        next_carry,
                        variable_map,
                    ));
                    new.as_mut().next = None;
                } else if let Some((next, old_variable_map)) = next_carry {
                    stack.push((next, Some(Preceding::Previous(new)), None, old_variable_map));
                    new.as_mut().next = None;
                }
            }
        }
    }

    // Dealloc old tree
    dealloc_syntax(node);

    first.unwrap()
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
        Op::Intrinsic(Intrinsic::If(Cmp::Eq)) => match statement.arg.as_slice() {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let _scope = current_ref.scope;
                let y = current_ref
                    .state
                    .get(&TypeKey::from(variable))
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
        },
        Op::Intrinsic(Intrinsic::If(Cmp::Lt)) => match statement.arg.as_slice() {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let _scope = current_ref.scope;
                let y = current_ref
                    .state
                    .get(&TypeKey::from(variable))
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
        Op::Special(Special::Unreachable) => {
            assert!(statement.arg.is_empty());
            // As execution ends, we return no valid states.
        }
        // See 1 & 2
        _ => {
            let scope = current_ref.scope;
            current_ref.unexplored = (
                new_append(current, scope, ast_node.next.unwrap(), stack),
                Vec::new(),
            );
        }
    }
}

pub unsafe fn close_path(current: NonNull<NewStateNode>) {
    if current.as_ref().unexplored.0.is_empty() && current.as_ref().unexplored.1.is_empty() {
        if current.as_ref().statement.as_ref().statement.op == Op::Special(Special::Unreachable) {
            new_passback_end(current, GraphNodeEnd::Valid);
        } else {
            println!("hit invalid");
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

unsafe fn dealloc_syntax(first: NonNull<NewNode>) {
    let mut stack = vec![first];
    while let Some(current) = stack.pop() {
        if let Some(child) = current.as_ref().child {
            stack.push(child);
        }
        if let Some(next) = current.as_ref().next {
            stack.push(next);
        }
        alloc::dealloc(
            current.as_ptr().cast(),
            alloc::Layout::new::<NewStateNode>(),
        );
    }
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
    let slice = statement.arg.as_slice();
    match statement.op {
        Op::Special(Special::Type) => match slice {
            [Value::Variable(variable), Value::Type(x)] => {
                match state.get(&TypeKey::from(variable)) {
                    // You can't redefine a static.
                    Some(_) => Vec::new(),
                    None => {
                        let mut new_state = state.clone();
                        new_state.insert(TypeKey::from(variable), TypeValue::from(x.clone()));
                        vec![new_state]
                    }
                }
            }
            _ => todo!(),
        },
        Op::Syscall(Syscall::Exit) => match slice {
            // TODO Check the integer fits into i32.
            [Value::Literal(Literal::Integer(_))] => vec![state.clone()],
            // TODO Check the variable fits into i32.
            [Value::Variable(variable)] => match state.get(&TypeKey::from(variable)) {
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
        Op::Intrinsic(Intrinsic::Assign) => match slice {
            [Value::Variable(rhs), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(rhs);
                match state.get(&key) {
                    // Iterates over the set of integer types which could contain `x`, returning a new state for each possibility.
                    None => TypeValueInteger::possible(*x)
                        .into_iter()
                        .map(|p| {
                            let mut new_state = state.clone();
                            new_state.insert(key.clone(), TypeValue::Integer(p));
                            new_state
                        })
                        .collect(),
                    _ => todo!(),
                }
            }
            [Value::Variable(variable), outer @ Value::Literal(Literal::Integer(first)), tail @ ..] =>
            {
                let key = TypeKey::from(variable);
                match state.get(&key) {
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
                                    key.clone(),
                                    TypeValue::Array(TypeValueArray { item, values }),
                                );
                                new_state
                            })
                            .collect()
                    }
                    _ => todo!(),
                }
            }
            [Value::Variable(variable), Value::Literal(Literal::String(s))] => {
                let key = TypeKey::from(variable.clone());
                match state.get(&key) {
                    None => {
                        let mut new_state = state.clone();
                        let values = std::iter::repeat(Type::U8)
                            .zip(s.as_bytes().iter().map(|v| *v as i128))
                            .map(TypeValue::from)
                            .collect::<Vec<_>>();

                        new_state.insert(
                            key.clone(),
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
            [Value::Variable(lhs), Value::Variable(rhs @ Variable { addressing: Addressing::Direct, .. })] => {
                let key = TypeKey::from(lhs.clone());
                let rhs_state = state.get(&TypeKey::from(rhs.clone())).unwrap();
                match state.get(&key) {
                    None => {
                        let mut new_state = state.clone();
                        new_state.insert(key.clone(),rhs_state.clone());
                        vec![new_state]
                    },
                    _ => todo!()
                }
            }
            [Value::Variable(lhs),Value::Variable(rhs @ Variable { addressing: Addressing::Reference, .. })] => {

            }
            x @ _ => todo!("{x:?}"),
        },
        Op::Intrinsic(Intrinsic::SubAssign) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    Some(TypeValue::Integer(y)) => match y.checked_sub(*x) {
                        Ok(z) => {
                            let mut new_state = state.clone();
                            *new_state.get_mut(&key).unwrap().integer_mut().unwrap() = z;
                            vec![new_state]
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::AddAssign) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    Some(TypeValue::Integer(y)) => match y.checked_add_i128(*x) {
                        Ok(z) => {
                            let mut new_state = state.clone();
                            *new_state.get_mut(&key).unwrap().integer_mut().unwrap() = z;
                            vec![new_state]
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::MulAssign) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    Some(TypeValue::Integer(y)) => match y.checked_mul(*x) {
                        Ok(z) => {
                            let mut new_state = state.clone();
                            *new_state.get_mut(&key).unwrap().integer_mut().unwrap() = z;
                            vec![new_state]
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::DivAssign) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    Some(TypeValue::Integer(y)) => match y.checked_div(*x) {
                        Ok(z) => {
                            let mut new_state = state.clone();
                            *new_state.get_mut(&key).unwrap().integer_mut().unwrap() = z;
                            vec![new_state]
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::AndAssign) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    Some(TypeValue::Integer(y)) => match y.checked_and(*x) {
                        Ok(z) => {
                            let mut new_state = state.clone();
                            *new_state.get_mut(&key).unwrap().integer_mut().unwrap() = z;
                            vec![new_state]
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::OrAssign) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    Some(TypeValue::Integer(y)) => match y.checked_or(*x) {
                        Ok(z) => {
                            let mut new_state = state.clone();
                            *new_state.get_mut(&key).unwrap().integer_mut().unwrap() = z;
                            vec![new_state]
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::XorAssign) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    Some(TypeValue::Integer(y)) => match y.checked_xor(*x) {
                        Ok(z) => {
                            let mut new_state = state.clone();
                            *new_state.get_mut(&key).unwrap().integer_mut().unwrap() = z;
                            vec![new_state]
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::If(_)) => match slice {
            [Value::Literal(Literal::Integer(_)), Value::Variable(variable)]
            | [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                match state.get(&TypeKey::from(variable)) {
                    Some(TypeValue::Integer(_)) => vec![state.clone()],
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Special(Special::Require(Cmp::Ge)) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    Some(TypeValue::Integer(y)) => match y.greater_than_or_equal(*x) {
                        true => {
                            let mut new_state = state.clone();
                            let integer = new_state.get_mut(&key).unwrap().integer_mut().unwrap();
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
        Op::Special(Special::Require(Cmp::Le)) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(x))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    Some(TypeValue::Integer(y)) => match y.less_than_or_equal(*x) {
                        true => {
                            let mut new_state = state.clone();
                            let integer = new_state.get_mut(&key).unwrap().integer_mut().unwrap();
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
        Op::Syscall(Syscall::Read) => match slice {
            [Value::Variable(variable), Value::Literal(Literal::Integer(_))] => {
                let key = TypeKey::from(variable);
                match state.get(&key) {
                    // Iterates over the set of integer types which could contain `x`, returning a new state for each possibility.
                    None => TypeValueInteger::any()
                        .into_iter()
                        .map(|p| {
                            let mut new_state = state.clone();
                            new_state.insert(key.clone(), TypeValue::Integer(p));
                            new_state
                        })
                        .collect(),
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        Op::Syscall(Syscall::Write) => match slice {
            [Value::Literal(Literal::Integer(_)), Value::Variable(variable)] => {
                match state.get(&TypeKey::from(variable)) {
                    // All defined values have known sizes so can be written.
                    Some(_) => vec![state.clone()],
                    // You cannot write an undefined value.
                    None => Vec::new(),
                }
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::Loop) => match slice {
            [] => vec![state.clone()],
            [Value::Literal(Literal::Integer(integer))] if u64::try_from(*integer).is_ok() => {
                vec![state.clone()]
            }
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::Break) => match slice {
            [] => vec![state.clone()],
            _ => todo!(),
        },
        Op::Intrinsic(Intrinsic::Add) => match slice {
            [Value::Variable(out), Value::Variable(lhs), Value::Variable(rhs)] => match (
                state.get(&TypeKey::from(out)),
                state.get(&TypeKey::from(lhs)),
                state.get(&TypeKey::from(rhs)),
            ) {
                (
                    Some(TypeValue::Integer(a)),
                    Some(TypeValue::Integer(b)),
                    Some(TypeValue::Integer(c)),
                ) => match a.checked_add(b).and_then(|ab| ab.checked_add(c)) {
                    Ok(z) => {
                        let mut new_state = state.clone();
                        *new_state.get_mut(&TypeKey::from(out)).unwrap() = TypeValue::Integer(z);
                        vec![new_state]
                    }
                    _ => todo!(),
                },
                (None, Some(TypeValue::Integer(b)), Some(TypeValue::Integer(c))) => {
                    match b.checked_add(c) {
                        Ok(z) => {
                            let mut new_state = state.clone();
                            new_state.insert(TypeKey::from(out), TypeValue::Integer(z));
                            vec![new_state]
                        }
                        _ => Vec::new(),
                    }
                }
                _ => todo!(),
            },
            _ => todo!(),
        },
        Op::Assembly(Assembly::Mov) => match slice {
            [Value::Register(register), Value::Literal(Literal::Integer(integer))] => {
                let mut new_state = state.clone();
                let value = if let Ok(unsigned) = u64::try_from(*integer) {
                    TypeValue::Integer(TypeValueInteger::U64(MyRange::from(unsigned)))
                } else if let Ok(signed) = i64::try_from(*integer) {
                    TypeValue::Integer(TypeValueInteger::I64(MyRange::from(signed)))
                } else {
                    todo!()
                };
                new_state.insert(TypeKey::Register(register.clone()), value);
                vec![new_state]
            }
            [Value::Register(register), Value::Variable(variable)] => {
                let mut new_state = state.clone();
                let value = match state.get(&TypeKey::from(variable)) {
                    Some(TypeValue::Integer(TypeValueInteger::I8(range)))
                        if let Some(integer) = range.value() =>
                    {
                        TypeValue::Integer(TypeValueInteger::I64(MyRange::from(i64::from(integer))))
                    }
                    Some(TypeValue::Integer(TypeValueInteger::I16(range)))
                        if let Some(integer) = range.value() =>
                    {
                        TypeValue::Integer(TypeValueInteger::I64(MyRange::from(i64::from(integer))))
                    }
                    Some(TypeValue::Integer(TypeValueInteger::I32(range)))
                        if let Some(integer) = range.value() =>
                    {
                        TypeValue::Integer(TypeValueInteger::I64(MyRange::from(i64::from(integer))))
                    }
                    Some(TypeValue::Integer(TypeValueInteger::I64(range)))
                        if let Some(integer) = range.value() =>
                    {
                        TypeValue::Integer(TypeValueInteger::I64(MyRange::from(i64::from(integer))))
                    }
                    Some(TypeValue::Integer(TypeValueInteger::U8(range)))
                        if let Some(integer) = range.value() =>
                    {
                        TypeValue::Integer(TypeValueInteger::U64(MyRange::from(u64::from(integer))))
                    }
                    Some(TypeValue::Integer(TypeValueInteger::U16(range)))
                        if let Some(integer) = range.value() =>
                    {
                        TypeValue::Integer(TypeValueInteger::U64(MyRange::from(u64::from(integer))))
                    }
                    Some(TypeValue::Integer(TypeValueInteger::U32(range)))
                        if let Some(integer) = range.value() =>
                    {
                        TypeValue::Integer(TypeValueInteger::U64(MyRange::from(u64::from(integer))))
                    }
                    Some(TypeValue::Integer(TypeValueInteger::U64(range)))
                        if let Some(integer) = range.value() =>
                    {
                        TypeValue::Integer(TypeValueInteger::U64(MyRange::from(u64::from(integer))))
                    }
                    _ => todo!(),
                };
                new_state.insert(TypeKey::Register(register.clone()), value);
                vec![new_state]
            }
            _ => todo!(),
        },
        Op::Assembly(Assembly::Svc) => vec![state.clone()],
        Op::Special(Special::Unreachable) => vec![state.clone()],
        Op::Special(Special::SizeOf) => match slice {
            [Value::Variable(rhs), Value::Variable(lhs)] => {
                let key = TypeKey::from(rhs.clone());
                let size = Type::from(state.get(&TypeKey::from(lhs.clone())).unwrap().clone()).bytes();
                match state.get(&key) {
                    // Iterates over the set of integer types which could contain `x`, returning a new state for each possibility.
                    None => TypeValueInteger::possible(size as i128)
                        .into_iter()
                        .map(|p| {
                            let mut new_state = state.clone();
                            new_state.insert(key.clone(), TypeValue::Integer(p));
                            new_state
                        })
                        .collect(),
                    _ => todo!(),
                }
            }
            _ => todo!(),
        },
        _ => todo!(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeValueState(HashMap<TypeKey, TypeValue>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKey {
    Register(Register),
    Variable(VariableAlias),
}

impl From<&str> for TypeKey {
    fn from(s: &str) -> Self {
        Self::Variable(VariableAlias::from(s))
    }
}

impl From<&Variable> for TypeKey {
    fn from(variable: &Variable) -> TypeKey {
        TypeKey::Variable(VariableAlias::from(variable.clone()))
    }
}
impl From<Variable> for TypeKey {
    fn from(variable: Variable) -> TypeKey {
        TypeKey::Variable(VariableAlias::from(variable))
    }
}
impl From<VariableAlias> for TypeKey {
    fn from(variable: VariableAlias) -> TypeKey {
        TypeKey::Variable(variable)
    }
}
impl From<Register> for TypeKey {
    fn from(variable: Register) -> TypeKey {
        TypeKey::Register(variable)
    }
}
impl From<&VariableAlias> for TypeKey {
    fn from(variable: &VariableAlias) -> TypeKey {
        TypeKey::Variable(variable.clone())
    }
}
impl From<&Register> for TypeKey {
    fn from(variable: &Register) -> TypeKey {
        TypeKey::Register(variable.clone())
    }
}

#[allow(dead_code)]
impl TypeValueState {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    fn iter(&self) -> std::collections::hash_map::Iter<'_, TypeKey, TypeValue> {
        self.0.iter()
    }
    fn into_iter(self) -> std::collections::hash_map::IntoIter<TypeKey, TypeValue> {
        self.0.into_iter()
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn get(&self, key: &TypeKey) -> Option<&TypeValue> {
        self.0.get(key)
    }
    fn get_mut(&mut self, key: &TypeKey) -> Option<&mut TypeValue> {
        self.0.get_mut(key)
    }
    fn insert(&mut self, key: TypeKey, value: TypeValue) -> Option<TypeValue> {
        self.0.insert(key, value)
    }
    fn contains_key(&self, key: &TypeKey) -> bool {
        self.0.contains_key(key)
    }
}

impl<const N: usize> From<[(TypeKey, TypeValue); N]> for TypeValueState {
    fn from(arr: [(TypeKey, TypeValue); N]) -> Self {
        Self(HashMap::from(arr))
    }
}

#[derive(Debug, Clone)]
struct TypeState(HashMap<TypeKey, Type>);

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
    fn iter(&self) -> std::collections::hash_map::Iter<'_, TypeKey, Type> {
        self.0.iter()
    }
    fn into_iter(self) -> std::collections::hash_map::IntoIter<TypeKey, Type> {
        self.0.into_iter()
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn get(&self, key: &TypeKey) -> Option<&Type> {
        self.0.get(key)
    }
    fn get_mut(&mut self, key: &TypeKey) -> Option<&mut Type> {
        self.0.get_mut(key)
    }
    fn insert(&mut self, key: TypeKey, value: Type) -> Option<Type> {
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

impl TryFrom<i128> for TypeValue {
    type Error = ();
    fn try_from(x: i128) -> Result<Self, Self::Error> {
        Ok(Self::Integer(TypeValueInteger::try_from(x)?))
    }
}
impl From<i8> for TypeValue {
    fn from(x: i8) -> Self {
        Self::Integer(TypeValueInteger::from(x))
    }
}
impl From<i16> for TypeValue {
    fn from(x: i16) -> Self {
        Self::Integer(TypeValueInteger::from(x))
    }
}
impl From<i32> for TypeValue {
    fn from(x: i32) -> Self {
        Self::Integer(TypeValueInteger::from(x))
    }
}
impl From<i64> for TypeValue {
    fn from(x: i64) -> Self {
        Self::Integer(TypeValueInteger::from(x))
    }
}
impl From<u8> for TypeValue {
    fn from(x: u8) -> Self {
        Self::Integer(TypeValueInteger::from(x))
    }
}
impl From<u16> for TypeValue {
    fn from(x: u16) -> Self {
        Self::Integer(TypeValueInteger::from(x))
    }
}
impl From<u32> for TypeValue {
    fn from(x: u32) -> Self {
        Self::Integer(TypeValueInteger::from(x))
    }
}
impl From<u64> for TypeValue {
    fn from(x: u64) -> Self {
        Self::Integer(TypeValueInteger::from(x))
    }
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

impl TryFrom<i128> for TypeValueInteger {
    type Error = ();
    fn try_from(x: i128) -> Result<Self, Self::Error> {
        Ok(Self::I64(MyRange::from(i64::try_from(x).map_err(drop)?)))

        // if let Ok(x) = u8::try_from(x) {
        //     Ok(Self::U8(MyRange::from(x)))
        // } else if let Ok(x) = u16::try_from(x) {
        //     Ok(Self::U16(MyRange::from(x)))
        // } else if let Ok(x) = u32::try_from(x) {
        //     Ok(Self::U32(MyRange::from(x)))
        // } else if let Ok(x) = u64::try_from(x) {
        //     Ok(Self::U64(MyRange::from(x)))
        // } else if let Ok(x) = i8::try_from(x) {
        //     Ok(Self::I8(MyRange::from(x)))
        // } else if let Ok(x) = i16::try_from(x) {
        //     Ok(Self::I16(MyRange::from(x)))
        // } else if let Ok(x) = i32::try_from(x) {
        //     Ok(Self::I32(MyRange::from(x)))
        // } else if let Ok(x) = i64::try_from(x) {
        //     Ok(Self::I64(MyRange::from(x)))
        // } else {
        //     Err(())
        // }
    }
}

impl From<i8> for TypeValueInteger {
    fn from(x: i8) -> Self {
        Self::I8(MyRange::from(x))
    }
}
impl From<i16> for TypeValueInteger {
    fn from(x: i16) -> Self {
        Self::I16(MyRange::from(x))
    }
}
impl From<i32> for TypeValueInteger {
    fn from(x: i32) -> Self {
        Self::I32(MyRange::from(x))
    }
}
impl From<i64> for TypeValueInteger {
    fn from(x: i64) -> Self {
        Self::I64(MyRange::from(x))
    }
}
impl From<u8> for TypeValueInteger {
    fn from(x: u8) -> Self {
        Self::U8(MyRange::from(x))
    }
}
impl From<u16> for TypeValueInteger {
    fn from(x: u16) -> Self {
        Self::U16(MyRange::from(x))
    }
}
impl From<u32> for TypeValueInteger {
    fn from(x: u32) -> Self {
        Self::U32(MyRange::from(x))
    }
}
impl From<u64> for TypeValueInteger {
    fn from(x: u64) -> Self {
        Self::U64(MyRange::from(x))
    }
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

    fn checked_sub(&self, rhs: i128) -> Result<Self, ()> {
        match self {
            Self::U8(range) => {
                let rhs = MyRange::from(u8::try_from(rhs).map_err(drop)?);
                Ok(Self::U8(range.clone() - rhs))
            }
            Self::U16(range) => {
                let rhs = MyRange::from(u16::try_from(rhs).map_err(drop)?);
                Ok(Self::U16(range.clone() - rhs))
            }
            Self::U32(range) => {
                let rhs = MyRange::from(u32::try_from(rhs).map_err(drop)?);
                Ok(Self::U32(range.clone() - rhs))
            }
            Self::U64(range) => {
                let rhs = MyRange::from(u64::try_from(rhs).map_err(drop)?);
                Ok(Self::U64(range.clone() - rhs))
            }
            Self::I8(range) => {
                let rhs = MyRange::from(i8::try_from(rhs).map_err(drop)?);
                Ok(Self::I8(range.clone() - rhs))
            }
            Self::I16(range) => {
                let rhs = MyRange::from(i16::try_from(rhs).map_err(drop)?);
                Ok(Self::I16(range.clone() - rhs))
            }
            Self::I32(range) => {
                let rhs = MyRange::from(i32::try_from(rhs).map_err(drop)?);
                Ok(Self::I32(range.clone() - rhs))
            }
            Self::I64(range) => {
                let rhs = MyRange::from(i64::try_from(rhs).map_err(drop)?);
                Ok(Self::I64(range.clone() - rhs))
            }
        }
    }

    fn checked_add_i128(&self, rhs: i128) -> Result<Self, ()> {
        match self.clone() {
            Self::U8(a) if let Ok(b) = u8::try_from(rhs) => Ok(Self::U8(a + b)),
            Self::U16(a) if let Ok(b) = u16::try_from(rhs) => Ok(Self::U16(a + b)),
            Self::U32(a) if let Ok(b) = u32::try_from(rhs) => Ok(Self::U32(a + b)),
            Self::U64(a) if let Ok(b) = u64::try_from(rhs) => Ok(Self::U64(a + b)),
            Self::I8(a) if let Ok(b) = i8::try_from(rhs) => Ok(Self::I8(a + b)),
            Self::I16(a) if let Ok(b) = i16::try_from(rhs) => Ok(Self::I16(a + b)),
            Self::I32(a) if let Ok(b) = i32::try_from(rhs) => Ok(Self::I32(a + b)),
            Self::I64(a) if let Ok(b) = i64::try_from(rhs) => Ok(Self::I64(a + b)),
            _ => Err(()),
        }
    }

    fn checked_add(&self, rhs: &Self) -> Result<Self, ()> {
        match (self.clone(), rhs.clone()) {
            (Self::U8(a), Self::U8(b)) => Ok(Self::U8(a + b)),
            (Self::U16(a), Self::U16(b)) => Ok(Self::U16(a + b)),
            (Self::U32(a), Self::U32(b)) => Ok(Self::U32(a + b)),
            (Self::U64(a), Self::U64(b)) => Ok(Self::U64(a + b)),
            (Self::I8(a), Self::I8(b)) => Ok(Self::I8(a + b)),
            (Self::I16(a), Self::I16(b)) => Ok(Self::I16(a + b)),
            (Self::I32(a), Self::I32(b)) => Ok(Self::I32(a + b)),
            (Self::I64(a), Self::I64(b)) => Ok(Self::I64(a + b)),
            _ => Err(()),
        }
    }

    fn checked_mul(&self, rhs: i128) -> Result<Self, ()> {
        match self {
            Self::U8(range) => {
                let rhs = MyRange::from(u8::try_from(rhs).map_err(drop)?);
                Ok(Self::U8(range.clone() * rhs))
            }
            Self::U16(range) => {
                let rhs = MyRange::from(u16::try_from(rhs).map_err(drop)?);
                Ok(Self::U16(range.clone() * rhs))
            }
            Self::U32(range) => {
                let rhs = MyRange::from(u32::try_from(rhs).map_err(drop)?);
                Ok(Self::U32(range.clone() * rhs))
            }
            Self::U64(range) => {
                let rhs = MyRange::from(u64::try_from(rhs).map_err(drop)?);
                Ok(Self::U64(range.clone() * rhs))
            }
            Self::I8(range) => {
                let rhs = MyRange::from(i8::try_from(rhs).map_err(drop)?);
                Ok(Self::I8(range.clone() * rhs))
            }
            Self::I16(range) => {
                let rhs = MyRange::from(i16::try_from(rhs).map_err(drop)?);
                Ok(Self::I16(range.clone() * rhs))
            }
            Self::I32(range) => {
                let rhs = MyRange::from(i32::try_from(rhs).map_err(drop)?);
                Ok(Self::I32(range.clone() * rhs))
            }
            Self::I64(range) => {
                let rhs = MyRange::from(i64::try_from(rhs).map_err(drop)?);
                Ok(Self::I64(range.clone() * rhs))
            }
        }
    }

    fn checked_div(&self, rhs: i128) -> Result<Self, ()> {
        match self {
            Self::U8(range) => {
                let rhs = MyRange::from(u8::try_from(rhs).map_err(drop)?);
                Ok(Self::U8(range.clone().checked_div(rhs).map_err(drop)?))
            }
            Self::U16(range) => {
                let rhs = MyRange::from(u16::try_from(rhs).map_err(drop)?);
                Ok(Self::U16(range.clone().checked_div(rhs).map_err(drop)?))
            }
            Self::U32(range) => {
                let rhs = MyRange::from(u32::try_from(rhs).map_err(drop)?);
                Ok(Self::U32(range.clone().checked_div(rhs).map_err(drop)?))
            }
            Self::U64(range) => {
                let rhs = MyRange::from(u64::try_from(rhs).map_err(drop)?);
                Ok(Self::U64(range.clone().checked_div(rhs).map_err(drop)?))
            }
            Self::I8(range) => {
                let rhs = MyRange::from(i8::try_from(rhs).map_err(drop)?);
                Ok(Self::I8(range.clone().checked_div(rhs).map_err(drop)?))
            }
            Self::I16(range) => {
                let rhs = MyRange::from(i16::try_from(rhs).map_err(drop)?);
                Ok(Self::I16(range.clone().checked_div(rhs).map_err(drop)?))
            }
            Self::I32(range) => {
                let rhs = MyRange::from(i32::try_from(rhs).map_err(drop)?);
                Ok(Self::I32(range.clone().checked_div(rhs).map_err(drop)?))
            }
            Self::I64(range) => {
                let rhs = MyRange::from(i64::try_from(rhs).map_err(drop)?);
                Ok(Self::I64(range.clone().checked_div(rhs).map_err(drop)?))
            }
        }
    }

    fn checked_and(&self, rhs: i128) -> Result<Self, ()> {
        match self {
            Self::U8(range) => {
                let rhs = MyRange::from(u8::try_from(rhs).map_err(drop)?);
                Ok(Self::U8(range.clone() & rhs))
            }
            Self::U16(range) => {
                let rhs = MyRange::from(u16::try_from(rhs).map_err(drop)?);
                Ok(Self::U16(range.clone() & rhs))
            }
            Self::U32(range) => {
                let rhs = MyRange::from(u32::try_from(rhs).map_err(drop)?);
                Ok(Self::U32(range.clone() & rhs))
            }
            Self::U64(range) => {
                let rhs = MyRange::from(u64::try_from(rhs).map_err(drop)?);
                Ok(Self::U64(range.clone() & rhs))
            }
            Self::I8(range) => {
                let rhs = MyRange::from(i8::try_from(rhs).map_err(drop)?);
                Ok(Self::I8(range.clone() & rhs))
            }
            Self::I16(range) => {
                let rhs = MyRange::from(i16::try_from(rhs).map_err(drop)?);
                Ok(Self::I16(range.clone() & rhs))
            }
            Self::I32(range) => {
                let rhs = MyRange::from(i32::try_from(rhs).map_err(drop)?);
                Ok(Self::I32(range.clone() & rhs))
            }
            Self::I64(range) => {
                let rhs = MyRange::from(i64::try_from(rhs).map_err(drop)?);
                Ok(Self::I64(range.clone() & rhs))
            }
        }
    }

    fn checked_or(&self, rhs: i128) -> Result<Self, ()> {
        match self {
            Self::U8(range) => {
                let rhs = MyRange::from(u8::try_from(rhs).map_err(drop)?);
                Ok(Self::U8(range.clone() | rhs))
            }
            Self::U16(range) => {
                let rhs = MyRange::from(u16::try_from(rhs).map_err(drop)?);
                Ok(Self::U16(range.clone() | rhs))
            }
            Self::U32(range) => {
                let rhs = MyRange::from(u32::try_from(rhs).map_err(drop)?);
                Ok(Self::U32(range.clone() | rhs))
            }
            Self::U64(range) => {
                let rhs = MyRange::from(u64::try_from(rhs).map_err(drop)?);
                Ok(Self::U64(range.clone() | rhs))
            }
            Self::I8(range) => {
                let rhs = MyRange::from(i8::try_from(rhs).map_err(drop)?);
                Ok(Self::I8(range.clone() | rhs))
            }
            Self::I16(range) => {
                let rhs = MyRange::from(i16::try_from(rhs).map_err(drop)?);
                Ok(Self::I16(range.clone() | rhs))
            }
            Self::I32(range) => {
                let rhs = MyRange::from(i32::try_from(rhs).map_err(drop)?);
                Ok(Self::I32(range.clone() | rhs))
            }
            Self::I64(range) => {
                let rhs = MyRange::from(i64::try_from(rhs).map_err(drop)?);
                Ok(Self::I64(range.clone() | rhs))
            }
        }
    }

    fn checked_xor(&self, rhs: i128) -> Result<Self, ()> {
        match self {
            Self::U8(range) => {
                let rhs = MyRange::from(u8::try_from(rhs).map_err(drop)?);
                Ok(Self::U8(range.clone() ^ rhs))
            }
            Self::U16(range) => {
                let rhs = MyRange::from(u16::try_from(rhs).map_err(drop)?);
                Ok(Self::U16(range.clone() ^ rhs))
            }
            Self::U32(range) => {
                let rhs = MyRange::from(u32::try_from(rhs).map_err(drop)?);
                Ok(Self::U32(range.clone() ^ rhs))
            }
            Self::U64(range) => {
                let rhs = MyRange::from(u64::try_from(rhs).map_err(drop)?);
                Ok(Self::U64(range.clone() ^ rhs))
            }
            Self::I8(range) => {
                let rhs = MyRange::from(i8::try_from(rhs).map_err(drop)?);
                Ok(Self::I8(range.clone() ^ rhs))
            }
            Self::I16(range) => {
                let rhs = MyRange::from(i16::try_from(rhs).map_err(drop)?);
                Ok(Self::I16(range.clone() ^ rhs))
            }
            Self::I32(range) => {
                let rhs = MyRange::from(i32::try_from(rhs).map_err(drop)?);
                Ok(Self::I32(range.clone() ^ rhs))
            }
            Self::I64(range) => {
                let rhs = MyRange::from(i64::try_from(rhs).map_err(drop)?);
                Ok(Self::I64(range.clone() ^ rhs))
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
            I64_MIN..I32_MIN => vec![Self::I64(MyRange::from(x as i64))],
            I32_MIN..I16_MIN => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::I32(MyRange::from(x as i32)),
            ],
            I16_MIN..I8_MIN => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::I32(MyRange::from(x as i32)),
                Self::I16(MyRange::from(x as i16)),
            ],
            I8_MIN..0 => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::I32(MyRange::from(x as i32)),
                Self::I16(MyRange::from(x as i16)),
                Self::I8(MyRange::from(x as i8)),
            ],
            0..U8_MAX => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::I32(MyRange::from(x as i32)),
                Self::I16(MyRange::from(x as i16)),
                Self::I8(MyRange::from(x as i8)),
                Self::U64(MyRange::from(x as u64)),
                Self::U32(MyRange::from(x as u32)),
                Self::U16(MyRange::from(x as u16)),
                Self::U8(MyRange::from(x as u8)),
            ],
            U8_MAX => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::I32(MyRange::from(x as i32)),
                Self::I16(MyRange::from(x as i16)),
                Self::U64(MyRange::from(x as u64)),
                Self::U32(MyRange::from(x as u32)),
                Self::U16(MyRange::from(x as u16)),
                Self::U8(MyRange::from(x as u8)),
            ],
            U16_EDGE..U16_MAX => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::I32(MyRange::from(x as i32)),
                Self::I16(MyRange::from(x as i16)),
                Self::U64(MyRange::from(x as u64)),
                Self::U32(MyRange::from(x as u32)),
                Self::U16(MyRange::from(x as u16)),
            ],
            U16_MAX => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::I32(MyRange::from(x as i32)),
                Self::U64(MyRange::from(x as u64)),
                Self::U32(MyRange::from(x as u32)),
                Self::U16(MyRange::from(x as u16)),
            ],
            U32_EDGE..U32_MAX => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::I32(MyRange::from(x as i32)),
                Self::U64(MyRange::from(x as u64)),
                Self::U32(MyRange::from(x as u32)),
            ],
            U32_MAX => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::U64(MyRange::from(x as u64)),
                Self::U32(MyRange::from(x as u32)),
            ],
            U64_EDGE..U64_MAX => vec![
                Self::I64(MyRange::from(x as i64)),
                Self::U64(MyRange::from(x as u64)),
            ],
            U64_MAX => vec![Self::U64(MyRange::from(x as u64))],
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

// An inclusive range that supports wrapping around.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MyRange<
    T: Copy + Ord + std::ops::Sub<Output = T> + std::ops::Add<Output = T> + Bounded + Zero + One,
> {
    domain: std::ops::RangeInclusive<T>,
    min: Option<T>,
    abs_min: Option<T>,
    max: Option<T>,
    transform: Vec<Transform<T>>,
}

impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + CheckedDiv,
    > MyRange<T>
{
    fn checked_div(&self, other: Self) -> Result<Self, ()> {
        match (self.transform.is_empty(), other.transform.is_empty()) {
            (true, true)
                if let Some(lhs) = self.value()
                    && let Some(rhs) = other.value() =>
            {
                let sum = lhs.checked_div(&rhs).ok_or(())?;
                Ok(Self {
                    domain: sum..=sum,
                    min: Some(sum),
                    abs_min: Some(sum),
                    max: Some(sum),
                    transform: Vec::new(),
                })
            }
            _ => todo!(),
        }
    }
}

impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + OverflowingAdd,
    > std::ops::Add<Self> for MyRange<T>
{
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self.transform.is_empty(), other.transform.is_empty()) {
            (true, true)
                if let Some(lhs) = self.value()
                    && let Some(rhs) = other.value() =>
            {
                let (sum, _) = lhs.overflowing_add(&rhs);
                Self {
                    domain: sum..=sum,
                    min: Some(sum),
                    abs_min: Some(sum),
                    max: Some(sum),
                    transform: Vec::new(),
                }
            }
            _ => todo!(),
        }
    }
}

impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + OverflowingAdd,
    > std::ops::Add<T> for MyRange<T>
{
    type Output = Self;

    fn add(self, other: T) -> Self {
        match self.transform.is_empty() {
            true if let Some(lhs) = self.value() => {
                let (sum, _) = lhs.overflowing_add(&other);
                Self {
                    domain: sum..=sum,
                    min: Some(sum),
                    abs_min: Some(sum),
                    max: Some(sum),
                    transform: Vec::new(),
                }
            }
            _ => todo!(),
        }
    }
}

impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + OverflowingMul,
    > std::ops::Mul for MyRange<T>
{
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match (self.transform.is_empty(), other.transform.is_empty()) {
            (true, true)
                if let Some(lhs) = self.value()
                    && let Some(rhs) = other.value() =>
            {
                let (sum, _) = lhs.overflowing_mul(&rhs);
                Self {
                    domain: sum..=sum,
                    min: Some(sum),
                    abs_min: Some(sum),
                    max: Some(sum),
                    transform: Vec::new(),
                }
            }
            _ => todo!(),
        }
    }
}

impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + OverflowingSub,
    > std::ops::Sub for MyRange<T>
{
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (self.transform.is_empty(), other.transform.is_empty()) {
            (true, true)
                if let Some(lhs) = self.value()
                    && let Some(rhs) = other.value() =>
            {
                let (sum, _) = lhs.overflowing_sub(&rhs);
                Self {
                    domain: sum..=sum,
                    min: Some(sum),
                    abs_min: Some(sum),
                    max: Some(sum),
                    transform: Vec::new(),
                }
            }
            _ => todo!(),
        }
    }
}

impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + std::ops::BitAnd<Output = T>,
    > std::ops::BitAnd for MyRange<T>
{
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        match (self.transform.is_empty(), other.transform.is_empty()) {
            (true, true)
                if let Some(lhs) = self.value()
                    && let Some(rhs) = other.value() =>
            {
                let sum = lhs & rhs;
                Self {
                    domain: sum..=sum,
                    min: Some(sum),
                    abs_min: Some(sum),
                    max: Some(sum),
                    transform: Vec::new(),
                }
            }
            _ => todo!(),
        }
    }
}

impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + std::ops::BitOr<Output = T>,
    > std::ops::BitOr for MyRange<T>
{
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        match (self.transform.is_empty(), other.transform.is_empty()) {
            (true, true)
                if let Some(lhs) = self.value()
                    && let Some(rhs) = other.value() =>
            {
                let sum = lhs | rhs;
                Self {
                    domain: sum..=sum,
                    min: Some(sum),
                    abs_min: Some(sum),
                    max: Some(sum),
                    transform: Vec::new(),
                }
            }
            _ => todo!(),
        }
    }
}

impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + std::ops::BitXor<Output = T>,
    > std::ops::BitXor for MyRange<T>
{
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        match (self.transform.is_empty(), other.transform.is_empty()) {
            (true, true)
                if let Some(lhs) = self.value()
                    && let Some(rhs) = other.value() =>
            {
                let sum = lhs ^ rhs;
                Self {
                    domain: sum..=sum,
                    min: Some(sum),
                    abs_min: Some(sum),
                    max: Some(sum),
                    transform: Vec::new(),
                }
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Transform<
    T: Copy + Ord + std::ops::Sub<Output = T> + std::ops::Add<Output = T> + Bounded + Zero + One,
> {
    op: TransformOp,
    rhs: MyRange<T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TransformOp {
    Add,
    Sub,
    Div,
    Mul,
}

impl<
        T: Copy + Ord + std::ops::Sub<Output = T> + std::ops::Add<Output = T> + Bounded + Zero + One,
    > From<T> for MyRange<T>
{
    fn from(x: T) -> Self {
        Self {
            domain: x..=x,
            min: Some(x),
            abs_min: Some(x),
            max: Some(x),
            transform: Vec::new(),
        }
    }
}

impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + Signed,
    > MyRange<T>
{
    fn from_signed(domain: std::ops::RangeInclusive<T>) -> MyRange<T> {
        Self {
            domain: domain.clone(),
            min: Some(*domain.start()),
            abs_min: Some(std::cmp::min(
                num_traits::abs(*domain.start()),
                num_traits::abs(*domain.end()),
            )),
            max: Some(*domain.end()),
            transform: Vec::new(),
        }
    }
}
impl<
        T: Copy
            + Ord
            + std::ops::Sub<Output = T>
            + std::ops::Add<Output = T>
            + Bounded
            + Zero
            + One
            + Unsigned,
    > MyRange<T>
{
    fn from_unsigned(domain: std::ops::RangeInclusive<T>) -> MyRange<T> {
        Self {
            domain: domain.clone(),
            min: Some(*domain.start()),
            abs_min: Some(*domain.start()),
            max: Some(*domain.end()),
            transform: Vec::new(),
        }
    }
}

#[allow(dead_code)]
impl<
        T: Copy + Ord + std::ops::Sub<Output = T> + std::ops::Add<Output = T> + Bounded + Zero + One,
    > MyRange<T>
{
    fn len(&self) -> T {
        *self.domain.end() - *self.domain.start()
    }
    fn less_than(&self, x: T) -> bool {
        if let Some(max) = self.max {
            max < x
        } else {
            todo!()
        }
    }
    fn less_than_or_equal(&self, x: T) -> bool {
        if let Some(max) = self.max {
            max <= x
        } else {
            todo!()
        }
    }
    fn greater_than(&self, x: T) -> bool {
        if let Some(min) = self.max {
            min > x
        } else {
            todo!()
        }
    }
    fn greater_than_or_equal(&self, x: T) -> bool {
        if let Some(min) = self.max {
            min >= x
        } else {
            todo!()
        }
    }

    fn contains(&self, x: T) -> bool {
        if self.transform.is_empty() {
            self.domain.contains(&x)
        } else {
            todo!()
        }
    }
    fn excludes(&self, x: T) -> bool {
        if self.transform.is_empty() {
            self.min.unwrap() > x || self.max.unwrap() < x
        } else {
            todo!()
        }
    }
    pub fn any() -> Self {
        Self {
            domain: T::min_value()..=T::max_value(),
            min: Some(T::min_value()),
            abs_min: Some(T::zero()),
            max: Some(T::max_value()),
            transform: Vec::new(),
        }
    }
    fn max(&self) -> T {
        if let Some(max) = self.max {
            max
        } else {
            todo!()
        }
    }
    fn min(&self) -> T {
        if let Some(min) = self.min {
            min
        } else {
            todo!()
        }
    }
    fn set_max(&mut self, x: T) {
        self.max = Some(x);
    }
    fn set_min(&mut self, x: T) {
        self.min = Some(x);
    }
    fn value(&self) -> Option<T> {
        if self.transform.is_empty() && self.domain.start() == self.domain.end() {
            Some(*self.domain.start())
        } else {
            None
        }
    }
}
