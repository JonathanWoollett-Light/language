#![allow(warnings)]

use crate::ast::*;
use itertools::Itertools;
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
use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ptr;
use std::ptr::NonNull;
use std::rc::Rc;
use crate::nonnull;

#[derive(Debug)]
struct ExploreBranch {
    /// Abstract Syntax Node
    asn: NonNull<AstNode>,
    prev: Option<NonNull<TreeEdge>>,
    // None means we haven't set the values to explore, an empty vec means we have explored all values.
    next: Option<Vec<NonNull<TreeEdge>>>,
}

// Deallocates `node` in the abstract syntax tree.
unsafe fn dealloc_asn(node: NonNull<AstNode>) {
    if let Some(pre) = node.as_ref().preceding {
        match pre {
            Preceding::Parent(mut parent) => {
                parent.as_mut().child = node.as_ref().next;
            }
            Preceding::Previous(mut previous) => {
                previous.as_mut().next = node.as_ref().next;
            }
        }
    }
    if let Some(mut next) = node.as_ref().next {
        next.as_mut().preceding = node.as_ref().preceding;
    }
    if let Some(child) = node.as_ref().child {
        dealloc_ast(child);
    }

    dealloc(node.as_ptr().cast(), Layout::new::<AstNode>());
}

/// Inline children from `node` in the abstract syntax tree.
unsafe fn inline_children(mut node: NonNull<AstNode>) {
    if let Some(mut child) = node.as_ref().child {
        child.as_mut().preceding = Some(Preceding::Previous(node));
        let mut front_child = child;
        while let Some(next_child) = front_child.as_ref().next {
            front_child = next_child;
        }
        front_child.as_mut().next = node.as_ref().next;

        if let Some(mut next) = node.as_ref().next {
            next.as_mut().preceding = Some(Preceding::Previous(front_child));
        }

        node.as_mut().next = Some(child);
        node.as_mut().child = None;
    }
}

/// Deallocates the abstract syntax tree starting at `node`.
pub unsafe fn dealloc_ast(node: NonNull<AstNode>) {
    if let Some(pre) = node.as_ref().preceding {
        match pre {
            Preceding::Parent(mut parent) => {
                parent.as_mut().child = None;
            }
            Preceding::Previous(mut previous) => {
                previous.as_mut().next = None;
            }
        }
    }
    let mut stack = vec![node];
    while let Some(current) = stack.pop() {
        // Push to stack.
        if let Some(next) = current.as_ref().next {
            stack.push(next);
        }
        if let Some(child) = current.as_ref().child {
            stack.push(child);
        }
        // Deallocate.
        dealloc(current.as_ptr().cast(), Layout::new::<AstNode>());
    }
}

/// Handles exploring leaves where we know the statement doesn't require a precondition.
unsafe fn handle_no_precond(mut old_leaf_ptr: NonNull<ExploreBranch>) -> NonNull<ExploreBranch> {
    let new_leaf = empty_nonnull();

    let edge = nonnull(TreeEdge {
        precond: None,
        next: TreeEdgeNext::new(new_leaf),
        prev: old_leaf_ptr,
    });

    let old_leaf = old_leaf_ptr.as_mut();
    assert_eq!(old_leaf.asn.as_ref().child, None);

    // Write new leaf node.
    ptr::write(
        new_leaf.as_ptr(),
        ExploreBranch {
            asn: old_leaf.asn.as_ref().next.unwrap(),
            prev: Some(edge),
            next: None,
        },
    );

    // Write new branch node.
    assert_eq!(old_leaf.next, None);
    old_leaf.next = Some(vec![edge]);

    return new_leaf;
}

#[derive(Debug)]
struct TreeEdge {
    /// A pre-conditional statement applied to `from` to narrow ambiguity.
    ///
    /// When exploring the source:
    /// ```text
    /// x = 2
    /// ...
    /// ```
    /// the paths where `x` is multiple different types will need to be explored, thus there will be
    /// multiple edges with the pre-conditional statements `x:u8`, `x:u16`, `x:i8` etc.
    precond: Option<Statement>,
    next: TreeEdgeNext,
    prev: NonNull<ExploreBranch>,
}

// If a conditional statement may be true or false, both paths need to be explored, but they
// should be explored in-order, e.g. the statements within the `if` should be explored first.
#[derive(Debug)]
struct TreeEdgeNext {
    next: Option<NonNull<ExploreBranch>>,
    child: Option<NonNull<ExploreBranch>>,
}
impl TreeEdgeNext {
    fn new(next: NonNull<ExploreBranch>) -> Self {
        Self {
            next: Some(next),
            child: None,
        }
    }
}

#[derive(Debug)]
pub struct Explorer {
    stack: Vec<NonNull<ExploreBranch>>,
    front: NonNull<ExploreBranch>,
}

enum Link {
    Type(Type),
    Variable(Identifier),
}

#[derive(Debug)]
enum Condition {
    True,
    False,
    Either,
}
impl From<bool> for Condition {
    fn from(x: bool) -> Self {
        match x {
            true => Condition::True,
            false => Condition::False,
        }
    }
}

/// This type will get very complicated but for now for the simple problems its not so bad.
#[derive(Debug)]
struct VariableValue {
    max: Option<i128>,
    min: Option<i128>,
    cast_type: Option<Type>,
}
impl From<&Value> for VariableValue {
    fn from(value: &Value) -> Self {
        match value {
            Value::Literal(Literal::Integer(x)) => Self {
                max: Some(*x),
                min: Some(*x),
                cast_type: None,
            },
            Value::Variable(Variable {
                addressing: Addressing::Direct,
                identifier,
                index: None,
                cast: None,
            }) => Self {
                max: None,
                min: None,
                cast_type: None,
            },
            _ => todo!(),
        }
    }
}

unsafe fn check_condition(lhs: &VariableValue, cmp: &Cmp, rhs: &VariableValue) -> Condition {
    match cmp {
        Cmp::Eq if lhs.max == lhs.min && rhs.max == rhs.min => Condition::from(rhs.max == lhs.max),
        _ => todo!(),
    }
}

unsafe fn update_value(statement: &Statement, value: &Value, variable_value: &mut VariableValue) {
    let slice = statement.arg.as_slice();

    let ident = match value {
        Value::Literal(Literal::Integer(x)) => {
            variable_value.max = Some(*x);
            variable_value.min = Some(*x);
            return;
        }
        Value::Variable(Variable {
            addressing: Addressing::Direct,
            identifier,
            index: None,
            cast: _,
        }) => identifier,
        _ => todo!(),
    };

    match statement.op {
        Op::Assign => match slice {
            [Value::Variable(Variable {
                addressing: Addressing::Direct,
                identifier,
                index: None,
                cast: _,
            }), Value::Literal(Literal::Integer(x))] => {
                if identifier == ident {
                    variable_value.max = Some(*x);
                    variable_value.min = Some(*x);
                }
            }
            [Value::Variable(Variable {
                addressing: Addressing::Direct,
                identifier,
                index: None,
                cast: Some(Cast::As(cast_type)),
            })] => variable_value.cast_type = Some(cast_type.clone()),
            x @ _ => todo!("{x:?}"),
        },
        _ => todo!(),
    }
}

unsafe fn evaluate_condition(
    mut edge_opt: Option<NonNull<TreeEdge>>,
    cmp: &Cmp,
    lhs: &Value,
    rhs: &Value,
) -> Condition {
    let mut lhs_value = VariableValue::from(lhs);
    let mut rhs_value = VariableValue::from(rhs);

    let mut i = 0;
    while let Some(edge) = edge_opt {
        assert!(i < 50);

        let branch = edge.as_ref().prev;
        let asn_statement = &branch.as_ref().asn.as_ref().statement;

        update_value(asn_statement, &lhs, &mut lhs_value);
        update_value(asn_statement, &rhs, &mut rhs_value);
        match check_condition(&lhs_value, &cmp, &rhs_value) {
            Condition::True => return Condition::True,
            Condition::False => return Condition::False,
            Condition::Either => {}
        }

        if let Some(precond) = &edge.as_ref().precond {
            update_value(precond, &lhs, &mut lhs_value);
            update_value(precond, &rhs, &mut rhs_value);
            match check_condition(&lhs_value, &cmp, &rhs_value) {
                Condition::True => return Condition::True,
                Condition::False => return Condition::False,
                Condition::Either => {}
            }
        }

        edge_opt = branch.as_ref().prev;
        i += 1;
    }
    Condition::Either
}

/// Finds the type for a variable by searching backwards through the exploration tree.
unsafe fn find_variable_cast(start: NonNull<TreeEdge>, identifier: &Identifier) -> Option<Type> {
    fn check_map(map: &HashMap<Identifier, Link>, mut ident: Identifier) -> Option<Type> {
        let mut i = 0;
        loop {
            assert!(i < 50);
            ident = match map.get(&ident) {
                None => break None,
                Some(Link::Type(cast_type)) => break Some(cast_type.clone()),
                Some(Link::Variable(variable_type)) => variable_type.clone(),
            };
            i += 1;
        }
    }
    unsafe fn type_statement(statement: &Statement, ident: &Identifier, map: &mut HashMap<Identifier, Link>) {
        let slice = statement.arg.as_slice();
        match &statement.op {
            Op::Assign => match slice {
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: Some(Cast::As(cast_type)),
                })] => {
                    map.insert(identifier.clone(), Link::Type(cast_type.clone()));
                }
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier: lhs_identifier,
                    index: None,
                    cast: None,
                }), Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier: rhs_identifier,
                    index: None,
                    cast: None,
                })] => {
                    map.insert(lhs_identifier.clone(), Link::Variable(rhs_identifier.clone()));
                }
                [Value::Variable(_), Value::Literal(_)] => {}
                _ => todo!(),
            },
            Op::Mov => {}
            Op::Svc => {}
            Op::Unreachable => {}
            Op::AddAssign => match slice {
                [Value::Variable(_), Value::Literal(Literal::Integer(_))] => {}
                _ => todo!(),
            },
            Op::If(_) => {}
            x @ _ => todo!("{x:?}"),
        }
    }

    let mut edge_opt = Some(start);
    let mut map = HashMap::new();

    let mut i = 0;
    while let Some(edge) = edge_opt {
        assert!(i < 50);
        if let Some(precond) = &edge.as_ref().precond {
            type_statement(precond, identifier, &mut map);
            if let Some(t) = check_map(&map, identifier.clone()) {
                return Some(t);
            }
        }

        let branch = edge.as_ref().prev;
        type_statement(&branch.as_ref().asn.as_ref().statement, identifier, &mut map);
        if let Some(t) = check_map(&map, identifier.clone()) {
            return Some(t);
        }
        edge_opt = branch.as_ref().prev;
        i += 1;
    }
    None
}

/// When no infomation is known about a variable we explore the possible types returned here.
fn default_possible_types() -> Vec<Type> {
    use IntegerType::*;
    vec![I64, I32, I16, I8, U64, U32, U16, U8].into_iter().map(Type::Integer).collect()
}

/// For a given integer value return all types that can contain it.
fn possible_integer_types(x: i128) -> Vec<Type> {
    use IntegerType::*;
    const I64_MIN: i128 = i64::MIN as i128;
    const I32_MIN: i128 = i32::MIN as i128;
    const I16_MIN: i128 = i16::MIN as i128;
    const I8_MIN: i128 = i8::MIN as i128;
    const I8_EDGE: i128 = i8::MAX as i128 + 1;
    const U8_EDGE: i128 = u8::MAX as i128 + 1;
    const U64_EDGE: i128 = u64::MAX as i128 + 1;
    const U32_EDGE: i128 = u32::MAX as i128 + 1;
    const U16_EDGE: i128 = u16::MAX as i128 + 1;
    const I16_EDGE: i128 = i16::MAX as i128 + 1;
    const I32_EDGE: i128 = i32::MAX as i128 + 1;
    const I64_EDGE: i128 = i64::MAX as i128 + 1;

    // The ordering matters, the best (by performance and memory usage) types are last in the vec.
    match x {
        I64_MIN..I32_MIN => vec![I64],
        I32_MIN..I16_MIN => vec![I64, I32],
        I16_MIN..I8_MIN => vec![I64, I32, I16],
        I8_MIN..0 => vec![I64, I32, I16, I8],
        0..I8_EDGE => vec![I64, I32, I16, I8, U64, U32, U16, U8],
        I8_EDGE..U8_EDGE => vec![I64, I32, I16, U64, U32, U16, U8],
        U8_EDGE..I16_EDGE => vec![I64, I32, I16, U64, U32, U16],
        I16_EDGE..U16_EDGE => vec![I64, I32, U64, U32, U16],
        U16_EDGE..I32_EDGE => vec![I64, I32, U64, U32],
        I32_EDGE..U32_EDGE => vec![I64, U64, U32],
        U32_EDGE..I64_EDGE => vec![I64, U64],
        I64_EDGE..U64_EDGE => vec![U64],
        _ => panic!(),
    }.into_iter().map(Type::Integer).collect()
}

pub enum ExplorationResult {
    Continue(Explorer),
    Done(NonNull<AstNode>),
}

unsafe fn explore_types(possible: Vec<Type>, mut leaf_ptr: NonNull<ExploreBranch>, lhs: &Variable) -> Vec<NonNull<ExploreBranch>> {
    // For each possible cast, create a leaf node.
    let (next, new_leaves) = possible
        .into_iter()
        .map(|p| {
            // Allocate new leaf.
            let new_leaf = empty_nonnull();

            // Create precondition.
            let precond = Statement {
                op: Op::Assign,
                arg: vec![Value::Variable(Variable {
                    cast: Some(Cast::As(Type::from(p))),
                    ..lhs.clone()
                })],
            };

            // Create edge.
            let edge = nonnull(TreeEdge {
                precond: Some(precond),
                next: TreeEdgeNext::new(new_leaf),
                prev: leaf_ptr,
            });

            // Allocate new leaf.
            ptr::write(
                new_leaf.as_ptr(),
                ExploreBranch {
                    asn: leaf_ptr.as_ref().asn.as_ref().next.unwrap(),
                    prev: Some(edge),
                    next: None,
                },
            );

            // Return branch next.
            (edge, new_leaf)
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    assert_eq!(leaf_ptr.as_ref().next, None);
    leaf_ptr.as_mut().next = Some(next);

    new_leaves
}

impl Explorer {
    pub unsafe fn new(root: NonNull<AstNode>) -> Self {
        let ptr = alloc(Layout::new::<ExploreBranch>()).cast();
        ptr::write(
            ptr,
            ExploreBranch {
                asn: root,
                prev: None,
                next: None,
            },
        );
        let nonnull = NonNull::new(ptr).unwrap();
        Self {
            stack: vec![nonnull],
            front: nonnull,
        }
    }

    pub unsafe fn next(mut self) -> ExplorationResult {
        let Some(mut leaf_ptr) = self.stack.pop() else { todo!() };

        let leaf = leaf_ptr.as_mut();
        let statement = &leaf.asn.as_ref().statement;
        let slice = statement.arg.as_slice();

        // eprintln!("\n\n{}\n\n", display_ast(self.front));
        eprintln!("statement: {statement}");

        match &statement.op {
            Op::Assign => {
                assert_eq!(leaf.asn.as_ref().child, None);
                match slice {
                    [Value::Variable(
                        lhs @ Variable {
                            addressing: Addressing::Direct,
                            identifier,
                            ..
                        },
                    ), Value::Literal(Literal::Integer(x))] => {
                        // Gets the type the variable has been previously cast as.
                        let cast_opt = match &lhs.cast {
                            Some(Cast::As(cast_type)) => Some(cast_type.clone()),
                            Some(Cast::Prev) => todo!(),
                            None => leaf.prev.and_then(|p| find_variable_cast(p, identifier)),
                        };
                        // Based on the variables casted type
                        let new_leaves = match cast_opt {
                            // This will only arrise when the variable hasn't been previously defined,
                            // in this case we need to explore possible types.
                            None => explore_types(possible_integer_types(*x), leaf_ptr, lhs),
                            // TODO: Check if the assigned value fits in the cast type, if it does do
                            // nothing, if it doesn't add to the stack to explore the types it could fit in
                            Some(cast) => todo!(),
                        };
                        self.stack.extend(new_leaves);
                    }
                    [Value::Variable(
                        lhs @ Variable {
                            addressing: Addressing::Direct,
                            identifier: lhs_identifier,
                            ..
                        },
                    ), Value::Variable(
                        Variable {
                            addressing: rhs_addressing,
                            identifier: rhs_identifier,
                            ..
                        },
                    )] => {
                        let cast_opt = match &lhs.cast {
                            Some(Cast::As(cast_type)) => Some(cast_type.clone()),
                            Some(Cast::Prev) => todo!(),
                            None => leaf
                                .prev
                                .and_then(|p| find_variable_cast(p, rhs_identifier))
                                .map(|rhs_type| match rhs_addressing {
                                    Addressing::Direct => rhs_type,
                                    Addressing::Reference => Type::Reference(Box::new(rhs_type)),
                                    Addressing::Dereference => todo!(),
                                }),
                        };

                        // Based on the variables casted type
                        let new_leaves = match cast_opt {
                            // This will only arrise when the variable hasn't been previously defined,
                            // in this case we need to explore possible types.
                            None => explore_types(default_possible_types(), leaf_ptr, lhs),
                            Some(cast) => vec![handle_no_precond(leaf_ptr)],
                        };

                        self.stack.extend(new_leaves);
                    }
                    x @ _ => todo!("{x:?}"),
                }
                ExplorationResult::Continue(self)
            }
            Op::Mov => {
                assert_eq!(leaf.asn.as_ref().child, None);
                let new_leaf = match slice {
                    [Value::Register(_), Value::Literal(_)] => handle_no_precond(leaf_ptr),
                    [Value::Register(_), Value::Variable(Variable {
                        addressing: Addressing::Direct,
                        index: None,
                        cast: None,
                        ..
                    })] => handle_no_precond(leaf_ptr),
                    _ => todo!(),
                };
                self.stack.push(new_leaf);
                ExplorationResult::Continue(self)
            }
            Op::Unreachable => {
                assert_eq!(slice, &[]);

                // All nodes after an `unreachable` are unreachable so they can be removed.
                assert_eq!(leaf.asn.as_ref().child, None);
                if let Some(next) = leaf.asn.as_ref().next {
                    dealloc_ast(next);
                }

                // To get this working, the cost function is 1 or 0. The 1st valid complete path
                // found will be used.

                // Backpropagate through the exploration tree
                //
                // When looking at a parent if this is the next node of a tree edge, we know this
                // whole edge has been evalauted as the child will always be evaluated first (since it is inserted
                // before the next node in `stack` in `Explorer`).
                //
                // Since we are only looking for the 1st valid path, if this is the next node and
                // this full edge has been evaluated. Remove the unexplored edges from `next` in
                // `ExploreBranch`, the nodes under these edges will also need to be removed from `stack` in `Explorer`.
                let mut current_leaf_ptr = leaf_ptr;
                while let Some(edge_ptr) = current_leaf_ptr.as_ref().prev {
                    let edge = edge_ptr.as_ref();
                    match edge.next {
                        // This is the child node of its parent and there is no next. So we can backpropagate.
                        TreeEdgeNext {
                            next: None,
                            child: Some(edge_child),
                        } => {
                            assert_eq!(edge_child, leaf_ptr);
                            todo!("backprop");
                        }
                        // This is the child node of its parent. This means the next node has not yet been explored, so we can't backprop further.
                        TreeEdgeNext {
                            next: Some(edge_next),
                            child: Some(edge_child),
                        } if edge_child == leaf_ptr => {
                            assert_ne!(edge_child, edge_next);
                            return ExplorationResult::Continue(self);
                        }
                        // This is the next node of its parent. This means the child node has been explored, so we can backpropagate.
                        TreeEdgeNext {
                            next: Some(edge_next),
                            child: Some(edge_child),
                        } if edge_next == leaf_ptr => {
                            assert_ne!(edge_child, edge_next);
                            todo!("backprop");
                        }
                        TreeEdgeNext {
                            next: Some(_),
                            child: Some(_),
                        } => unreachable!(),
                        // This is the next node of its parent and there is no child. So we can backpropagate.
                        TreeEdgeNext {
                            next: Some(edge_next),
                            child: None,
                        } => {
                            // assert_eq!(edge_next, leaf_ptr);
                            let mut parent_ptr = edge.prev;
                            let parent = parent_ptr.as_mut();

                            // All edges that are invalid are removed from the next list, and edges
                            // are explored in reverse order they are in the next list.
                            // So here the last edge in the next list will be this edge.
                            let parent_nexts = parent.next.as_mut().unwrap();
                            assert_eq!(parent_nexts.last(), Some(&edge_ptr));

                            // We pick the 1st valid discovered edge. So here we discard the
                            // unexplored edges, removing them from the next list, removing their
                            // leaf nodes from the stack and deallocating these.
                            for unexplored_edge_ptr in parent_nexts.drain(..parent_nexts.len() - 1).rev() {
                                let unexplored_edge = unexplored_edge_ptr.as_ref();
                                // assert_eq!(unexplored_edge.precond, None);
                                assert_eq!(unexplored_edge.prev, parent_ptr);

                                match unexplored_edge.next {
                                    TreeEdgeNext {
                                        next: Some(unexplored_next),
                                        child: Some(unexplored_child),
                                    } => {
                                        assert_eq!(self.stack.pop(), Some(unexplored_child));
                                        dealloc(unexplored_child.as_ptr().cast(), Layout::new::<ExploreBranch>());
                                        assert_eq!(self.stack.pop(), Some(unexplored_next));
                                        dealloc(unexplored_next.as_ptr().cast(), Layout::new::<ExploreBranch>());
                                    }
                                    TreeEdgeNext {
                                        next: None,
                                        child: Some(unexplored_child),
                                    } => {
                                        assert_eq!(self.stack.pop(), Some(unexplored_child));
                                        dealloc(unexplored_child.as_ptr().cast(), Layout::new::<ExploreBranch>());
                                    }
                                    TreeEdgeNext {
                                        next: Some(unexplored_next),
                                        child: None,
                                    } => {
                                        assert_eq!(self.stack.pop(), Some(unexplored_next));
                                        dealloc(unexplored_next.as_ptr().cast(), Layout::new::<ExploreBranch>());
                                    }
                                    // Unexplored nodes will only contain `BranchNode::Leaf`s and
                                    // there will be atleast a next or child otherwise the edge
                                    // would have never been created in the first place.
                                    _ => unreachable!(),
                                }

                                dealloc(unexplored_edge_ptr.as_ptr().cast(), Layout::new::<TreeEdge>());
                            }

                            current_leaf_ptr = parent_ptr;
                        }
                        TreeEdgeNext {
                            next: None,
                            child: None,
                        } => unreachable!(),
                    }
                }
                // If we reach an edge with no precursors in backprop we have a full tree.
                ExplorationResult::Done(self.resolve())
            }
            // This should be removed when inlining so should never be encountered here.
            Op::Def => unreachable!(),
            Op::Svc => match slice {
                [Value::Literal(Literal::Integer(0))] => {
                    let new_leaf = handle_no_precond(leaf_ptr);
                    self.stack.push(new_leaf);
                    ExplorationResult::Continue(self)
                }
                _ => todo!(),
            },
            Op::AddAssign => match slice {
                [Value::Variable(
                    lhs @ Variable {
                        addressing: Addressing::Direct,
                        identifier,
                        ..
                    },
                ), Value::Literal(Literal::Integer(x))] => {
                    // Gets the type the variable has been previously cast as.
                    let cast_opt = match &lhs.cast {
                        Some(Cast::As(cast_type)) => Some(cast_type.clone()),
                        Some(Cast::Prev) => todo!(),
                        None => leaf.prev.and_then(|p| find_variable_cast(p, identifier)),
                    }
                    .unwrap();

                    let possible = possible_integer_types(*x);
                    match cast_opt.integer().map(|t| possible.iter().any(|p| p == t)) {
                        Some(true) => {
                            let new_leaf = handle_no_precond(leaf_ptr);
                            self.stack.push(new_leaf);
                        }
                        Some(false) | None => todo!(),
                    }
                    ExplorationResult::Continue(self)
                }
                [Value::Variable(
                    lhs @ Variable {
                        addressing: Addressing::Direct,
                        identifier: lhs_identifier,
                        ..
                    },
                ), Value::Variable(
                    Variable {
                        addressing: Addressing::Direct,
                        identifier: rhs_identifier,
                        ..
                    },
                )] => {
                    // TODO: If this fails at some point maybe it should be
                    // `find_variable_cast(p, lhs_identifier)`.
                    //
                    // There will be a type at this point, we can check this but it doesn't
                    // require any pre-conditional statement.
                    assert!(match &lhs.cast {
                        Some(Cast::As(cast_type)) => Some(cast_type.clone()),
                        Some(Cast::Prev) => todo!(),
                        None => leaf.prev.and_then(|p| find_variable_cast(p, rhs_identifier)),
                    }
                    .is_some());

                    let new_leaf = handle_no_precond(leaf_ptr);
                    self.stack.push(new_leaf);
                    ExplorationResult::Continue(self)
                }
                _ => todo!(),
            },
            Op::If(cmp) => match slice {
                [lhs, rhs] => {
                    let cond = evaluate_condition(leaf.prev, cmp, lhs, rhs);
                    let new_leaves = match cond {
                        Condition::Either => todo!(),
                        Condition::True => {
                            inline_children(leaf.asn);
                            let if_asn = leaf.asn;
                            // It is okay to unwrap, as a program cannot end in an `if` and thus
                            // must have following statements.
                            leaf.asn = if_asn.as_ref().next.unwrap();
                            dealloc_asn(if_asn);
                            vec![leaf_ptr]
                        }
                        Condition::False => {
                            let if_asn = leaf.asn;
                            // It is okay to unwrap, as a program cannot end in an `if` and thus
                            // must have following statements.
                            leaf.asn = if_asn.as_ref().next.unwrap();
                            dealloc_asn(if_asn);
                            vec![leaf_ptr]
                        }
                    };
                    self.stack.extend(new_leaves);
                    ExplorationResult::Continue(self)
                }
                _ => todo!(),
            },
            Op::TypeOf => match slice {
                [Value::Variable(
                    Variable {
                        addressing: Addressing::Direct,
                        identifier: lhs_identifier,
                        cast: lhs_cast,
                        ..
                    },
                ), Value::Variable(
                    rhs @ Variable {
                        addressing: rhs_addressing,
                        identifier: rhs_identifier,
                        cast: rhs_cast,
                        ..
                    },
                )] => {
                    // The type of the left-hande-side can only be a `Type` type.
                    assert!(match lhs_cast {
                        Some(Cast::As(cast_type)) => match cast_type {
                            Type::TypeType => true,
                            _ => false,
                        },
                        Some(Cast::Prev) => false,
                        None => true
                    });

                    let rhs_cast_opt = match &rhs_cast {
                        Some(Cast::As(cast_type)) => Some(match rhs_addressing {
                            Addressing::Direct => cast_type.clone(),
                            // You can only cast a reference to a reference of the equivalent depth
                            // e.g. you can cast `&&u8` to `&&i8` but not to `i8` or `&i8`.
                            Addressing::Reference => todo!(),
                            Addressing::Dereference => todo!(),
                        }),
                        Some(Cast::Prev) => todo!(),
                        None => leaf
                            .prev
                            .and_then(|p| find_variable_cast(p, rhs_identifier))
                            .map(|rhs_type| match rhs_addressing {
                                Addressing::Direct => rhs_type,
                                Addressing::Reference => Type::Reference(Box::new(rhs_type)),
                                Addressing::Dereference => rhs_type.reference().unwrap().as_ref().clone(),
                            }),
                    };

                    // Based on the variables casted type
                    let new_leaves = match rhs_cast_opt {
                        // This will only arrise when the variable hasn't been previously defined,
                        // in this case we need to explore possible types.
                        None => explore_types(default_possible_types(), leaf_ptr, rhs),
                        Some(cast) => vec![handle_no_precond(leaf_ptr)],
                    };

                    self.stack.extend(new_leaves);
                    ExplorationResult::Continue(self)
                }
                x @ _ => todo!("{x:?}"),
            }
            x @ _ => todo!("{x:?}"),
        }
    }

    unsafe fn resolve(self) -> NonNull<AstNode> {
        let Self { stack, front } = self;
        assert_eq!(stack, []);
        let front_ast = front.as_ref().asn;
        let mut stack = vec![front];
        while let Some(mut next_ptr) = stack.pop() {
            let mut next = next_ptr.as_mut();

            let Some(mut edges) = next.next.as_mut() else {
                assert_eq!(next.asn.as_ref().statement.op, Op::Unreachable);
                continue;
            };
            let edge_ptr = edges.pop().unwrap();
            assert_eq!(edges, &[]);

            // If there is a preconditional statement, insert this into the AST.
            if let Some(precond) = &edge_ptr.as_ref().precond {
                let mut asn_ptr = next.asn;
                let new_asn_ptr = alloc(Layout::new::<AstNode>()).cast();
                ptr::write(
                    new_asn_ptr,
                    AstNode {
                        statement: precond.clone(),
                        preceding: asn_ptr.as_ref().preceding,
                        child: None,
                        next: Some(asn_ptr),
                    },
                );
                let new_asn_nonnull = NonNull::new(new_asn_ptr).unwrap();
                // If there is a preceding existing AST node, update this to point to the preconditional node.
                if let Some(pre) = asn_ptr.as_ref().preceding {
                    match pre {
                        Preceding::Parent(mut parent) => {
                            parent.as_mut().child = Some(new_asn_nonnull);
                        }
                        Preceding::Previous(mut previous) => {
                            previous.as_mut().next = Some(new_asn_nonnull);
                        }
                    }
                }
                // Update current AST node to point to preconditional node as preceding.
                asn_ptr.as_mut().preceding = Some(Preceding::Previous(new_asn_nonnull));
            }

            let next_edge = &edge_ptr.as_ref().next;
            if let Some(n) = next_edge.next {
                stack.push(n);
            }
            if let Some(c) = next_edge.child {
                stack.push(c);
            }
        }
        // If precondition was inserted before it might be new front
        if let Some(pre) = front_ast.as_ref().preceding {
            let Preceding::Previous(new_front) = pre else {
                unreachable!()
            };
            new_front
        } else {
            front_ast
        }
    }
}

unsafe fn empty_nonnull<T>() -> NonNull<T> {
    NonNull::new(alloc(Layout::new::<T>()).cast::<T>()).unwrap()
}

