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

#[derive(Debug)]
struct ExploreBranch {
    /// Abstract Syntax Node
    asn: NonNull<NewNode>,
    prev: Option<NonNull<TreeEdge>>,
    // None means we haven't set the values to explore, an empty vec means we have explored all values.
    next: Option<Vec<NonNull<TreeEdge>>>,
}

/// Handles exploring leaves where we know the statement doesn't require a precondition.
unsafe fn handle_no_precond(mut old_leaf: NonNull<ExploreBranch>) -> NonNull<ExploreBranch> {
    let new_leaf = empty_nonnull();

    let edge = nonnull(TreeEdge {
        precond: None,
        next: TreeEdgeNext::new(new_leaf),
        prev: old_leaf,
    });

    // Write new leaf node.
    ptr::write(
        new_leaf.as_ptr(),
        ExploreBranch {
            asn: old_leaf.as_ref().asn.as_ref().next.unwrap(),
            prev: Some(edge),
            next: None,
        },
    );

    // Write new branch node.
    assert_eq!(old_leaf.as_ref().next, None);
    old_leaf.as_mut().next = Some(vec![edge]);

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

/// Finds the type for a variable by searching backwards through the exploration tree.
unsafe fn find_variable_cast(start: NonNull<TreeEdge>) -> Option<Type> {
    todo!()
}

enum IntegerType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}

// For a given integer value return all types that can contain it.
fn containing_types(x: i128) -> Vec<IntegerType> {
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
    }
}

impl From<IntegerType> for Type {
    fn from(x: IntegerType) -> Type {
        match x {
            IntegerType::U8 => Type::U8,
            IntegerType::U16 => Type::U16,
            IntegerType::U32 => Type::U32,
            IntegerType::U64 => Type::U64,
            IntegerType::I8 => Type::I8,
            IntegerType::I16 => Type::I16,
            IntegerType::I32 => Type::I32,
            IntegerType::I64 => Type::I64,
        }
    }
}

pub enum ExplorationResult {
    Continue(Explorer),
    Done(NonNull<NewNode>),
}

impl Explorer {
    pub unsafe fn new(root: NonNull<NewNode>) -> Self {
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

        let leaf = leaf_ptr.as_ref();
        let statement = &leaf.asn.as_ref().statement;
        let slice = statement.arg.as_slice();
        match &statement.op {
            Op::Assign => {
                assert_eq!(leaf.asn.as_ref().child, None);
                match slice {
                    [Value::Variable(
                        rhs @ Variable {
                            addressing: Addressing::Direct | Addressing::Dereference,
                            ..
                        },
                    ), Value::Literal(Literal::Integer(x))] => {
                        // Gets the type the variable has been previously cast as.
                        let cast_opt = match &rhs.cast {
                            Some(Cast::As(cast_type)) => Some(cast_type.clone()),
                            Some(Cast::Prev) => todo!(),
                            None => leaf.prev.and_then(|p| find_variable_cast(p)),
                        };
                        // Based on the variables casted type
                        let new_leaves = match cast_opt {
                            // This will only arrise when the variable hasn't been previously defined,
                            // in this case we need to explore possible types.
                            None => {
                                // Get all possible integer types.
                                let possible = containing_types(*x);

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
                                                ..rhs.clone()
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
                                                asn: leaf.asn.as_ref().next.unwrap(),
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
                            // TODO: Check if the assigned value fits in the cast type, if it does do
                            // nothing, if it doesn't add to the stack to explore the types it could fit in
                            Some(cast) => todo!(),
                        };
                        self.stack.extend(new_leaves);
                    }
                    _ => todo!(),
                }
                ExplorationResult::Continue(self)
            }
            Op::Mov => {
                assert_eq!(leaf.asn.as_ref().child, None);
                let new_leaves = match slice {
                    [Value::Register(_), Value::Literal(_)] => handle_no_precond(leaf_ptr),
                    [Value::Register(_), Value::Variable(Variable {
                        addressing: Addressing::Direct,
                        index: None,
                        cast: None,
                        ..
                    })] => handle_no_precond(leaf_ptr),
                    _ => todo!(),
                };
                self.stack.push(new_leaves);
                ExplorationResult::Continue(self)
            }
            Op::Unreachable => {
                assert_eq!(slice, &[]);
                todo!();
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
                            assert_eq!(edge_next, leaf_ptr);
                            let parent_ptr = edge.prev;
                            let parent = parent_ptr.as_mut();

                            // All edges that are invalid are removed from the next list, and edges
                            // are explored in the order they are in the next list.
                            // So here the 1st edge in the next list will be this edge.
                            assert_eq!(parent.next.unwrap().pop(), Some(edge_ptr));

                            // We pick the 1st valid discovered edge. So here we discard the
                            // unexplored edges, removing them from the next list, removing their
                            // leaf nodes from the stack and deallocating these.
                            for unexplored_edge_ptr in parent.next.unwrap().drain(..).rev() {
                                let unexplored_edge = unexplored_edge_ptr.as_ref();
                                assert_eq!(unexplored_edge.precond, None);
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
            // This should be removed when inlining.
            Op::Def => unreachable!(),
            x @ _ => todo!("{x:?}"),
        }
    }
    unsafe fn resolve(self) -> NonNull<NewNode> {
        let Self { stack, front } = self;
        assert_eq!(stack, []);
        let front_ast = front.as_ref().asn;
        let mut stack = vec![front];
        while let Some(mut next_ptr) = stack.pop() {
            let mut next = next_ptr.as_mut();
            let mut edges = next.next.as_mut().unwrap();
            let edge_ptr = edges.pop().unwrap();
            assert_eq!(edges, &[]);

            // If there is a preconditional statement, insert this into the AST.
            if let Some(precond) = &edge_ptr.as_ref().precond {
                let mut asn_ptr = next.asn;
                let new_asn_ptr = alloc(Layout::new::<NewNode>()).cast();
                ptr::write(
                    new_asn_ptr,
                    NewNode {
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

fn nonnull<T>(x: T) -> NonNull<T> {
    NonNull::new(Box::into_raw(Box::new(x))).unwrap()
}
