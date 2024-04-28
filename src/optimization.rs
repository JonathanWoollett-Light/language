use crate::ast::*;
use std::alloc::dealloc;
use std::alloc::Layout;
use std::ptr::NonNull;

// Pre-exploration optimization.
pub unsafe fn prex_optimization(root: NonNull<NewNode>) {
    let mut stack = vec![root];
    while let Some(mut current) = stack.pop() {
        match current.as_ref().statement.op {
            Op::Unreachable => {
                assert_eq!(current.as_ref().child, None);
                if let Some(next) = current.as_mut().next.take() {
                    let mut dealloc_stack = vec![next];
                    while let Some(dealloc_current) = dealloc_stack.pop() {
                        if let Some(child) = dealloc_current.as_ref().child {
                            dealloc_stack.push(child);
                        }
                        if let Some(next) = dealloc_current.as_ref().next {
                            dealloc_stack.push(next);
                        }
                        dealloc(dealloc_current.as_ptr().cast(), Layout::new::<NewNode>())
                    }
                }
            }
            _ => {
                if let Some(child) = current.as_ref().child {
                    stack.push(child);
                }
                if let Some(next) = current.as_ref().next {
                    stack.push(next);
                }
            }
        }
    }
}

/// Post-exploration optimization.
pub unsafe fn post_optimization(root: NonNull<NewNode>) {}
