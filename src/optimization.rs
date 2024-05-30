use crate::ast::*;
use std::alloc::dealloc;
use std::alloc::Layout;
use std::ptr::NonNull;

// Pre-exploration optimization.
pub unsafe fn prex_optimization(root: NonNull<AstNode>) {
    let mut stack = vec![root];
    while let Some(mut current) = stack.pop() {
        match current.as_ref().statement.op {
            Op::Unreachable => {
                assert_eq!(current.as_ref().child, None);
                if let Some(next) = current.as_ref().next {
                    crate::exploration::dealloc_ast(next);
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
pub unsafe fn post_optimization(_root: NonNull<AstNode>) {}
