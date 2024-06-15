use crate::ast::*;
use std::ptr::NonNull;

/// Replaces `loop`, `if` and `break` with assembly.
pub unsafe fn inline(_node: NonNull<AstNode>) -> NonNull<AstNode> {
    todo!()
}
