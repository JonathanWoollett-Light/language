use crate::ast::*;
use std::collections::HashMap;

pub fn optimize_nodes(nodes: &[Node]) -> Vec<Node> {
    let mut output = Vec::new();

    // Stores types for each variable.
    let mut type_map = HashMap::new();

    assert!(!nodes.is_empty());
    let mut stack = vec![0];
    while let Some(current) = stack.pop() {
        // Set node
        let node = &nodes[current];
        if let Some(next) = node.next {
            stack.push(next);
        }
        if let Some(child) = node.child {
            stack.push(child)
        }

        match &node.statement {
            Statement {
                runtime: _,
                op: Op::Intrinsic(Intrinsic::Assign),
                arg,
            } => {
                let Some([Value::Variable(Variable(identifier)), tail @ ..]) = arg.get(..) else {
                    todo!()
                };
                match tail {
                    [] | [Value::Literal(Literal::Integer(_))] => {}
                    [Value::Literal(Literal::String(s))] => {
                        type_map.insert(identifier, Type::Array(Array(s.len() as u64)));
                    }
                    _ => {
                        type_map.insert(identifier, Type::Array(Array(tail.len() as u64)));
                    }
                }
                output.push(node.clone());
            }
            // E.g. `x = read 1` which reads from `STDIN` the number of bytes required for the type of `x`, in this case the type isn't specified so we need to figure it (and defaulting to `i32` if we can't).
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::Read),
                arg,
            } => {
                if let Some([x, _fd]) = arg.get(..) {
                    let Value::Variable(Variable(x)) = x else {
                        todo!()
                    };

                    let val_type = if let Some(val_type) = type_map.get(&x) {
                        *val_type
                    } else {
                        let val_type = search(x, current, nodes);
                        type_map.insert(x, val_type);
                        val_type
                    };
                    let n = val_type.bytes();

                    // This is wrong, but it works for now.
                    let mut new_node = node.clone();
                    new_node
                        .statement
                        .arg
                        .push(Value::Literal(Literal::Integer(n)));
                    output.push(new_node);
                }
            }
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::Write),
                arg,
            } => {
                if let Some([_result, _fd, x]) = arg.get(..) {
                    let Value::Variable(Variable(x)) = x else {
                        todo!()
                    };

                    let val_type = type_map.get(&x).unwrap();
                    let n = val_type.bytes();

                    // This is wrong, but it works for now.
                    let mut new_node = node.clone();
                    new_node
                        .statement
                        .arg
                        .push(Value::Literal(Literal::Integer(n)));
                    output.push(new_node);
                }
            }
            // This is wrong, but it works for now.
            _ => output.push(node.clone()),
        }
    }
    output
}

#[derive(Copy, Clone)]
enum Type {
    Integer(Integer),
    Array(Array),
    FileDescriptor,
}
impl Type {
    fn bytes(&self) -> u64 {
        self.bits().div_ceil(8)
    }
    fn bits(&self) -> u64 {
        match self {
            Self::Integer(Integer(n)) => *n,
            Self::FileDescriptor => 32,
            Self::Array(Array(x)) => 8 * x,
        }
    }
}
/// The number of elements in the array
#[derive(Copy, Clone)]
struct Array(u64);
/// The bits in size.
#[derive(Copy, Clone)]
struct Integer(u64);

/// Searches through the AST to figure out the type of a variable.
fn search(identifier: &[u8], start: usize, nodes: &[Node]) -> Type {
    let mut size = None;

    let mut stack = vec![start];
    while let Some(current) = stack.pop() {
        // Set node
        let node = &nodes[current];
        if let Some(next) = node.next {
            stack.push(next);
        }
        if let Some(child) = node.child {
            stack.push(child)
        }

        // Look at node
        match &node.statement {
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::Read),
                arg,
            } if let Some([x,_fd,n]) = arg.get(..) && let Value::Variable(Variable(x)) = x && identifier == x => {
                if let Some(m) = size {
                    match n {
                        Value::Literal(Literal::Integer(n)) => {
                            assert_eq!(m, *n, "Cannot read 2 different number of bytes into a single type of 1 size.");
                        },
                        _ => todo!()
                    }
                }
                else {
                    match n {
                        Value::Literal(Literal::Integer(n)) => {
                            size = Some(*n);
                        },
                        _ => todo!()
                    }
                }
            },
            Statement {
                runtime: _,
                op: Op::Syscall(Syscall::MemfdCreate),
                arg,
            } if let Some([x]) = arg.get(..) && let Value::Variable(Variable(x)) = x && identifier == x => {
                assert!(size.is_none() || size == Some(32));
                return Type::FileDescriptor;
            },
            _ => continue,
        }
    }
    match size {
        // Default to `i32`.
        None => Type::Integer(Integer(32)),
        // Default to signed integer.
        Some(n) => Type::Integer(Integer(n)),
    }
}
