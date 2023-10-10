use crate::ast::*;
use crate::LOOP_LIMIT;
use std::io::Bytes;
use std::io::Read;
use std::iter::once;
use std::iter::Peekable;

const RUNTIME_IDENTIFIER: &[u8] = b"comptime";

#[cfg(test)]
use tracing::instrument;

#[cfg_attr(test, instrument(level = "TRACE", skip(bytes)))]
pub fn get_type<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Type {
    match bytes.next().map(Result::unwrap) {
        Some(b'i') => match bytes.next().map(Result::unwrap) {
            Some(b'8') => Type::I8,
            Some(b'1') => match bytes.next().map(Result::unwrap) {
                Some(b'6') => Type::I16,
                _ => panic!(),
            },
            Some(b'3') => match bytes.next().map(Result::unwrap) {
                Some(b'2') => Type::I32,
                _ => panic!(),
            },
            Some(b'6') => match bytes.next().map(Result::unwrap) {
                Some(b'4') => Type::I64,
                _ => panic!(),
            },
            _ => panic!(),
        },
        Some(b'u') => match bytes.next().map(Result::unwrap) {
            Some(b'8') => Type::U8,
            Some(b'1') => match bytes.next().map(Result::unwrap) {
                Some(b'6') => Type::U16,
                _ => panic!(),
            },
            Some(b'3') => match bytes.next().map(Result::unwrap) {
                Some(b'2') => Type::U32,
                _ => panic!(),
            },
            Some(b'6') => match bytes.next().map(Result::unwrap) {
                Some(b'4') => Type::U64,
                _ => panic!(),
            },
            _ => panic!(),
        },
        _ => panic!(),
    }
}

#[cfg_attr(test, instrument(level = "TRACE", skip(bytes), ret))]
pub fn get_variable<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Variable {
    // Get identifier
    let mut identifier = Vec::new();
    #[cfg(debug_assertions)]
    let mut i = 0;
    loop {
        #[cfg(debug_assertions)]
        {
            assert!(i < LOOP_LIMIT);
            i += 1;
        }

        match bytes.peek().map(|r| r.as_ref().unwrap()) {
            Some(b'a'..=b'z' | b'_') => {
                let b = bytes.next().unwrap().unwrap();
                identifier.push(b);
            }
            // End of line
            Some(b' ') | Some(b'\n') | None => {
                break Variable {
                    identifier,
                    index: None,
                }
            }
            // Slice start
            Some(b'.') => {
                break Variable {
                    identifier,
                    index: None,
                }
            }
            // Slice end
            Some(b']') => {
                break Variable {
                    identifier,
                    index: None,
                }
            }
            // Index start
            Some(b'[') => {
                bytes.next().unwrap().unwrap();
                let start = get_offset(bytes);
                match bytes.next().map(Result::unwrap) {
                    // Slice
                    Some(b'.') => {
                        assert_eq!(bytes.next().map(Result::unwrap), Some(b'.'));
                        let stop = get_offset(bytes);
                        assert_eq!(bytes.next().map(Result::unwrap), Some(b']'));
                        break Variable {
                            identifier,
                            index: Some(Box::new(Index::Slice(Slice { start, stop }))),
                        };
                    }
                    // Offset
                    Some(b']') => {
                        break Variable {
                            identifier,
                            index: Some(Box::new(Index::Offset(start.unwrap()))),
                        }
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(
                "{:?} {:?}",
                std::str::from_utf8(&identifier),
                std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<_>>())
            ),
        }
    }
}

// Similar to `get_value` but different in how it considers possible endings when parsing.
#[cfg_attr(test, instrument(level = "TRACE", skip(bytes), ret))]
pub fn get_offset<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Option<Offset> {
    match bytes.peek().map(|r| r.as_ref().unwrap()) {
        Some(b'0') => {
            bytes.next().unwrap().unwrap();
            Some(Offset::Integer(0))
        }
        Some(b'1'..=b'9') => {
            let b = bytes.next().unwrap().unwrap();
            let mut literal = (b - b'0') as u64;
            #[cfg(debug_assertions)]
            let mut i = 0;
            loop {
                #[cfg(debug_assertions)]
                {
                    assert!(i < LOOP_LIMIT);
                    i += 1;
                }

                match bytes.peek().map(|r| r.as_ref().unwrap()) {
                    Some(b'0'..=b'9') => {
                        let b = bytes.next().unwrap().unwrap();
                        literal *= 10;
                        literal += (b - b'0') as u64;
                    }
                    // Slice start
                    Some(b'.') => break,
                    // Slice end
                    Some(b']') => break,
                    _ => panic!(),
                }
            }
            Some(Offset::Integer(literal))
        }
        Some(b'a'..=b'z' | b'_') => Some(Offset::Variable(get_variable(bytes))),
        // Slice start
        Some(b'.') => None,
        // Slice end
        Some(b']') => None,
        x => panic!("unexpected: {:?}", x.map(|c| *c as char)),
    }
}

#[cfg_attr(test, instrument(level = "TRACE", skip(bytes), ret))]
pub fn get_value<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Value {
    let non_zero_integer = |bytes: &mut Peekable<Bytes<R>>| -> i128 {
        let b = bytes.next().unwrap().unwrap();
        let mut literal = (b - b'0') as i128;
        #[cfg(debug_assertions)]
        let mut i = 0;
        loop {
            #[cfg(debug_assertions)]
            {
                assert!(i < LOOP_LIMIT);
                i += 1;
            }

            match bytes.peek().map(|r| r.as_ref().unwrap()) {
                Some(b'0'..=b'9') => {
                    let b = bytes.next().unwrap().unwrap();
                    literal *= 10;
                    literal += (b - b'0') as i128;
                }
                Some(b' ') | Some(b'\n') | None => break,
                _ => panic!(),
            }
        }
        literal
    };
    match bytes.peek().map(|r| r.as_ref().unwrap()) {
        Some(b'-') => {
            bytes.next().unwrap().unwrap();
            match bytes.peek().map(|r| r.as_ref().unwrap()) {
                Some(b'0') => {
                    bytes.next().unwrap().unwrap();
                    Value::Literal(Literal::Integer(0))
                }
                Some(b'1'..=b'9') => Value::Literal(Literal::Integer(-non_zero_integer(bytes))),
                _ => todo!(),
            }
        }
        Some(b'0') => {
            bytes.next().unwrap().unwrap();
            Value::Literal(Literal::Integer(0))
        }
        Some(b'1'..=b'9') => Value::Literal(Literal::Integer(non_zero_integer(bytes))),
        Some(b'a'..=b'z' | b'_') => Value::Variable(get_variable(bytes)),
        Some(b'"') => {
            bytes.next().unwrap().unwrap();
            #[cfg(debug_assertions)]
            let mut i = 0;
            let mut string = Vec::new();
            loop {
                #[cfg(debug_assertions)]
                {
                    assert!(i < LOOP_LIMIT);
                    i += 1;
                }

                match bytes.peek().map(|r| r.as_ref().unwrap()) {
                    Some(b'\\') => {
                        bytes.next().unwrap().unwrap();
                        match bytes.next().map(Result::unwrap) {
                            Some(b'n') => string.push(b'\n'),
                            Some(b'"') => string.push(b'"'),
                            _ => todo!(),
                        }
                    }
                    Some(b'"') => {
                        bytes.next().unwrap().unwrap();
                        break;
                    }
                    Some(_) => {
                        let b = bytes.next().unwrap().unwrap();
                        string.push(b);
                    }
                    _ => panic!(),
                }
            }
            Value::Literal(Literal::String(
                std::str::from_utf8(&string).unwrap().to_string(),
            ))
        }
        _ => panic!(
            "unexpected: {:?}",
            std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<_>>()).unwrap()
        ),
    }
}

#[cfg_attr(test, instrument(level = "TRACE", skip(bytes), ret))]
pub fn get_values<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Vec<Value> {
    let mut values = Vec::new();

    #[cfg(debug_assertions)]
    let mut i = 0;
    loop {
        #[cfg(debug_assertions)]
        {
            assert!(i < LOOP_LIMIT);
            i += 1;
        }

        match bytes.peek().map(|r| r.as_ref().unwrap()) {
            Some(b' ') => {
                bytes.next().unwrap().unwrap();
            }
            Some(b'\n') | None => break,
            Some(_) => {
                values.push(get_value(bytes));
            }
        }
    }
    values
}

#[cfg_attr(test, instrument(level = "TRACE", skip(bytes)))]
pub fn get_nodes<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Vec<Node> {
    let mut stack: Vec<Node> = Vec::new();
    let mut parent_stack: Vec<usize> = Vec::new();
    let mut indent = 0;

    // TODO Is this comment still valid?
    // Due to how we handle parsing, variable identifiers cannot be defined with the starting
    // character `e` or `i` as these lead to the parsing trying to evaluate `exit` or `if`.
    // This is a weird quirk that should be fixed in the future.

    #[cfg(debug_assertions)]
    let mut i = 0;
    loop {
        #[cfg(debug_assertions)]
        {
            assert!(i < LOOP_LIMIT);
            i += 1;
        }

        let statement = match bytes.peek().map(|r| r.as_ref().unwrap()) {
            // Indent
            Some(b' ') => {
                bytes.next().unwrap().unwrap();
                let remaining = bytes
                    .by_ref()
                    .take(3)
                    .map(Result::unwrap)
                    .collect::<Vec<_>>();
                debug_assert_eq!(remaining.len(), b"   ".len());
                assert_eq!(
                    remaining.as_slice(),
                    b"   ",
                    "remaining: {:?}",
                    std::str::from_utf8(&remaining).unwrap()
                );
                indent += 1;
                continue;
            }
            // Newline
            Some(b'\n') => {
                bytes.next().unwrap().unwrap();
                continue;
            }
            // Comment
            Some(b'#') => {
                loop {
                    match bytes.next().map(Result::unwrap) {
                        Some(b'\n') | None => break,
                        Some(_) => continue,
                    }
                }
                continue;
            }
            Some(_) => get_statement(bytes),
            // End of file
            None => break,
        };

        // Wrap the statement in a node.
        let node = Node::new(statement);

        // Links node
        assert!(
            indent <= parent_stack.len(),
            "{} <= {}",
            indent,
            parent_stack.len()
        );
        let after = parent_stack.split_off(indent);
        if let Some(previous) = after.first() {
            stack[*previous].next = Some(stack.len());
        } else if let Some(parent) = parent_stack.last() {
            stack[*parent].child = Some(stack.len());
        }
        parent_stack.push(stack.len());

        // Add node
        stack.push(node);

        // Reset the indent for the next line.
        indent = 0;
    }
    stack
}

#[cfg_attr(test, instrument(level = "TRACE", skip(bytes)))]
pub fn get_statement<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Statement {
    let mut variable = get_variable(bytes);

    // Check if statement should only be evaluated at runtime.
    let mut comptime = false;
    if variable.identifier == RUNTIME_IDENTIFIER {
        assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));

        variable = get_variable(bytes);
        comptime = true;
    }

    match (variable.identifier.as_slice(), &variable.index) {
        // Loop
        (b"loop", None) => Statement {
            comptime,
            op: Op::Intrinsic(Intrinsic::Loop),
            arg: get_values(bytes),
        },
        // Exit
        (b"exit", None) => Statement {
            comptime,
            op: Op::Syscall(Syscall::Exit),
            arg: get_values(bytes),
        },
        // If
        (b"if", None) => {
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            let lhs = get_value(bytes);
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            let op = match bytes.next().map(Result::unwrap) {
                Some(b'=') => Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
                Some(b'>') => Op::Intrinsic(Intrinsic::If(Cmp::Gt)),
                Some(b'<') => Op::Intrinsic(Intrinsic::If(Cmp::Lt)),
                _ => panic!(),
            };
            assert_eq!(
                bytes.next().map(Result::unwrap),
                Some(b' '),
                "{:?}",
                std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<_>>())
            );
            let rhs = get_value(bytes);
            let arg = vec![lhs, rhs];
            Statement { comptime, op, arg }
        }
        (b"break", None) => Statement {
            comptime,
            op: Op::Intrinsic(Intrinsic::Break),
            arg: Vec::new(),
        },
        _ => {
            let lhs = Value::Variable(variable);
            assert_eq!(
                bytes.next().map(Result::unwrap),
                Some(b' '),
                "{:?}",
                std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<_>>())
            );
            match bytes.next().map(Result::unwrap) {
                // Add, Sub, Mul, Div, Rem
                Some(p) if let Some(arithmetic) = Intrinsic::arithmetic_assign(p) => {
                    assert_eq!(Some(b'='), bytes.next().map(Result::unwrap));
                    assert_eq!(Some(b' '), bytes.next().map(Result::unwrap));
                    let arg = get_value(bytes);
                    let arg = vec![lhs, arg];
                    Statement { comptime, op: Op::Intrinsic(arithmetic), arg, }
                }
                Some(b':') => match bytes.next().map(Result::unwrap) {
                    Some(b' ') => {
                        let variable_type = get_type(bytes);
                        Statement {
                            comptime,
                            op: Op::Special(Special::Type),
                            arg: vec![lhs, Value::Type(variable_type)]
                        }
                    }
                    Some(b'=') => {
                        assert_eq!(Some(b' '), bytes.next().map(Result::unwrap));
                        let first = get_value(bytes);

                        let get_statement = |lhs: Value, first: Value, bytes: &mut Peekable<Bytes<R>>| -> Statement {
                            let tail = get_values(bytes);
                            match (&first, &tail) {
                                (Value::Variable(Variable { identifier, index: None }), _) if let Ok(syscall) = Syscall::try_from(identifier.as_slice()) => {
                                    Statement {
                                        comptime,
                                        op: Op::Syscall(syscall),
                                        arg: once(lhs)
                                            .chain(tail.iter().cloned())
                                            .collect(),
                                    }
                                },
                                _ => Statement {
                                    comptime,
                                    op: Op::Intrinsic(Intrinsic::Assign),
                                    arg: once(lhs)
                                        .chain(once(first.clone()))
                                        .chain(tail.iter().cloned())
                                        .collect(),
                                },
                            }
                        };

                        match bytes.peek().map(|r| r.as_ref().unwrap()) {
                            Some(b' ') => {
                                bytes.next().unwrap().unwrap(); // Skip space.
                                match bytes.peek().map(|r| r.as_ref().unwrap()) {
                                    Some(p) if let Some(arithmetic) = Intrinsic::arithmetic(*p) => {
                                        bytes.next().unwrap().unwrap(); // Skip arithmetic operator.
                                        assert_eq!(Some(b' '), bytes.next().map(Result::unwrap));

                                        let second = get_value(bytes);
                                        let arg = vec![lhs, first, second];
                                        Statement { comptime, op: Op::Intrinsic(arithmetic), arg, }
                                    },
                                    _ => get_statement(lhs, first, bytes),
                                }
                            },
                            _ => get_statement(lhs, first, bytes),
                        }
                    },
                    _ => panic!("{:?}", std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<_>>())),
                }
                _ => panic!("{:?}", std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<_>>())),
            }
        }
    }
}
