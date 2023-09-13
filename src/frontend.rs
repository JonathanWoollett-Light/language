use crate::ast::*;
use crate::LOOP_LIMIT;
use std::io::Bytes;
use std::io::Read;
use std::iter::once;
use std::iter::Peekable;

const RUNTIME_IDENTIFIER: &[u8] = b"rt";

pub enum GetValue {
    Value(Value),
    NewLine,
    Space,
    None,
}

pub fn get_identifier<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Vec<u8> {
    let mut variable = Vec::new();
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
                variable.push(b);
            }
            Some(b' ') | Some(b'\n') | None => break,
            _ => panic!(),
        }
    }
    variable
}

pub fn get_value<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> GetValue {
    match bytes.peek().map(|r| r.as_ref().unwrap()) {
        Some(b'0') => {
            bytes.next().unwrap().unwrap();
            GetValue::Value(Value::Literal(Literal::Integer(0)))
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
                    Some(b' ') | Some(b'\n') | None => break,
                    _ => panic!(),
                }
            }
            GetValue::Value(Value::Literal(Literal::Integer(literal)))
        }
        Some(b'a'..=b'z' | b'_') => {
            GetValue::Value(Value::Variable(Variable(get_identifier(bytes))))
        }
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
            GetValue::Value(Value::Literal(Literal::String(
                std::str::from_utf8(&string).unwrap().to_string(),
            )))
        }
        Some(b' ') => GetValue::Space,
        Some(b'\n') => GetValue::NewLine,
        None => GetValue::None,
        x => panic!("unexpected: {:?}", x.map(|c| *c as char)),
    }
}

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

        match get_value(bytes) {
            GetValue::Value(value) => values.push(value),
            GetValue::Space => {
                bytes.next().unwrap().unwrap();
            }
            GetValue::NewLine | GetValue::None => break,
        }
    }
    values
}

pub fn get_nodes<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Vec<Node> {
    let mut stack: Vec<Node> = Vec::new();
    let mut parent_stack: Vec<usize> = Vec::new();
    let mut indent = 0;

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

        let byte = bytes.peek().map(|r| r.as_ref().unwrap());
        let statement = match byte {
            // Indent
            Some(b' ') => {
                bytes.next().unwrap().unwrap();
                let remaining = bytes
                    .by_ref()
                    .take(3)
                    .map(Result::unwrap)
                    .collect::<Vec<_>>();
                debug_assert_eq!(remaining.len(), b"   ".len());
                match remaining.as_slice() {
                    b"   " => {
                        indent += 1;
                        continue;
                    }
                    _ => panic!("remaining: {:?}", std::str::from_utf8(&remaining).unwrap()),
                }
            }
            // Newline
            Some(b'\n') => {
                bytes.next().unwrap().unwrap();
                continue;
            }
            // Op with assignment
            Some(_) => {
                let mut identifier = get_identifier(bytes);

                // Check if statement should only be evaluated at runtime.
                let mut runtime = false;
                if identifier == RUNTIME_IDENTIFIER {
                    assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
                    identifier = get_identifier(bytes);
                    runtime = true
                }

                match identifier.as_slice() {
                    // Exit
                    b"exit" => Statement {
                        runtime,
                        op: Op::Syscall(Syscall::Exit),
                        arg: get_values(bytes),
                    },
                    // If
                    b"if" => {
                        assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
                        let GetValue::Value(lhs) = get_value(bytes) else {
                            panic!()
                        };
                        assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
                        let op = match bytes.next().map(Result::unwrap) {
                            Some(b'=') => Op::Intrinsic(Intrinsic::IfEq),
                            Some(b'>') => Op::Intrinsic(Intrinsic::IfLt),
                            Some(b'<') => Op::Intrinsic(Intrinsic::IfGt),
                            _ => panic!(),
                        };
                        assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
                        let GetValue::Value(rhs) = get_value(bytes) else {
                            panic!()
                        };
                        Statement {
                            runtime,
                            op,
                            arg: vec![lhs, rhs],
                        }
                    }
                    _ => {
                        let lhs = Value::Variable(Variable(identifier));
                        assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
                        match bytes.next().map(Result::unwrap) {
                            // Add, Sub, Mul, Div
                            Some(part @ b'+' | part @ b'-' | part @ b'*' | part @ b'/') => {
                                assert_eq!(Some(b'='), bytes.next().map(Result::unwrap));
                                assert_eq!(Some(b' '), bytes.next().map(Result::unwrap));
                                let GetValue::Value(arg) = get_value(bytes) else {
                                    panic!()
                                };
                                Statement {
                                    runtime,
                                    op: Op::Intrinsic(match part {
                                        b'+' => Intrinsic::Add,
                                        b'-' => Intrinsic::Sub,
                                        b'/' => Intrinsic::Div,
                                        b'*' => Intrinsic::Mul,
                                        _ => panic!(),
                                    }),
                                    arg: vec![lhs, arg],
                                }
                            }
                            Some(b':') => {
                                assert_eq!(Some(b'='), bytes.next().map(Result::unwrap));
                                assert_eq!(Some(b' '), bytes.next().map(Result::unwrap));
                                let values = get_values(bytes);
                                match values.as_slice() {
                                    [first @ Value::Literal(_), tail @ ..] => Statement {
                                        runtime,
                                        op: Op::Intrinsic(Intrinsic::Assign),
                                        arg: once(lhs)
                                            .chain(once(first.clone()))
                                            .chain(tail.iter().cloned())
                                            .collect(),
                                    },
                                    [Value::Variable(Variable(variable)), tail @ ..] => {
                                        match variable.as_slice() {
                                            b"write" => Statement {
                                                runtime,
                                                op: Op::Syscall(Syscall::Write),
                                                arg: once(lhs)
                                                    .chain(tail.iter().cloned())
                                                    .collect(),
                                            },
                                            b"read" => Statement {
                                                runtime,
                                                op: Op::Syscall(Syscall::Read),
                                                arg: once(lhs)
                                                    .chain(tail.iter().cloned())
                                                    .collect(),
                                            },
                                            b"memfd_create" => Statement {
                                                runtime,
                                                op: Op::Syscall(Syscall::MemfdCreate),
                                                arg: once(lhs)
                                                    .chain(tail.iter().cloned())
                                                    .collect(),
                                            },
                                            _ => panic!(),
                                        }
                                    }
                                    _ => todo!(),
                                }
                            }
                            _ => panic!(),
                        }
                    }
                }
            }
            // End of file
            None => break,
        };

        // Wrap the statement in a node.
        let node = Node::new(statement);

        // Links node
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
