use crate::ast::*;
use crate::LOOP_LIMIT;
use std::alloc;
use std::io::Bytes;
use std::io::Read;
use std::iter::once;
use std::iter::Peekable;
use std::ptr::{self, NonNull};

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
    let mut identifier = Identifier::new();

    let addressing = match bytes.peek().map(|r| r.as_ref().unwrap()) {
        Some(b'&') => {
            bytes.next().unwrap().unwrap();
            Addressing::Reference
        }
        Some(b'*') => {
            bytes.next().unwrap().unwrap();
            Addressing::Dereference
        }
        Some(b'a'..=b'z' | b'_' | b'0'..=b'9') => {
            let b = bytes.next().unwrap().unwrap();
            identifier.push(b);
            Addressing::Direct
        }
        _ => panic!(
            "{identifier:?} {:?}",
            std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<_>>())
        ),
    };

    #[cfg(debug_assertions)]
    let mut i = 0;
    loop {
        #[cfg(debug_assertions)]
        {
            assert!(i < LOOP_LIMIT);
            i += 1;
        }

        match bytes.peek().map(|r| r.as_ref().unwrap()) {
            Some(b'a'..=b'z' | b'_' | b'0'..=b'9') => {
                let b = bytes.next().unwrap().unwrap();
                identifier.push(b);
            }
            // End of line
            Some(b' ') | Some(b'\n') | None => {
                break Variable {
                    addressing,
                    identifier,
                    index: None,
                }
            }
            // Slice start
            Some(b'.') => {
                break Variable {
                    addressing,
                    identifier,
                    index: None,
                }
            }
            // Slice end
            Some(b']') => {
                break Variable {
                    addressing,
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
                            addressing,
                            identifier,
                            index: Some(Box::new(Index::Slice(Slice { start, stop }))),
                        };
                    }
                    // Offset
                    Some(b']') => {
                        break Variable {
                            addressing,
                            identifier,
                            index: Some(Box::new(Index::Offset(start.unwrap()))),
                        }
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(
                "{identifier:?} {:?}",
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
        Some(b'&' | b'*' | b'a'..=b'z' | b'_') => {
            let variable = get_variable(bytes);
            if let Ok(register) = Register::try_from(&variable) {
                Value::Register(register)
            } else {
                Value::Variable(variable)
            }
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
pub fn get_nodes<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Option<NonNull<NewNode>> {
    let mut indent = 0;

    let mut first = None;
    let mut new_parent_stack: Vec<NonNull<NewNode>> = Vec::new();

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
        let mut new_node = unsafe {
            let ptr = alloc::alloc(alloc::Layout::new::<NewNode>()).cast::<NewNode>();
            ptr::write(ptr, NewNode::new(statement));
            NonNull::new(ptr).unwrap()
        };

        if first.is_none() {
            first = Some(new_node);
        }

        // Links node
        assert!(
            indent <= new_parent_stack.len(),
            "{} <= {}",
            indent,
            new_parent_stack.len()
        );
        let mut after = new_parent_stack.split_off(indent);
        if let Some(previous) = after.first_mut() {
            unsafe { previous.as_mut().next = Some(new_node) };
            unsafe { new_node.as_mut().preceding = Some(Preceding::Previous(*previous)) };
        } else if let Some(parent) = new_parent_stack.last_mut() {
            unsafe { parent.as_mut().child = Some(new_node) };
            unsafe { new_node.as_mut().preceding = Some(Preceding::Parent(*parent)) };
        }
        new_parent_stack.push(new_node);

        // Reset the indent for the next line.
        indent = 0;
    }
    first
}

pub fn get_cmp<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Cmp {
    match bytes.next().map(Result::unwrap) {
        Some(b'=') => Cmp::Eq,
        Some(b'>') => match bytes.peek().map(|r| r.as_ref().unwrap()) {
            Some(b'=') => {
                bytes.next().unwrap().unwrap();
                Cmp::Ge
            }
            _ => Cmp::Gt,
        },
        Some(b'<') => match bytes.peek().map(|r| r.as_ref().unwrap()) {
            Some(b'=') => {
                bytes.next().unwrap().unwrap();
                Cmp::Le
            }
            _ => Cmp::Lt,
        },
        _ => panic!(),
    }
}

#[cfg_attr(test, instrument(level = "TRACE", skip(bytes)))]
pub fn get_statement<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Statement {
    let variable = get_variable(bytes);

    // Check if statement should only be evaluated at runtime.
    let comptime = false;
    // if variable.identifier == "comptime" {
    //     assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));

    //     variable = get_variable(bytes);
    //     comptime = true;
    // }

    match (variable.identifier.0.as_slice(), &variable.index) {
        // Loop
        (b"loop", None) => Statement {
            comptime,
            op: Op::Intrinsic(Intrinsic::Loop),
            arg: get_values(bytes),
        },
        // If
        (b"if", None) => {
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            let lhs = get_value(bytes);
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            let op = Op::Intrinsic(Intrinsic::If(get_cmp(bytes)));
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            let rhs = get_value(bytes);
            let arg = vec![lhs, rhs];
            Statement { comptime, op, arg }
        }
        (b"break", None) => Statement {
            comptime,
            op: Op::Intrinsic(Intrinsic::Break),
            arg: Vec::new(),
        },
        (b"require", None) => {
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            let lhs = get_value(bytes);
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            let op = Op::Special(Special::Require(get_cmp(bytes)));
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            let rhs = get_value(bytes);
            let arg = vec![lhs, rhs];
            Statement { comptime, op, arg }
        }
        (b"unreachable", None) => Statement {
            comptime,
            op: Op::Special(Special::Unreachable),
            arg: Vec::new(),
        },
        (b"def", None) => {
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            let ident = get_variable(bytes);
            Statement {
                comptime,
                op: Op::Intrinsic(Intrinsic::Def),
                arg: vec![Value::Variable(ident)],
            }
        }
        (b"svc", None) => {
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            Statement {
                comptime,
                op: Op::Assembly(Assembly::Svc),
                arg: get_values(bytes),
            }
        }
        (b"mov", None) => {
            assert_eq!(bytes.next().map(Result::unwrap), Some(b' '));
            Statement {
                comptime,
                op: Op::Assembly(Assembly::Mov),
                arg: get_values(bytes),
            }
        }
        _ => {
            let lhs: Value = Value::Variable(variable);
            assert_eq!(
                bytes.next().map(Result::unwrap),
                Some(b' '),
                "{:?}",
                std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<_>>())
            );

            // if let Ok(syscall) = Syscall::try_from(lhs.variable().unwrap().identifier.as_slice()) {
            //     return Statement {
            //         comptime,
            //         op: Op::Syscall(syscall),
            //         arg: get_values(bytes),
            //     };
            // }

            match bytes.peek().map(|r| r.as_ref().unwrap()) {
                // Add, Sub, Mul, Div, Rem
                Some(p) if let Some(arithmetic) = Intrinsic::arithmetic_assign(*p) => {
                    bytes.next().map(Result::unwrap);
                    assert_eq!(Some(b'='), bytes.next().map(Result::unwrap));
                    assert_eq!(Some(b' '), bytes.next().map(Result::unwrap));
                    let arg = get_value(bytes);
                    let arg = vec![lhs, arg];
                    Statement {
                        comptime,
                        op: Op::Intrinsic(arithmetic),
                        arg,
                    }
                }
                Some(b':') => {
                    bytes.next().map(Result::unwrap);
                    match bytes.next().map(Result::unwrap) {
                        Some(b' ') => {
                            let variable_type = get_type(bytes);
                            Statement {
                                comptime,
                                op: Op::Special(Special::Type),
                                arg: vec![lhs, Value::Type(variable_type)],
                            }
                        }
                        Some(b'=') => {
                            assert_eq!(Some(b' '), bytes.next().map(Result::unwrap));
                            let first = get_value(bytes);

                            match bytes.peek().map(|r| r.as_ref().unwrap()) {
                                Some(b' ') => {
                                    bytes.next().unwrap().unwrap(); // Skip space.
                                    match first {
                                        Value::Variable(Variable {
                                            addressing: Addressing::Direct,
                                            identifier,
                                            index: None,
                                        }) if identifier == "sizeof" => {
                                            let tail = get_values(bytes);
                                            Statement {
                                                comptime,
                                                op: Op::Special(Special::SizeOf),
                                                arg: once(lhs)
                                                    .chain(tail.iter().cloned())
                                                    .collect(),
                                            }
                                        }
                                        Value::Variable(Variable {
                                            addressing: Addressing::Direct,
                                            identifier,
                                            index: None,
                                        }) if let Ok(syscall) = Syscall::try_from(&identifier) => {
                                            let tail = get_values(bytes);
                                            Statement {
                                                comptime,
                                                op: Op::Syscall(syscall),
                                                arg: once(lhs)
                                                    .chain(tail.iter().cloned())
                                                    .collect(),
                                            }
                                        }
                                        _ => match bytes.peek().map(|r| r.as_ref().unwrap()) {
                                            Some(p)
                                                if let Some(arithmetic) =
                                                    Intrinsic::arithmetic(*p) =>
                                            {
                                                bytes.next().unwrap().unwrap(); // Skip arithmetic operator.
                                                assert_eq!(
                                                    bytes.next().map(Result::unwrap),
                                                    Some(b' '),
                                                    "{:?}",
                                                    std::str::from_utf8(
                                                        &bytes
                                                            .map(Result::unwrap)
                                                            .collect::<Vec<_>>()
                                                    )
                                                );

                                                let second = get_value(bytes);
                                                let arg = vec![lhs, first, second];
                                                Statement {
                                                    comptime,
                                                    op: Op::Intrinsic(arithmetic),
                                                    arg,
                                                }
                                            }
                                            _ => {
                                                let tail = get_values(bytes);
                                                Statement {
                                                    comptime,
                                                    op: Op::Intrinsic(Intrinsic::Assign),
                                                    arg: once(lhs)
                                                        .chain(once(first.clone()))
                                                        .chain(tail.iter().cloned())
                                                        .collect(),
                                                }
                                            }
                                        },
                                    }
                                }
                                Some(b'\n') => Statement {
                                    comptime,
                                    op: Op::Intrinsic(Intrinsic::Assign),
                                    arg: once(lhs).chain(once(first.clone())).collect(),
                                },
                                _ => todo!(),
                            }
                        }
                        _ => panic!(
                            "{:?}",
                            std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<_>>())
                        ),
                    }
                }
                _ => Statement {
                    comptime,
                    op: Op::Intrinsic(Intrinsic::Call),
                    arg: std::iter::once(lhs).chain(get_values(bytes)).collect(),
                },
            }
        }
    }
}

pub fn get_includes(source: &mut Vec<u8>) {
    let mut i = 0;
    const INCLUDE: &[u8] = b"include";
    while i < source.len() - INCLUDE.len() {
        let j = i + INCLUDE.len();
        if &source[i..j] == INCLUDE {
            let mut k = j;
            loop {
                k += 1;
                match source.get(k) {
                    Some(b'\n') => break,
                    Some(_) => continue,
                    None => panic!(),
                }
            }
            let url = std::str::from_utf8(&source[j..k]).unwrap();
            let text = reqwest::blocking::get(url).unwrap().text().unwrap();
            source.splice(i..k, text.bytes());
        } else {
            i += 1;
        }
    }
}
