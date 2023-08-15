#![feature(test)]
extern crate test;

#[allow(unreachable_code)]
fn main() {
    let mut allocator = SimpleAllocator::new();
    let _ = node_from_bytes(b"", &mut allocator);
    todo!()
}

fn successor_from_bytes(bytes: &[u8], allocator: &mut SimpleAllocator) -> Successor {
    println!(
        "successor_from_bytes: b{:?}",
        std::str::from_utf8(bytes).unwrap()
    );
    match &bytes[..4] {
        b"    " => {
            let node = node_from_bytes(&bytes[4..], allocator);
            println!("node: {:?}", node);
            let ptr = allocator.alloc(node);
            Successor::Child(ptr)
        }
        _ => {
            let node = node_from_bytes(bytes, allocator);
            println!("node: {:?}", node);
            let ptr: usize = allocator.alloc(node);
            println!("ptr: {:?}", ptr);
            Successor::Next(ptr)
        }
    }
}

fn call_identifier_from_bytes(bytes: &[u8]) -> CallIdentifier {
    match bytes {
        b"add" => CallIdentifier::Intrinsic(Intrinsic::Add),
        b"div" => CallIdentifier::Intrinsic(Intrinsic::Div),
        b"mul" => CallIdentifier::Intrinsic(Intrinsic::Mul),
        b"sub" => CallIdentifier::Intrinsic(Intrinsic::Sub),
        b"assign" => CallIdentifier::Intrinsic(Intrinsic::Assign),
        b"exit" => CallIdentifier::Syscall(Syscall::Exit),
        _ => panic!(),
    }
}

fn node_from_bytes(bytes: &[u8], allocator: &mut SimpleAllocator) -> Node {
    let mut i = 0;
    loop {
        match bytes[i] {
            b'a'..=b'z' | b'_' => {
                i += 1;
            }
            b' ' => break,
            _ => panic!(),
        }
    }
    let identifier = call_identifier_from_bytes(&bytes[..i]);

    i += 1; // Skip space

    let mut argument = Vec::new();
    'outer: loop {
        let x = match bytes[i] {
            b'0' => {
                i += 1;
                match bytes.get(i) {
                    Some(b'\n') => {
                        argument.push(Value::Literal(Literal(0)));
                        break 'outer Node {
                            statement: Statement::Call(Call {
                                identifier,
                                argument,
                            }),
                            successor: Some(successor_from_bytes(&bytes[i + 1..], allocator)),
                        };
                    }
                    None => {
                        argument.push(Value::Literal(Literal(0)));
                        break 'outer Node {
                            statement: Statement::Call(Call {
                                identifier,
                                argument,
                            }),
                            successor: None,
                        };
                    }
                    Some(b' ') => Value::Literal(Literal(0)),
                    Some(_) => panic!(),
                }
            }
            b'1'..=b'9' => {
                let mut literal = (bytes[i] - b'0') as u64;
                i += 1;
                loop {
                    match bytes.get(i) {
                        Some(b'0'..=b'9') => {
                            literal *= 10;
                            literal += (bytes[i] - b'0') as u64;
                            i += 1;
                        }
                        Some(b'\n') => {
                            argument.push(Value::Literal(Literal(literal)));
                            break 'outer Node {
                                statement: Statement::Call(Call {
                                    identifier,
                                    argument,
                                }),
                                successor: Some(successor_from_bytes(&bytes[i + 1..], allocator)),
                            };
                        }
                        None => {
                            argument.push(Value::Literal(Literal(literal)));
                            break 'outer Node {
                                statement: Statement::Call(Call {
                                    identifier,
                                    argument,
                                }),
                                successor: None,
                            };
                        }
                        Some(b' ') => break Value::Literal(Literal(literal)),
                        _ => panic!(),
                    }
                }
            }
            b'a'..=b'z' => {
                let j = i;

                loop {
                    i += 1;
                    match bytes.get(i) {
                        Some(b'a'..=b'z' | b'_') => continue,
                        Some(b'\n') => {
                            argument.push(Value::Variable(Variable(Vec::from(&bytes[j..i]))));
                            break 'outer Node {
                                statement: Statement::Call(Call {
                                    identifier,
                                    argument,
                                }),
                                successor: Some(successor_from_bytes(&bytes[i + 1..], allocator)),
                            };
                        }
                        None => {
                            argument.push(Value::Variable(Variable(Vec::from(&bytes[j..i]))));
                            break 'outer Node {
                                statement: Statement::Call(Call {
                                    identifier,
                                    argument,
                                }),
                                successor: None,
                            };
                        }
                        Some(b' ') => {
                            i += 1;
                            break Value::Variable(Variable(Vec::from(&bytes[j..i - 1])));
                        }
                        _ => panic!(),
                    }
                }
            }
            _ => panic!(),
        };
        argument.push(x);
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Loop(Option<AllocatorKey>);

#[derive(Debug, Eq, PartialEq)]
enum Successor {
    Child(AllocatorKey),
    Next(AllocatorKey),
}

// Like `*mut Node`
type AllocatorKey = usize;

// An allocator is such a wonky hashmap after all.
// A very CPU performant but memory inefficient allocator.
#[derive(Debug, PartialEq, Eq)]
struct SimpleAllocator {
    buffer: Vec<Node>,
}
impl SimpleAllocator {
    fn alloc(&mut self, value: Node) -> AllocatorKey {
        let key = self.buffer.len();
        self.buffer.push(value);
        key
    }
    fn new() -> Self {
        Self { buffer: Vec::new() }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Node {
    statement: Statement,
    successor: Option<Successor>,
}

#[derive(Debug, Eq, PartialEq)]
enum Value {
    Literal(Literal),
    Variable(Variable),
}

#[derive(Debug, Eq, PartialEq)]
struct Literal(u64);

#[derive(Debug, Eq, PartialEq)]
struct Variable(Vec<u8>);

#[derive(Debug, Eq, PartialEq)]
struct Call {
    identifier: CallIdentifier,
    argument: Vec<Value>,
}

#[derive(Debug, Eq, PartialEq)]
enum CallIdentifier {
    Intrinsic(Intrinsic),
    Syscall(Syscall),
}

#[derive(Debug, Eq, PartialEq)]
enum Intrinsic {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Eq, PartialEq)]
enum Syscall {
    Exit,
}

#[derive(Debug, Eq, PartialEq)]
enum Statement {
    Call(Call),
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    impl SimpleAllocator {
        fn get(&self, key: AllocatorKey) -> &Node {
            &self.buffer[key]
        }
    }
    impl Successor {
        fn unwrap(&self) -> &AllocatorKey {
            match self {
                Self::Child(x) | Self::Next(x) => x,
            }
        }
    }

    fn parse(text: &str) -> (SimpleAllocator, Node) {
        let mut allocator = SimpleAllocator::new();
        let node = node_from_bytes(text.as_bytes(), &mut allocator);
        (allocator,node)
    }

    const ONE: &str = "exit 0";

    #[bench]
    fn bench_one(b: &mut Bencher) {
        b.iter(|| parse(ONE));
    }

    #[test]
    fn test_one() {
        let (allocator, node) = parse(ONE);
        assert_eq!(
            node,
            Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Syscall(Syscall::Exit),
                    argument: vec![Value::Literal(Literal(0))]
                }),
                successor: None,
            }
        );
        assert_eq!(allocator, SimpleAllocator::new());
    }

    const TWO: &str = "exit 1";

    #[bench]
    fn bench_two(b: &mut Bencher) {
        b.iter(|| parse(TWO));
    }

    #[test]
    fn test_two() {
        let (allocator, node) = parse(TWO);
        assert_eq!(
            node,
            Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Syscall(Syscall::Exit),
                    argument: vec![Value::Literal(Literal(1))]
                }),
                successor: None,
            }
        );
        assert_eq!(allocator,SimpleAllocator::new());
    }

    const THREE: &str = "exit 12";

    #[bench]
    fn bench_three(b: &mut Bencher) {
        b.iter(|| parse(THREE));
    }

    #[test]
    fn test_three() {
        let (allocator, node) = parse(THREE);
        assert_eq!(
            node,
            Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Syscall(Syscall::Exit),
                    argument: vec![Value::Literal(Literal(12))]
                }),
                successor: None,
            }
        );
        assert_eq!(allocator, SimpleAllocator::new());
    }

    const FOUR: &str = "exit 1\nexit 2";

    #[bench]
    fn bench_four(b: &mut Bencher) {
        b.iter(|| parse(FOUR));
    }

    #[test]
    fn test_four() {
        let (allocator, node) = parse(FOUR);
        assert_eq!(
            node,
            Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Syscall(Syscall::Exit),
                    argument: vec![Value::Literal(Literal(1))]
                }),
                successor: Some(Successor::Next(0)),
            }
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![Node {
                    statement: Statement::Call(Call {
                        identifier: CallIdentifier::Syscall(Syscall::Exit),
                        argument: vec![Value::Literal(Literal(2))]
                    }),
                    successor: None,
                }]
            }
        );
    }


    const FIVE: &str = "exit 1\n    exit 2";

    #[bench]
    fn bench_five(b: &mut Bencher) {
        b.iter(|| parse(FIVE));
    }

    #[test]
    fn test_five() {
        let (allocator, node) = parse(FIVE);
        assert_eq!(
            node,
            Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Syscall(Syscall::Exit),
                    argument: vec![Value::Literal(Literal(1))]
                }),
                successor: Some(Successor::Child(0)),
            }
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![Node {
                    statement: Statement::Call(Call {
                        identifier: CallIdentifier::Syscall(Syscall::Exit),
                        argument: vec![Value::Literal(Literal(2))]
                    }),
                    successor: None,
                }]
            }
        );
    }

    const SIX: &str = "assign x 1\nexit 0";

    #[bench]
    fn bench_six(b: &mut Bencher) {
        b.iter(|| parse(SIX));
    }

    #[test]
    fn test_six() {
        let (allocator, node) = parse(SIX);
        assert_eq!(
            node,
            Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Intrinsic(Intrinsic::Assign),
                    argument: vec![
                        Value::Variable(Variable(vec![b'x'])),
                        Value::Literal(Literal(1))
                    ]
                }),
                successor: Some(Successor::Next(0)),
            }
        );
        let node = allocator.get(*node.successor.as_ref().unwrap().unwrap());
        assert_eq!(
            node,
            &Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Syscall(Syscall::Exit),
                    argument: vec![Value::Literal(Literal(0))]
                }),
                successor: None,
            }
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![Node {
                    statement: Statement::Call(Call {
                        identifier: CallIdentifier::Syscall(Syscall::Exit),
                        argument: vec![Value::Literal(Literal(0))]
                    }),
                    successor: None,
                }]
            }
        );
    }
    
    const SEVEN: &str = "assign x 1\nexit x";
    
    #[bench]
    fn bench_seven(b: &mut Bencher) {
        b.iter(|| parse(SEVEN));
    }

    #[test]
    fn seven() {
        let (allocator, node) = parse(SEVEN);
        assert_eq!(
            node,
            Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Intrinsic(Intrinsic::Assign),
                    argument: vec![
                        Value::Variable(Variable(Vec::from([b'x']))),
                        Value::Literal(Literal(1))
                    ]
                }),
                successor: Some(Successor::Next(0)),
            }
        );
        let node = allocator.get(*node.successor.as_ref().unwrap().unwrap());
        assert_eq!(
            node,
            &Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Syscall(Syscall::Exit),
                    argument: vec![Value::Variable(Variable(vec![b'x']))]
                }),
                successor: None,
            }
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![Node {
                    statement: Statement::Call(Call {
                        identifier: CallIdentifier::Syscall(Syscall::Exit),
                        argument: vec![Value::Variable(Variable(vec![b'x']))]
                    }),
                    successor: None,
                }]
            }
        );
    }
    
    const EIGHT: &str = "assign x 1\nadd x 1\nexit x";

    #[bench]
    fn bench_eight(b: &mut Bencher) {
        b.iter(|| parse(EIGHT));
    }

    #[test]
    fn test_eight() {
        let (allocator, node) = parse(EIGHT);
        assert_eq!(
            node,
            Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Intrinsic(Intrinsic::Assign),
                    argument: vec![
                        Value::Variable(Variable(vec![b'x'])),
                        Value::Literal(Literal(1))
                    ]
                }),
                successor: Some(Successor::Next(1)),
            }
        );
        let node = allocator.get(*node.successor.as_ref().unwrap().unwrap());
        assert_eq!(
            node,
            &Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Intrinsic(Intrinsic::Add),
                    argument: vec![
                        Value::Variable(Variable(vec![b'x'])),
                        Value::Literal(Literal(1))
                    ]
                }),
                successor: Some(Successor::Next(0)),
            }
        );
        let node = allocator.get(*node.successor.as_ref().unwrap().unwrap());
        assert_eq!(
            node,
            &Node {
                statement: Statement::Call(Call {
                    identifier: CallIdentifier::Syscall(Syscall::Exit),
                    argument: vec![Value::Variable(Variable(vec![b'x']))]
                }),
                successor: None,
            }
        );

        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![
                    Node {
                        statement: Statement::Call(Call {
                            identifier: CallIdentifier::Syscall(Syscall::Exit),
                            argument: vec![Value::Variable(Variable(vec![b'x']))]
                        }),
                        successor: None,
                    },
                    Node {
                        statement: Statement::Call(Call {
                            identifier: CallIdentifier::Intrinsic(Intrinsic::Add),
                            argument: vec![
                                Value::Variable(Variable(vec![b'x'])),
                                Value::Literal(Literal(1))
                            ]
                        }),
                        successor: Some(Successor::Next(0)),
                    },
                ]
            }
        );
    }
}
