#![feature(test)]
extern crate test;

#[allow(unreachable_code)]
fn main() {
    let mut allocator = SimpleAllocator::new();
    let _ = node_from_bytes(b"", &mut allocator, 0);
    todo!()
}

fn next_from_bytes(
    bytes: &[u8],
    allocator: &mut SimpleAllocator,
    depth: u8,
) -> (Option<AllocatorKey>, usize) {
    let (node_opt, distance) = node_from_bytes(bytes, allocator, depth);
    let key_opt = node_opt.map(|node| allocator.alloc(node));
    (key_opt, distance)
}

fn call_from_bytes(
    op: Op,
    head: Vec<Value>,
    bytes: &[u8],
    allocator: &mut SimpleAllocator,
    depth: u8,
) -> (Node, usize) {
    let mut i = 0;

    let mut node = Node {
        statement: Statement { op, arg: head },
        child: None,
        next: None,
    };
    let arg = &mut node.statement.arg;
    'outer: loop {
        let x = match bytes[i] {
            b'0' => {
                i += 1;
                match bytes.get(i) {
                    Some(b'\n') => {
                        arg.push(Value::Literal(Literal(0)));
                        i += 1; // Skip newline
                        let (key, distance) = next_from_bytes(&bytes[i + 1..], allocator, depth);
                        node.next = key;
                        i += distance;
                        break 'outer;
                    }
                    None => {
                        arg.push(Value::Literal(Literal(0)));
                        break 'outer;
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
                            arg.push(Value::Literal(Literal(literal)));
                            i += 1; // Skip newline
                            let (key, distance) = next_from_bytes(&bytes[i..], allocator, depth);
                            node.next = key;
                            i += distance;
                            break 'outer;
                        }
                        None => {
                            arg.push(Value::Literal(Literal(literal)));
                            break 'outer;
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
                            arg.push(Value::Variable(Variable(Vec::from(&bytes[j..i]))));
                            i += 1; // Skip newline
                            let (key, distance) = next_from_bytes(&bytes[i..], allocator, depth);
                            node.next = key;
                            i += distance;
                            break 'outer;
                        }
                        None => {
                            arg.push(Value::Variable(Variable(Vec::from(&bytes[j..i]))));
                            break 'outer;
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
        arg.push(x);
    }
    (node, i)
}

fn if_from_bytes(bytes: &[u8], allocator: &mut SimpleAllocator, depth: u8) -> (Node, usize) {
    let mut node = Node::default();

    let mut i = 0;
    let first = match bytes[i] {
        b'1'..=b'9' => {
            let mut x = (bytes[i] - b'0') as u64;
            i += 1;
            loop {
                match bytes[i] {
                    b'0'..=b'9' => {
                        x *= 10;
                        x += (bytes[i] - b'0') as u64;
                        i += 1;
                    }
                    b' ' => break Value::Literal(Literal(x)),
                    _ => panic!(),
                }
            }
        }
        b'a'..=b'z' | b'_' => {
            let j = i;
            i += 1;
            loop {
                match bytes[i] {
                    b'a'..=b'z' | b'_' => {
                        i += 1;
                    }
                    b' ' => break Value::Variable(Variable(Vec::from(&bytes[j..i]))),
                    _ => panic!(),
                }
            }
        }
        _ => panic!(),
    };
    node.statement.arg.push(first);

    // Skip the space
    i += 1;

    node.statement.op = match &bytes[i] {
        b'=' => Op::Intrinsic(Intrinsic::IfEq),
        b'>' => Op::Intrinsic(Intrinsic::IfLt),
        b'<' => Op::Intrinsic(Intrinsic::IfGt),
        _ => panic!(),
    };

    // Skip symbol
    i += 1;

    // Skip space
    i += 1;

    let second = match bytes[i] {
        b'1'..=b'9' => {
            let mut x = (bytes[i] - b'0') as u64;
            i += 1;
            loop {
                match bytes[i] {
                    b'0'..=b'9' => {
                        x *= 10;
                        x += (bytes[i] - b'0') as u64;
                        i += 1;
                    }
                    b'\n' => break Value::Literal(Literal(x)),
                    _ => panic!(),
                }
            }
        }
        b'a'..=b'z' | b'_' => {
            let j = i;
            i += 1;
            loop {
                match bytes[i] {
                    b'a'..=b'z' | b'_' => {
                        i += 1;
                    }
                    b'\n' => break Value::Variable(Variable(Vec::from(&bytes[j..i]))),
                    _ => panic!(),
                }
            }
        }
        _ => panic!(),
    };
    node.statement.arg.push(second);

    // Skip newline
    i += 1;

    let (child, child_distance) = next_from_bytes(&bytes[i..], allocator, depth + 1);
    node.child = child;
    let (next, next_distance) = next_from_bytes(&bytes[i + 1 + child_distance..], allocator, depth);
    node.next = next;

    (node, i + 1 + child_distance + next_distance)
}

fn node_from_bytes(
    bytes: &[u8],
    allocator: &mut SimpleAllocator,
    depth: u8,
) -> (Option<Node>, usize) {
    let mut i = 0;
    for _ in 0..depth {
        match &bytes[i..i + 4] {
            b"    " => {
                i += 4;
            }
            _ => return (None, i),
        }
    }
    let j = i;

    loop {
        match bytes[i] {
            b'a'..=b'z' | b'_' => {
                i += 1;
            }
            b' ' => break,
            _ => panic!(),
        }
    }

    let (node, distance) = match &bytes[j..].split_at(i - j) {
        (head, [b' ', b'+', b'=', b' ', tail @ ..]) => call_from_bytes(
            Op::Intrinsic(Intrinsic::Add),
            vec![Value::Variable(Variable(Vec::from(*head)))],
            tail,
            allocator,
            depth,
        ),
        (head, [b' ', b'-', b'=', b' ', tail @ ..]) => call_from_bytes(
            Op::Intrinsic(Intrinsic::Sub),
            vec![Value::Variable(Variable(Vec::from(*head)))],
            tail,
            allocator,
            depth,
        ),
        (head, [b' ', b'/', b'=', b' ', tail @ ..]) => call_from_bytes(
            Op::Intrinsic(Intrinsic::Div),
            vec![Value::Variable(Variable(Vec::from(*head)))],
            tail,
            allocator,
            depth,
        ),
        (head, [b' ', b'*', b'=', b' ', tail @ ..]) => call_from_bytes(
            Op::Intrinsic(Intrinsic::Mul),
            vec![Value::Variable(Variable(Vec::from(*head)))],
            tail,
            allocator,
            depth,
        ),
        (head, [b' ', b':', b'=', b' ', tail @ ..]) => call_from_bytes(
            Op::Intrinsic(Intrinsic::Assign),
            vec![Value::Variable(Variable(Vec::from(*head)))],
            tail,
            allocator,
            depth,
        ),
        ([b'e', b'x', b'i', b't'], [b' ', tail @ ..]) => call_from_bytes(
            Op::Syscall(Syscall::Exit),
            Vec::new(),
            tail,
            allocator,
            depth,
        ),
        ([b'i', b'f'], [b' ', tail @ ..]) => if_from_bytes(tail, allocator, depth),
        (head, tail) => {
            panic!(
                "Unknown {:?} and {:?}",
                std::str::from_utf8(head).unwrap(),
                std::str::from_utf8(&tail[0..8]).unwrap()
            )
        }
    };

    (Some(node), distance + i)
}

use std::fmt::Write;

fn assembly_from_node(node: &Node, allocator: &SimpleAllocator) -> String {
    let mut data = String::new();
    let mut block_counter = 0;
    let assembly = instruction_from_node(node, allocator, &mut data, &mut block_counter);

    let data_str = if data.is_empty() {
        String::new()
    } else {
        format!(".data\n{data}")
    };

    format!(
        "\
        .global _start\n\
        _start:\n\
        {assembly}\
        {data_str}\
    ",
    )
}

fn instruction_from_node(
    node: &Node,
    allocator: &SimpleAllocator,
    data: &mut String,
    block_counter: &mut usize,
) -> String {
    let mut assembly = String::new();
    let mut current = node;

    loop {
        match current.statement.op {
            Op::Syscall(Syscall::Exit) => match current.statement.arg.get(0) {
                Some(Value::Literal(Literal(x))) => write!(
                    &mut assembly,
                    "\
                    mov x8, #{}\n\
                    mov x0, #{x}\n\
                    svc #0\n\
                ",
                    libc::SYS_exit
                )
                .unwrap(),
                Some(Value::Variable(Variable(x))) => write!(
                    &mut assembly,
                    "\
                    mov x8, #{}\n\
                    ldr x0, ={}\n\
                    ldr x0, [x0]\n\
                    svc #0\n\
                ",
                    libc::SYS_exit,
                    std::str::from_utf8(x).unwrap()
                )
                .unwrap(),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Assign) => match current.statement.arg.get(..) {
                Some([Value::Variable(Variable(x)), Value::Literal(Literal(y))]) => write!(
                    data,
                    "\
                    {}:\n\
                    .word {y}\n\
                ",
                    std::str::from_utf8(x).unwrap()
                )
                .unwrap(),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Add) => match current.statement.arg.get(..) {
                Some([Value::Variable(Variable(x)), Value::Literal(Literal(y))]) => write!(
                    &mut assembly,
                    "\
                    ldr x0, ={}\n\
                    ldr x1, [x0]\n\
                    add x1, x1, #{y}\n\
                    str x1, [x0]\n\
                ",
                    std::str::from_utf8(x).unwrap()
                )
                .unwrap(),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::IfEq) => match current.statement.arg.get(..) {
                Some([Value::Variable(Variable(x)), Value::Literal(Literal(y))]) => {
                    write!(
                        &mut assembly,
                        "\
                        ldr x0, ={}\n\
                        ldr x0, [x0]\n\
                        cmp w0, #{y}\n\
                        bne block{block_counter}\n\
                        {}\
                        block{block_counter}:\n\
                    ",
                        std::str::from_utf8(x).unwrap(),
                        if let Some(child) = current.child {
                            instruction_from_node(
                                allocator.get(child).unwrap(),
                                allocator,
                                data,
                                block_counter,
                            )
                        } else {
                            String::new()
                        }
                    )
                    .unwrap();
                    *block_counter += 1;
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
        if let Some(next) = current.next {
            current = allocator.get(next).unwrap();
        } else {
            break assembly;
        }
    }
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
    fn get(&self, key: AllocatorKey) -> Option<&Node> {
        self.buffer.get(key)
    }
    fn alloc(&mut self, value: Node) -> AllocatorKey {
        let key = self.buffer.len();
        self.buffer.push(value);
        key
    }
    fn new() -> Self {
        Self { buffer: Vec::new() }
    }
}

/// The AST maps closely to assembly for simplicity.
#[derive(Debug, Eq, PartialEq, Default)]
struct Node {
    statement: Statement,
    child: Option<AllocatorKey>,
    next: Option<AllocatorKey>,
}

#[derive(Debug, Eq, PartialEq, Default)]
struct Statement {
    op: Op,
    arg: Vec<Value>,
}

#[derive(Debug, Eq, PartialEq)]
enum Value {
    Literal(Literal),
    Variable(Variable),
}

impl Default for Value {
    fn default() -> Self {
        Self::Literal(Default::default())
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
struct Literal(u64);

#[derive(Debug, Eq, PartialEq, Default)]
struct Variable(Vec<u8>);

#[derive(Debug, Eq, PartialEq, Default)]
enum Intrinsic {
    #[default]
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    // beq
    IfEq,
    // blt
    IfLt,
    // bgt
    IfGt,
}

#[derive(Debug, Eq, PartialEq, Default)]
enum Syscall {
    #[default]
    Exit,
}

#[derive(Debug, Eq, PartialEq)]
enum Op {
    Intrinsic(Intrinsic),
    Syscall(Syscall),
}

impl Default for Op {
    fn default() -> Self {
        Self::Intrinsic(Default::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::remove_file;
    use std::fs::OpenOptions;
    use std::io::Write;
    use std::process::Command;
    use test::Bencher;
    use uuid::Uuid;

    fn parse(text: &str) -> (SimpleAllocator, Option<Node>) {
        let mut allocator = SimpleAllocator::new();
        let (node, _distance) = node_from_bytes(text.as_bytes(), &mut allocator, 0);
        (allocator, node)
    }

    fn assemble(
        node: &Node,
        allocator: &SimpleAllocator,
        expected_assembly: &str,
        expected_exitcode: i32,
    ) {
        let assembly = assembly_from_node(node, allocator);
        assert_eq!(assembly, expected_assembly);
        let path = format!("/tmp/{}", Uuid::new_v4());
        let assembly_path = format!("{path}.s");
        let mut file = OpenOptions::new()
            .create(true)
            .write(true)
            .open(&assembly_path)
            .unwrap();
        file.write_all(assembly.as_bytes()).unwrap();
        let object_path = format!("{path}.o");
        let output = Command::new("as")
            .args(["-o", &object_path, &assembly_path])
            .output()
            .unwrap();
        assert_eq!(
            output.stdout,
            [],
            "{}",
            std::str::from_utf8(&output.stdout).unwrap()
        );
        assert_eq!(
            output.stderr,
            [],
            "{}",
            std::str::from_utf8(&output.stderr).unwrap()
        );
        assert_eq!(output.status.code(), Some(0));
        remove_file(assembly_path).unwrap();

        let output = Command::new("ld")
            .args(["-s", "-o", &path, &object_path])
            .output()
            .unwrap();
        assert_eq!(
            output.stdout,
            [],
            "{}",
            std::str::from_utf8(&output.stdout).unwrap()
        );
        assert_eq!(
            output.stderr,
            [],
            "{}",
            std::str::from_utf8(&output.stderr).unwrap()
        );
        assert_eq!(output.status.code(), Some(0));
        remove_file(object_path).unwrap();

        let output = Command::new(&path).output().unwrap();
        assert_eq!(
            output.stdout,
            [],
            "{}",
            std::str::from_utf8(&output.stdout).unwrap()
        );
        assert_eq!(
            output.stderr,
            [],
            "{}",
            std::str::from_utf8(&output.stderr).unwrap()
        );
        assert_eq!(output.status.code(), Some(expected_exitcode));
        remove_file(path).unwrap();
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
            Some(Node {
                statement: Statement {
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(0))]
                },
                child: None,
                next: None,
            })
        );
        assert_eq!(allocator, SimpleAllocator::new());
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
        ";
        assemble(&node.unwrap(), &allocator, expected_assembly, 0);
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
            Some(Node {
                statement: Statement {
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(1))]
                },
                child: None,
                next: None,
            })
        );
        assert_eq!(allocator, SimpleAllocator::new());
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
        ";
        assemble(&node.unwrap(), &allocator, expected_assembly, 1);
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
            Some(Node {
                statement: Statement {
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(12))]
                },
                child: None,
                next: None,
            })
        );
        assert_eq!(allocator, SimpleAllocator::new());
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #12\n\
            svc #0\n\
        ";
        assemble(&node.unwrap(), &allocator, expected_assembly, 12);
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
            Some(Node {
                statement: Statement {
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(1))]
                },
                child: None,
                next: Some(0),
            })
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(2))]
                    },
                    child: None,
                    next: None,
                }]
            }
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
            mov x8, #93\n\
            mov x0, #2\n\
            svc #0\n\
        ";
        assemble(&node.unwrap(), &allocator, expected_assembly, 1);
    }

    const SIX: &str = "x := 1\nexit 0";

    #[bench]
    fn bench_six(b: &mut Bencher) {
        b.iter(|| parse(SIX));
    }

    #[test]
    fn test_six() {
        let (allocator, node) = parse(SIX);
        assert_eq!(
            node,
            Some(Node {
                statement: Statement {
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable(Vec::from([b'x']))),
                        Value::Literal(Literal(1))
                    ]
                },
                child: None,
                next: Some(0),
            })
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(0))]
                    },
                    child: None,
                    next: None,
                }]
            }
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .data\n\
            x:\n\
            .word 1\n\
        ";
        assemble(&node.unwrap(), &allocator, expected_assembly, 0);
    }

    const SEVEN: &str = "x := 1\nexit x";

    #[bench]
    fn bench_seven(b: &mut Bencher) {
        b.iter(|| parse(SEVEN));
    }

    #[test]
    fn seven() {
        let (allocator, node) = parse(SEVEN);
        assert_eq!(
            node,
            Some(Node {
                statement: Statement {
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable(Vec::from([b'x']))),
                        Value::Literal(Literal(1))
                    ]
                },
                child: None,
                next: Some(0),
            })
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x'])))]
                    },
                    child: None,
                    next: None,
                }]
            }
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            ldr x0, =x\n\
            ldr x0, [x0]\n\
            svc #0\n\
            .data\n\
            x:\n\
            .word 1\n\
        ";
        assemble(&node.unwrap(), &allocator, expected_assembly, 1);
    }

    const EIGHT: &str = "x := 1\nx += 1\nexit x";

    #[bench]
    fn bench_eight(b: &mut Bencher) {
        b.iter(|| parse(EIGHT));
    }

    #[test]
    fn test_eight() {
        let (allocator, node) = parse(EIGHT);
        assert_eq!(
            node,
            Some(Node {
                statement: Statement {
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable(Vec::from([b'x']))),
                        Value::Literal(Literal(1))
                    ]
                },
                child: None,
                next: Some(1),
            })
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![
                    Node {
                        statement: Statement {
                            op: Op::Syscall(Syscall::Exit),
                            arg: vec![Value::Variable(Variable(Vec::from([b'x'])))]
                        },
                        child: None,
                        next: None,
                    },
                    Node {
                        statement: Statement {
                            op: Op::Intrinsic(Intrinsic::Add),
                            arg: vec![
                                Value::Variable(Variable(Vec::from([b'x']))),
                                Value::Literal(Literal(1))
                            ]
                        },
                        child: None,
                        next: Some(0),
                    },
                ]
            }
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            ldr x0, =x\n\
            ldr x1, [x0]\n\
            add x1, x1, #1\n\
            str x1, [x0]\n\
            mov x8, #93\n\
            ldr x0, =x\n\
            ldr x0, [x0]\n\
            svc #0\n\
            .data\n\
            x:\n\
            .word 1\n\
        ";
        assemble(&node.unwrap(), &allocator, expected_assembly, 2);
    }

    const NINE: &str = "x := 1\nif x = 2\n    exit 1\nexit 0";

    #[bench]
    fn bench_nine(b: &mut Bencher) {
        b.iter(|| parse(NINE));
    }

    #[test]
    fn test_nine() {
        let (allocator, node) = parse(NINE);
        assert_eq!(
            node,
            Some(Node {
                statement: Statement {
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable(Vec::from([b'x']))),
                        Value::Literal(Literal(1))
                    ]
                },
                child: None,
                next: Some(2),
            })
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![
                    Node {
                        statement: Statement {
                            op: Op::Syscall(Syscall::Exit),
                            arg: vec![Value::Literal(Literal(1))]
                        },
                        child: None,
                        next: None,
                    },
                    Node {
                        statement: Statement {
                            op: Op::Syscall(Syscall::Exit),
                            arg: vec![Value::Literal(Literal(0))]
                        },
                        child: None,
                        next: None,
                    },
                    Node {
                        statement: Statement {
                            op: Op::Intrinsic(Intrinsic::IfEq),
                            arg: vec![
                                Value::Variable(Variable(Vec::from([b'x']))),
                                Value::Literal(Literal(2))
                            ]
                        },
                        child: Some(0),
                        next: Some(1),
                    },
                ]
            }
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            ldr x0, =x\n\
            ldr x0, [x0]\n\
            cmp w0, #2\n\
            bne block0\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
            block0:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .data\n\
            x:\n\
            .word 1\n\
        ";
        assemble(&node.unwrap(), &allocator, expected_assembly, 0);
    }

    const TEN: &str = "x := 2\nif x = 2\n    exit 1\nexit 0";

    #[bench]
    fn bench_ten(b: &mut Bencher) {
        b.iter(|| parse(TEN));
    }

    #[test]
    fn test_ten() {
        let (allocator, node) = parse(TEN);
        assert_eq!(
            node,
            Some(Node {
                statement: Statement {
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable(Vec::from([b'x']))),
                        Value::Literal(Literal(2))
                    ]
                },
                child: None,
                next: Some(2),
            })
        );
        assert_eq!(
            allocator,
            SimpleAllocator {
                buffer: vec![
                    Node {
                        statement: Statement {
                            op: Op::Syscall(Syscall::Exit),
                            arg: vec![Value::Literal(Literal(1))]
                        },
                        child: None,
                        next: None,
                    },
                    Node {
                        statement: Statement {
                            op: Op::Syscall(Syscall::Exit),
                            arg: vec![Value::Literal(Literal(0))]
                        },
                        child: None,
                        next: None,
                    },
                    Node {
                        statement: Statement {
                            op: Op::Intrinsic(Intrinsic::IfEq),
                            arg: vec![
                                Value::Variable(Variable(Vec::from([b'x']))),
                                Value::Literal(Literal(2))
                            ]
                        },
                        child: Some(0),
                        next: Some(1),
                    },
                ]
            }
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            ldr x0, =x\n\
            ldr x0, [x0]\n\
            cmp w0, #2\n\
            bne block0\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
            block0:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .data\n\
            x:\n\
            .word 2\n\
        ";
        assemble(&node.unwrap(), &allocator, expected_assembly, 1);
    }
}
