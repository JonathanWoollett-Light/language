#![feature(test)]
#![feature(let_chains)]

extern crate test;

use std::io::Bytes;
use std::io::Read;
use std::iter::once;
use std::iter::Peekable;

#[cfg(debug_assertions)]
const LOOP_LIMIT: usize = 20;

#[allow(unreachable_code)]
fn main() {
    let empty = std::io::empty();
    let reader = std::io::BufReader::new(empty);
    let mut iter = reader.bytes().peekable();
    let nodes = get_nodes(&mut iter);
    let _assembly = assembly_from_node(&nodes);
    todo!()
}

enum GetValue {
    Value(Value),
    NewLine,
    Space,
    None,
}

fn get_identifier<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Vec<u8> {
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

fn get_value<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> GetValue {
    match bytes.peek().map(|r| r.as_ref().unwrap()) {
        Some(b'0') => {
            bytes.next().unwrap().unwrap();
            GetValue::Value(Value::Literal(Literal(0)))
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
            GetValue::Value(Value::Literal(Literal(literal)))
        }
        Some(b'a'..=b'z' | b'_') => {
            GetValue::Value(Value::Variable(Variable(get_identifier(bytes))))
        }
        Some(b' ') => GetValue::Space,
        Some(b'\n') => GetValue::NewLine,
        None => GetValue::None,
        _ => panic!(),
    }
}

fn get_values<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Vec<Value> {
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

fn get_nodes<R: Read>(bytes: &mut Peekable<Bytes<R>>) -> Vec<Node> {
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
                    _ => panic!(),
                }
            }
            // Newline
            Some(b'\n') => {
                bytes.next().unwrap().unwrap();
                continue;
            }
            // Op with assignment
            Some(_) => {
                let identifier = get_identifier(bytes);
                match identifier.as_slice() {
                    // Exit
                    b"exit" => Statement {
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
                                let GetValue::Value(value) = get_value(bytes) else {
                                    panic!()
                                };
                                match value {
                                    Value::Literal(_) => Statement {
                                        op: Op::Intrinsic(Intrinsic::Assign),
                                        arg: vec![lhs, value],
                                    },
                                    Value::Variable(Variable(variable)) => {
                                        match variable.as_slice() {
                                            b"write" => Statement {
                                                op: Op::Syscall(Syscall::Write),
                                                arg: once(lhs).chain(get_values(bytes)).collect(),
                                            },
                                            b"read" => Statement {
                                                op: Op::Syscall(Syscall::Read),
                                                arg: once(lhs).chain(get_values(bytes)).collect(),
                                            },
                                            b"memfd_create" => Statement {
                                                op: Op::Syscall(Syscall::MemfdCreate),
                                                arg: once(lhs).chain(get_values(bytes)).collect(),
                                            },
                                            _ => panic!(),
                                        }
                                    }
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

use std::fmt::Write;

fn assembly_from_node(nodes: &[Node]) -> String {
    let mut data = String::new();
    let mut bss = String::new();
    let mut block_counter = 0;
    // Have we defined an empty string in the data section to use for anonymous `memfd_create`.
    let mut empty = false;
    let assembly = instruction_from_node(
        nodes,
        0,
        &mut data,
        &mut bss,
        &mut block_counter,
        &mut empty,
    );

    let data_str = if data.is_empty() {
        String::new()
    } else {
        format!(".data\n{data}")
    };
    let bss_str = if bss.is_empty() {
        String::new()
    } else {
        format!(".bss\n{bss}")
    };

    format!(
        "\
        .global _start\n\
        _start:\n\
        {assembly}\
        {data_str}\
        {bss_str}\
    ",
    )
}

fn instruction_from_node(
    nodes: &[Node],
    index: usize,
    data: &mut String,
    bss: &mut String,
    block_counter: &mut usize,
    empty: &mut bool,
) -> String {
    let mut assembly = String::new();
    let mut current = &nodes[index];

    #[cfg(debug_assertions)]
    let mut i = 0;
    loop {
        #[cfg(debug_assertions)]
        {
            assert!(i < LOOP_LIMIT);
            i += 1;
        }

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
                            instruction_from_node(nodes, child, data, bss, block_counter, empty)
                        } else {
                            String::new()
                        }
                    )
                    .unwrap();
                    *block_counter += 1;
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Read) => match current.statement.arg.get(..) {
                Some([Value::Variable(Variable(x)), Value::Literal(Literal(fd))]) => {
                    write!(
                        &mut assembly,
                        "\
                        mov x8, #{}\n\
                        mov x0, #{fd}\n\
                        ldr x1, ={}\n\
                        mov x2, 4\n\
                        svc #0\n\
                    ",
                        libc::SYS_read,
                        std::str::from_utf8(x).unwrap()
                    )
                    .unwrap();
                    write!(
                        bss,
                        "\
                        {}:\n\
                        .skip 4\n\
                    ",
                        std::str::from_utf8(x).unwrap()
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Write) => match current.statement.arg.get(..) {
                Some(
                    [Value::Variable(Variable(v)), Value::Literal(Literal(fd)), Value::Variable(Variable(x))],
                ) if v == b"_" => {
                    write!(
                        &mut assembly,
                        "\
                        mov x8, #{}\n\
                        mov x0, #{fd}\n\
                        ldr x1, ={}\n\
                        mov x2, 4\n\
                        svc #0\n\
                    ",
                        libc::SYS_write,
                        std::str::from_utf8(x).unwrap()
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::MemfdCreate) => match current.statement.arg.get(..) {
                Some([Value::Variable(Variable(x))]) => {
                    write!(
                        &mut assembly,
                        "\
                        mov x8, #{}\n\
                        ldr x0, =empty\n\
                        mov x1, #0\n\
                        svc #0\n\
                        ldr x1, ={}\n\
                        str x0, [x1]\n\
                    ",
                        libc::SYS_memfd_create,
                        std::str::from_utf8(x).unwrap()
                    )
                    .unwrap();
                    if !*empty {
                        write!(
                            data,
                            "\
                            empty:\n\
                            .word 0\n\
                        "
                        )
                        .unwrap();
                        *empty = true;
                    }
                    write!(
                        bss,
                        "\
                        {}:\n\
                        .skip 4\n\
                    ",
                        std::str::from_utf8(x).unwrap()
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
        if let Some(next) = current.next {
            current = &nodes[next];
        } else {
            break assembly;
        }
    }
}

/// The AST maps closely to assembly for simplicity.
#[derive(Debug, Eq, PartialEq, Default)]
struct Node {
    statement: Statement,
    child: Option<usize>,
    next: Option<usize>,
}
impl Node {
    fn new(statement: Statement) -> Self {
        Self {
            statement,
            child: None,
            next: None,
        }
    }
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
    Read,
    Write,
    MemfdCreate,
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

    fn parse(text: &str) -> Vec<Node> {
        let reader = std::io::BufReader::new(text.as_bytes());
        let mut iter = reader.bytes().peekable();
        get_nodes(&mut iter)
    }

    fn assemble(nodes: &[Node], expected_assembly: &str, expected_exitcode: i32) {
        let assembly = assembly_from_node(nodes);
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
        let nodes = parse(ONE);
        assert_eq!(
            nodes,
            [Node {
                statement: Statement {
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(0))]
                },
                child: None,
                next: None,
            }]
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
        ";
        assemble(&nodes, expected_assembly, 0);
    }

    const TWO: &str = "exit 1";

    #[bench]
    fn bench_two(b: &mut Bencher) {
        b.iter(|| parse(TWO));
    }

    #[test]
    fn test_two() {
        let nodes = parse(TWO);
        assert_eq!(
            nodes,
            [Node {
                statement: Statement {
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(1))]
                },
                child: None,
                next: None,
            }]
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
        ";
        assemble(&nodes, expected_assembly, 1);
    }

    const THREE: &str = "exit 12";

    #[bench]
    fn bench_three(b: &mut Bencher) {
        b.iter(|| parse(THREE));
    }

    #[test]
    fn test_three() {
        let nodes = parse(THREE);
        assert_eq!(
            nodes,
            [Node {
                statement: Statement {
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(12))]
                },
                child: None,
                next: None,
            }]
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #12\n\
            svc #0\n\
        ";
        assemble(&nodes, expected_assembly, 12);
    }

    const FOUR: &str = "exit 1\nexit 2";

    #[bench]
    fn bench_four(b: &mut Bencher) {
        b.iter(|| parse(FOUR));
    }

    #[test]
    fn test_four() {
        let nodes = parse(FOUR);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(1))]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(2))]
                    },
                    child: None,
                    next: None,
                }
            ]
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
        assemble(&nodes, expected_assembly, 1);
    }

    const SIX: &str = "x := 1\nexit 0";

    #[bench]
    fn bench_six(b: &mut Bencher) {
        b.iter(|| parse(SIX));
    }

    #[test]
    fn test_six() {
        let nodes = parse(SIX);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(0))]
                    },
                    child: None,
                    next: None,
                }
            ]
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
        assemble(&nodes, expected_assembly, 0);
    }

    const SEVEN: &str = "x := 1\nexit x";

    #[bench]
    fn bench_seven(b: &mut Bencher) {
        b.iter(|| parse(SEVEN));
    }

    #[test]
    fn seven() {
        let nodes = parse(SEVEN);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x'])))]
                    },
                    child: None,
                    next: None,
                }
            ]
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
        assemble(&nodes, expected_assembly, 1);
    }

    const EIGHT: &str = "x := 1\nx += 1\nexit x";

    #[bench]
    fn bench_eight(b: &mut Bencher) {
        b.iter(|| parse(EIGHT));
    }

    #[test]
    fn test_eight() {
        let nodes = parse(EIGHT);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
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
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x'])))]
                    },
                    child: None,
                    next: None,
                },
            ]
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
        assemble(&nodes, expected_assembly, 2);
    }

    const NINE: &str = "x := 1\nif x = 2\n    exit 1\nexit 0";

    #[bench]
    fn bench_nine(b: &mut Bencher) {
        b.iter(|| parse(NINE));
    }

    #[test]
    fn test_nine() {
        let nodes = parse(NINE);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        op: Op::Intrinsic(Intrinsic::IfEq),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(2))
                        ]
                    },
                    child: Some(2),
                    next: Some(3),
                },
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
            ]
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
        assemble(&nodes, expected_assembly, 0);
    }

    const TEN: &str = "x := 2\nif x = 2\n    exit 1\nexit 0";

    #[bench]
    fn bench_ten(b: &mut Bencher) {
        b.iter(|| parse(TEN));
    }

    #[test]
    fn test_ten() {
        let nodes = parse(TEN);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(2))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        op: Op::Intrinsic(Intrinsic::IfEq),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(2))
                        ]
                    },
                    child: Some(2),
                    next: Some(3),
                },
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
            ]
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
        assemble(&nodes, expected_assembly, 1);
    }

    use std::mem::size_of;
    #[test]
    fn test_eleven() {
        // Create pipe and write an i32 to it.
        let mut pipe_out = [0, 0];
        let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
        assert_eq!(res, 0);
        let [read, write] = pipe_out;
        let data = 27i32;
        let bytes = data.to_ne_bytes();
        let res = unsafe { libc::write(write, bytes.as_ptr().cast(), size_of::<i32>() as _) };
        assert_eq!(res, size_of::<i32>() as _);

        // Format code
        let eleven = format!("x := read {read}\nexit x");

        // Parse code to AST
        let nodes = parse(&eleven);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Read),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(read as _))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x'])))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );

        // Parse AST to assembly
        let expected_assembly = format!(
            "\
            .global _start\n\
            _start:\n\
            mov x8, #63\n\
            mov x0, #{read}\n\
            ldr x1, =x\n\
            mov x2, 4\n\
            svc #0\n\
            mov x8, #93\n\
            ldr x0, =x\n\
            ldr x0, [x0]\n\
            svc #0\n\
            .bss\n\
            x:\n\
            .skip 4\n\
        "
        );
        assemble(&nodes, &expected_assembly, data);
        unsafe {
            libc::close(read);
            libc::close(write);
        }
    }

    #[test]
    fn test_twelve() {
        // Create pipe and write an i32 to it.
        let mut pipe_out = [0, 0];
        let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
        assert_eq!(res, 0);
        let [read, write] = pipe_out;
        let data = 27i32;
        let bytes = data.to_ne_bytes();
        let res = unsafe { libc::write(write, bytes.as_ptr().cast(), size_of::<i32>() as _) };
        assert_eq!(res, size_of::<i32>() as _);

        // Format code
        let eleven = format!("x := read {read}\n_ := write {write} x\nexit 0");

        // Parse code to AST
        let nodes = parse(&eleven);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Read),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(read as _))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Write),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'_']))),
                            Value::Literal(Literal(write as _)),
                            Value::Variable(Variable(Vec::from([b'x'])))
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(0))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );

        // Parse AST to assembly
        let expected_assembly = format!(
            "\
            .global _start\n\
            _start:\n\
            mov x8, #63\n\
            mov x0, #{read}\n\
            ldr x1, =x\n\
            mov x2, 4\n\
            svc #0\n\
            mov x8, #64\n\
            mov x0, #{write}\n\
            ldr x1, =x\n\
            mov x2, 4\n\
            svc #0\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .bss\n\
            x:\n\
            .skip 4\n\
        "
        );
        assemble(&nodes, &expected_assembly, 0);

        // Read the value from pipe
        let mut buffer = [0u8; size_of::<i32>()];
        let res = unsafe { libc::read(read, buffer.as_mut_ptr().cast(), size_of::<i32>() as _) };
        assert_eq!(res, size_of::<i32>() as _);
        assert_eq!(buffer, bytes);
    }

    const THIRTEEN: &str = "x := memfd_create\nexit 0";

    #[test]
    fn test_thirteen() {
        // Parse code to AST
        let nodes = parse(THIRTEEN);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::MemfdCreate),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x']))),]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(0))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );

        // Parse AST to assembly
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #279\n\
            ldr x0, =empty\n\
            mov x1, #0\n\
            svc #0\n\
            ldr x1, =x\n\
            str x0, [x1]\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .data\n\
            empty:\n\
            .word 0\n\
            .bss\n\
            x:\n\
            .skip 4\n\
        ";
        assemble(&nodes, expected_assembly, 0);
    }
}
