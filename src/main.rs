#![feature(test)]
#![feature(let_chains)]
#![feature(int_roundings)]
#![feature(if_let_guard)]
#![feature(iter_intersperse)]
#![feature(box_patterns)]
#![feature(exclusive_range_pattern)]
#![feature(array_chunks)]

extern crate test;

use std::io::Read;

mod ast;
#[allow(unused_imports)]
use ast::*;
mod frontend;
use frontend::*;
mod middle;
use middle::*;
mod backend;
use backend::*;

#[cfg(debug_assertions)]
const LOOP_LIMIT: usize = 200;

#[allow(unreachable_code)]
fn main() {
    let empty = std::io::empty();
    let reader = std::io::BufReader::new(empty);
    let mut iter = reader.bytes().peekable();
    let nodes = get_nodes(&mut iter);
    let optimized_nodes = optimize(&nodes);
    // let nodes = optimize_nodes(&nodes);
    let _assembly = assembly_from_node(&optimized_nodes);
    todo!()
}

// use tracing_subscriber::fmt::format::FmtSpan;
// tracing_subscriber::fmt::fmt()
//     .with_max_level(tracing_subscriber::filter::LevelFilter::TRACE)
//     .with_span_events(FmtSpan::ENTER)
//     .init();

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::remove_file;
    use std::fs::OpenOptions;
    use std::io::Write;
    use std::mem::size_of;
    use std::process::Command;
    use uuid::Uuid;

    fn parse(text: &str) -> Vec<Node> {
        let reader = std::io::BufReader::new(text.as_bytes());
        let mut iter = reader.bytes().peekable();
        get_nodes(&mut iter)
    }

    fn assemble(nodes: &[Node], expected_assembly: &str, expected_exitcode: i32) {
        let assembly = assembly_from_node(nodes);

        // for (a, b) in assembly.chars().zip(expected_assembly.chars()) {
        //     if a != b {
        //         println!("{a} != {b}");
        //     }
        // }
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

    impl Variable {
        pub fn new(s: &str) -> Self {
            Self {
                identifier: Vec::from(s.as_bytes()),
                index: None,
            }
        }
    }

    #[test]
    fn one() {
        let nodes = parse(ONE);
        assert_eq!(
            nodes,
            [Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))]
                },
                child: None,
                next: None,
            }]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))]
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
        assemble(&optimized_nodes, expected_assembly, 0);
    }

    const TWO: &str = "exit 1";

    #[test]
    fn two() {
        let nodes = parse(TWO);
        assert_eq!(
            nodes,
            [Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))]
                },
                child: None,
                next: None,
            }]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))]
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
        assemble(&optimized_nodes, expected_assembly, 1);
    }

    const THREE: &str = "exit 12";

    #[test]
    fn three() {
        let nodes = parse(THREE);
        assert_eq!(
            nodes,
            [Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(12))]
                },
                child: None,
                next: None,
            }]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(12))]
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
        assemble(&optimized_nodes, expected_assembly, 12);
    }

    const FOUR: &str = "exit 1\nexit 2";

    #[test]
    fn four() {
        let nodes = parse(FOUR);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(1))]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(2))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))]
                },
                child: None,
                next: None,
            },]
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
        ";
        assemble(&optimized_nodes, expected_assembly, 1);
    }

    const SIX: &str = "x := 1\nexit 0";

    #[test]
    fn six() {
        let nodes = parse(SIX);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Type(Type::U8),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
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
            x: .byte 1\n\
        ";
        assemble(&optimized_nodes, expected_assembly, 0);
    }

    const SEVEN: &str = "x := 1\nexit x";

    #[test]
    fn seven() {
        let nodes = parse(SEVEN);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable::new("x"))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Type(Type::U8),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable::new("x"))]
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
            ldrb w0, [x0]\n\
            svc #0\n\
            .data\n\
            x: .byte 1\n\
        ";
        assemble(&optimized_nodes, expected_assembly, 1);
    }

    const EIGHT: &str = "x := 1\nx += 1\nexit x";

    #[test]
    fn eight() {
        let nodes = parse(EIGHT);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::AddAssign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable::new("x"))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Type(Type::U8),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::AddAssign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable::new("x"))]
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
            ldr w1, [x0]\n\
            add w1, w1, #1\n\
            strb w1, [x0]\n\
            mov x8, #93\n\
            ldr x0, =x\n\
            ldrb w0, [x0]\n\
            svc #0\n\
            .data\n\
            x: .byte 1\n\
        ";
        assemble(&optimized_nodes, expected_assembly, 2);
    }

    const NINE: &str = "x := 1\nif x = 2\n    exit 1\nexit 0";

    #[test]
    fn nine() {
        let nodes = parse(NINE);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(2))
                        ]
                    },
                    child: Some(2),
                    next: Some(3),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(1))]
                    },
                    child: None,
                    next: None,
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Type(Type::U8),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .data\n\
            x: .byte 1\n\
        ";
        assemble(&optimized_nodes, expected_assembly, 0);
    }

    const TEN: &str = "x := 2\nif x = 2\n    exit 1\nexit 0";

    #[test]
    fn ten() {
        let nodes = parse(TEN);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(2))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(2))
                        ]
                    },
                    child: Some(2),
                    next: Some(3),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(1))]
                    },
                    child: None,
                    next: None,
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Type(Type::U8),
                            Value::Literal(Literal::Integer(2))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(1))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
            .data\n\
            x: .byte 2\n\
        ";
        assemble(&optimized_nodes, expected_assembly, 1);
    }

    #[test]
    fn eleven() {
        // Create pipe and write an i32 to it.
        let mut pipe_out = [0, 0];
        let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
        assert_eq!(res, 0);
        let [read, write] = pipe_out;
        let data = 27u8;
        let bytes = data.to_ne_bytes();
        let res = unsafe { libc::write(write, bytes.as_ptr().cast(), size_of::<u8>() as _) };
        assert_eq!(res, size_of::<u8>() as _);

        // Format code
        let eleven = format!("x := read {read}\nexit x");

        // Parse code to AST
        let nodes = parse(&eleven);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Read),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(read as _)),
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable::new("x"))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![Value::Variable(Variable::new("x")), Value::Type(Type::U8)]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Read),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(read as _)),
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable::new("x"))]
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
            mov x2, #1\n\
            svc #0\n\
            mov x8, #93\n\
            ldr x0, =x\n\
            ldrb w0, [x0]\n\
            svc #0\n\
            .bss\n\
            x:\n\
            .skip 1\n\
        "
        );
        assemble(&optimized_nodes, &expected_assembly, data as i32);
        unsafe {
            libc::close(read);
            libc::close(write);
        }
    }

    #[test]
    fn twelve() {
        // Create pipe and write an i32 to it.
        let mut pipe_out = [0, 0];
        let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
        assert_eq!(res, 0);
        let [read, write] = pipe_out;
        let data = 27u8;
        let bytes = data.to_ne_bytes();
        let res = unsafe { libc::write(write, bytes.as_ptr().cast(), size_of::<u8>() as _) };
        assert_eq!(res, size_of::<u8>() as _);

        // Format code
        let eleven = format!("x := read {read}\n_ := write {write} x\nexit 0");

        // Parse code to AST
        let nodes = parse(&eleven);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Read),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(read as _))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Write),
                        arg: vec![
                            Value::Variable(Variable::new("_")),
                            Value::Literal(Literal::Integer(write as _)),
                            Value::Variable(Variable::new("x"))
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![Value::Variable(Variable::new("x")), Value::Type(Type::U8)]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Read),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(read as _)),
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Write),
                        arg: vec![
                            Value::Variable(Variable::new("_")),
                            Value::Literal(Literal::Integer(write as _)),
                            Value::Variable(Variable::new("x")),
                        ]
                    },
                    child: None,
                    next: Some(3),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
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
            mov x2, #1\n\
            svc #0\n\
            mov x8, #64\n\
            mov x0, #{write}\n\
            ldr x1, =x\n\
            mov x2, #1\n\
            svc #0\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .bss\n\
            x:\n\
            .skip 1\n\
        "
        );
        assemble(&optimized_nodes, &expected_assembly, 0);

        // Read the value from pipe
        let mut buffer = [0u8; size_of::<u8>()];
        let res = unsafe { libc::read(read, buffer.as_mut_ptr().cast(), size_of::<u8>() as _) };
        assert_eq!(res, size_of::<u8>() as _);
        assert_eq!(buffer, bytes);
        unsafe {
            libc::close(read);
            libc::close(write);
        }
    }

    const THIRTEEN: &str = "x := memfd_create\nexit 0";

    #[test]
    fn thirteen() {
        // Parse code to AST
        let nodes = parse(THIRTEEN);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::MemfdCreate),
                        arg: vec![Value::Variable(Variable::new("x")),]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![Value::Variable(Variable::new("x")), Value::Type(Type::I32)]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::MemfdCreate),
                        arg: vec![Value::Variable(Variable::new("x")),]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
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
            str w0, [x1]\n\
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
        assemble(&optimized_nodes, expected_assembly, 0);
    }

    const FOURTEEN: &str = "\
        x := 0\n\
        x -= 1\n\
        if x < 0\n    require x >= -128\n    exit 1\n\
        require x <= 255\n\
        exit 0\n\
    ";

    #[test]
    fn fourteen() {
        let nodes = parse(FOURTEEN);

        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(0)),
                        ],
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::SubAssign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1)),
                        ],
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::If(Cmp::Lt)),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(0)),
                        ],
                    },
                    child: Some(3),
                    next: Some(5),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Require(Cmp::Ge)),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(-128)),
                        ],
                    },
                    child: None,
                    next: Some(4),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(1))],
                    },
                    child: None,
                    next: None,
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Require(Cmp::Le)),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(255)),
                        ],
                    },
                    child: None,
                    next: Some(6),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))],
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Type(Type::U8),
                            Value::Literal(Literal::Integer(0))
                        ]
                    },
                    child: None,
                    next: Some(1)
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::SubAssign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1))
                        ]
                    },
                    child: None,
                    next: Some(2)
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None
                }
            ]
        );
        let expected_assembly = "\
            .global _start\n\
            _start:\n\
            ldr x0, =x\n\
            ldr w1, [x0]\n\
            sub w1, w1, #1\n\
            strb w1, [x0]\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .data\n\
            x: .byte 0\n\
        ";
        assemble(&optimized_nodes, expected_assembly, 0);
    }

    const LOOP_ONE: &str = "\
        x := 0\n\
        loop\n\
        \x20   x += 1\n\
        \x20   if x = 3\n\
        \x20       break\n\
        exit 0\n\
    ";

    #[test]
    fn loop_one() {
        let nodes = parse(LOOP_ONE);

        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(0)),
                        ],
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Loop),
                        arg: vec![],
                    },
                    child: Some(2),
                    next: Some(5),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::AddAssign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1)),
                        ],
                    },
                    child: None,
                    next: Some(3),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(3)),
                        ],
                    },
                    child: Some(4),
                    next: None,
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Break),
                        arg: vec![],
                    },
                    child: None,
                    next: None,
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0)),],
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Type(Type::U8),
                            Value::Literal(Literal::Integer(0)),
                        ],
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Loop),
                        arg: vec![],
                    },
                    child: Some(2),
                    next: Some(4),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::AddAssign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(1)),
                        ],
                    },
                    child: None,
                    next: Some(3),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(3)),
                        ],
                    },
                    child: Some(5),
                    next: None,
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Break),
                        arg: vec![],
                    },
                    child: None,
                    next: None,
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0)),],
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
            ldr w1, [x0]\n\
            sub w1, w1, #1\n\
            strb w1, [x0]\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .data\n\
            x: .byte 0\n\
        ";
        assemble(&optimized_nodes, expected_assembly, 0);
    }

    #[test]
    fn hello_world_arr() {
        // Create pipe
        let mut pipe_out = [0, 0];
        let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
        assert_eq!(res, 0);
        let [read, write] = pipe_out;

        let hello_world = format!(
            "x := 72 101 108 108 111 44 32 87 111 114 108 100 33 10\n_ := write {write} x\nexit 0"
        );
        // Parse code to AST
        let nodes = parse(&hello_world);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::Integer(72)),
                            Value::Literal(Literal::Integer(101)),
                            Value::Literal(Literal::Integer(108)),
                            Value::Literal(Literal::Integer(108)),
                            Value::Literal(Literal::Integer(111)),
                            Value::Literal(Literal::Integer(44)),
                            Value::Literal(Literal::Integer(32)),
                            Value::Literal(Literal::Integer(87)),
                            Value::Literal(Literal::Integer(111)),
                            Value::Literal(Literal::Integer(114)),
                            Value::Literal(Literal::Integer(108)),
                            Value::Literal(Literal::Integer(100)),
                            Value::Literal(Literal::Integer(33)),
                            Value::Literal(Literal::Integer(10)),
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Write),
                        arg: vec![
                            Value::Variable(Variable::new("_")),
                            Value::Literal(Literal::Integer(write as _)),
                            Value::Variable(Variable::new("x")),
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Type(Type::Array(Box::new(Array {
                                item: Type::U8,
                                len: 14
                            }))),
                            Value::Literal(Literal::Integer(72)),
                            Value::Literal(Literal::Integer(101)),
                            Value::Literal(Literal::Integer(108)),
                            Value::Literal(Literal::Integer(108)),
                            Value::Literal(Literal::Integer(111)),
                            Value::Literal(Literal::Integer(44)),
                            Value::Literal(Literal::Integer(32)),
                            Value::Literal(Literal::Integer(87)),
                            Value::Literal(Literal::Integer(111)),
                            Value::Literal(Literal::Integer(114)),
                            Value::Literal(Literal::Integer(108)),
                            Value::Literal(Literal::Integer(100)),
                            Value::Literal(Literal::Integer(33)),
                            Value::Literal(Literal::Integer(10)),
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Write),
                        arg: vec![
                            Value::Variable(Variable::new("_")),
                            Value::Literal(Literal::Integer(write as _)),
                            Value::Variable(Variable::new("x")),
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
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
            mov x8, #64\n\
            mov x0, #{write}\n\
            ldr x1, =x\n\
            mov x2, #14\n\
            svc #0\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .data\n\
            x: .byte 72,101,108,108,111,44,32,87,111,114,108,100,33,10\n\
        "
        );
        assemble(&optimized_nodes, &expected_assembly, 0);

        // Read the value from pipe
        let mut buffer = [0u8; 14];
        let res = unsafe { libc::read(read, buffer.as_mut_ptr().cast(), 14) };
        assert_eq!(res, 14);
        assert_eq!(std::str::from_utf8(&buffer).unwrap(), "Hello, World!\n");
        unsafe {
            libc::close(read);
            libc::close(write);
        }
    }

    #[test]
    fn hello_world_str() {
        // Create pipe
        let mut pipe_out = [0, 0];
        let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
        assert_eq!(res, 0);
        let [read, write] = pipe_out;

        let hello_world = format!("x := \"Hello, World!\n\"\n_ := write {write} x\nexit 0");
        // Parse code to AST
        let nodes = parse(&hello_world);
        assert_eq!(
            nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::Assign),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Literal(Literal::String(String::from("Hello, World!\n"))),
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Write),
                        arg: vec![
                            Value::Variable(Variable::new("_")),
                            Value::Literal(Literal::Integer(write as _)),
                            Value::Variable(Variable::new("x")),
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Special(Special::Type),
                        arg: vec![
                            Value::Variable(Variable::new("x")),
                            Value::Type(Type::Array(Box::new(Array {
                                item: Type::U8,
                                len: 14
                            }))),
                            Value::Literal(Literal::String(String::from("Hello, World!\n")))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Write),
                        arg: vec![
                            Value::Variable(Variable::new("_")),
                            Value::Literal(Literal::Integer(write as _)),
                            Value::Variable(Variable::new("x")),
                        ]
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal::Integer(0))]
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
            mov x8, #64\n\
            mov x0, #{write}\n\
            ldr x1, =x\n\
            mov x2, #14\n\
            svc #0\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .data\n\
            x: .byte 72,101,108,108,111,44,32,87,111,114,108,100,33,10\n\
        "
        );
        assemble(&optimized_nodes, &expected_assembly, 0);

        // Read the value from pipe
        let mut buffer = [0u8; 14];
        let res = unsafe { libc::read(read, buffer.as_mut_ptr().cast(), 14) };
        assert_eq!(res, 14);
        assert_eq!(std::str::from_utf8(&buffer).unwrap(), "Hello, World!\n");
        unsafe {
            libc::close(read);
            libc::close(write);
        }
    }
}

#[cfg(feature = "false")]
#[cfg(test)]
mod tests {
    // `target` is an integer in the range -10^9..10^9.
    // `nums` is a set of integers each in the range -10^9..10^9.
    // `length` is an integer in the range 2..10^4 that gives the number of values in `nums`.
    //
    // `target`, `length` and `nums` are written to the input pipe (in order).
    //
    // If any 2 values in `nums` are found to add to equal `target` write the indicies of these values to
    // the output pipe and exit with the exit code 0, else exit with the exit code 1.
    #[allow(unused_variables)]
    #[test]
    fn test_twosum() {
        use tracing_subscriber::fmt::format::FmtSpan;
        tracing_subscriber::fmt::fmt()
            .with_max_level(tracing_subscriber::filter::LevelFilter::DEBUG)
            .with_span_events(FmtSpan::ENTER)
            .init();

        // Create input pipe
        let mut input_pipe_out = [0, 0];
        let res = unsafe { libc::pipe(input_pipe_out.as_mut_ptr()) };
        assert_eq!(res, 0);
        let [input_read, input_write] = input_pipe_out;

        // Create output pipe
        let mut output_pipe_out = [0, 0];
        let res = unsafe { libc::pipe(output_pipe_out.as_mut_ptr()) };
        assert_eq!(res, 0);
        let [output_read, output_write] = output_pipe_out;

        let hello_world = format!(
            r#"target_min := -1000000000
target := read {input_read}
length := read {input_read}

# Allocate memory
fd := memfd_create
_ := ftruncate fd length
mem := mmap fd length

buf_len := 10
len := buf_len / length
rem := buf_len % length
index := 0

# Handler full buffers.
loop
    if len = 0
        break
    len -= 1

    buf := read {input_read}
    pos := 0
    loop
        if pos = buf_len
            break

        diff := buf[pos] - target
        diff_offset := diff + target_min

        if mem[diff_offset] > -1
            _ := write {output_write} mem[diff_offset]
            _ := write {output_write} index
            exit 0
        buff_offset := buf[pos] + target_min
        mem[buff_offset] := index

        pos += 1
        index += 1

# Handle partial buffer remainder.
buf[..rem] := read {input_read}
pos := 0
loop
    if pos = rem
        break
    diff := buf[pos] - target
    diff_offset := diff + target_min
    if mem[diff_offset] > -1
        _ := write {output_write} mem[diff_offset]
        _ := write {output_write} index
        exit 0
    buff_offset := buf[pos] + target_min
    mem[buff_offset] := index

    pos += 1
    index += 1

# Exit with 1 if no 2 values found that sum to target.
exit 1"#
        );
        // Parse code to AST
        let nodes = parse(&hello_world);
        let expected_nodes = [
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("target_min")),
                        Value::Literal(Literal::Integer(-1000000000)),
                    ],
                },
                child: None,
                next: Some(1),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("target")),
                        Value::Literal(Literal::Integer(input_read as _)),
                    ],
                },
                child: None,
                next: Some(2),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("length")),
                        Value::Literal(Literal::Integer(input_read as _)),
                    ],
                },
                child: None,
                next: Some(3),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::MemfdCreate),
                    arg: vec![Value::Variable(Variable::new("fd"))],
                },
                child: None,
                next: Some(4),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::FTruncate),
                    arg: vec![
                        Value::Variable(Variable::new("_")),
                        Value::Variable(Variable::new("fd")),
                        Value::Variable(Variable::new("length")),
                    ],
                },
                child: None,
                next: Some(5),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Mmap),
                    arg: vec![
                        Value::Variable(Variable::new("mem")),
                        Value::Variable(Variable::new("fd")),
                        Value::Variable(Variable::new("length")),
                    ],
                },
                child: None,
                next: Some(6),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("buf_len")),
                        Value::Literal(Literal::Integer(10)),
                    ],
                },
                child: None,
                next: Some(7),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Div),
                    arg: vec![
                        Value::Variable(Variable::new("len")),
                        Value::Variable(Variable::new("buf_len")),
                        Value::Variable(Variable::new("length")),
                    ],
                },
                child: None,
                next: Some(8),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Rem),
                    arg: vec![
                        Value::Variable(Variable::new("rem")),
                        Value::Variable(Variable::new("buf_len")),
                        Value::Variable(Variable::new("length")),
                    ],
                },
                child: None,
                next: Some(9),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("index")),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                child: None,
                next: Some(10),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Loop),
                    arg: Vec::new(),
                },
                child: Some(11),
                next: Some(29),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::IfEq),
                    arg: vec![
                        Value::Variable(Variable::new("len")),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                child: Some(12),
                next: Some(13),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Break),
                    arg: Vec::new(),
                },
                child: None,
                next: None,
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::SubAssign),
                    arg: vec![
                        Value::Variable(Variable::new("len")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                child: None,
                next: Some(14),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("buf")),
                        Value::Literal(Literal::Integer(input_read as _)),
                    ],
                },
                child: None,
                next: Some(15),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("pos")),
                        Value::Literal(Literal::Integer(0 as _)),
                    ],
                },
                child: None,
                next: Some(16),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Loop),
                    arg: Vec::new(),
                },
                child: Some(17),
                next: None,
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::IfEq),
                    arg: vec![
                        Value::Variable(Variable::new("pos")),
                        Value::Variable(Variable::new("buf_len")),
                    ],
                },
                child: Some(18),
                next: Some(19),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Break),
                    arg: Vec::new(),
                },
                child: None,
                next: None,
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Sub),
                    arg: vec![
                        Value::Variable(Variable::new("diff")),
                        Value::Variable(Variable {
                            identifier: vec![b'b', b'u', b'f'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "pos",
                            ))))),
                        }),
                        Value::Variable(Variable::new("target")),
                    ],
                },
                child: None,
                next: Some(20),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Add),
                    arg: vec![
                        Value::Variable(Variable::new("diff_offset")),
                        Value::Variable(Variable::new("diff")),
                        Value::Variable(Variable::new("target_min")),
                    ],
                },
                child: None,
                next: Some(21),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::IfGt),
                    arg: vec![
                        Value::Variable(Variable {
                            identifier: vec![b'm', b'e', b'm'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "diff_offset",
                            ))))),
                        }),
                        Value::Literal(Literal::Integer(-1)),
                    ],
                },
                child: Some(22),
                next: Some(25),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Write),
                    arg: vec![
                        Value::Variable(Variable::new("_")),
                        Value::Literal(Literal::Integer(output_write as _)),
                        Value::Variable(Variable {
                            identifier: vec![b'm', b'e', b'm'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "diff_offset",
                            ))))),
                        }),
                    ],
                },
                child: None,
                next: Some(23),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Write),
                    arg: vec![
                        Value::Variable(Variable::new("_")),
                        Value::Literal(Literal::Integer(output_write as _)),
                        Value::Variable(Variable::new("index")),
                    ],
                },
                child: None,
                next: Some(24),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                child: None,
                next: None,
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Add),
                    arg: vec![
                        Value::Variable(Variable::new("buff_offset")),
                        Value::Variable(Variable {
                            identifier: vec![b'b', b'u', b'f'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "pos",
                            ))))),
                        }),
                        Value::Variable(Variable::new("target_min")),
                    ],
                },
                child: None,
                next: Some(26),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable {
                            identifier: vec![b'm', b'e', b'm'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "buff_offset",
                            ))))),
                        }),
                        Value::Variable(Variable::new("index")),
                    ],
                },
                child: None,
                next: Some(27),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AddAssign),
                    arg: vec![
                        Value::Variable(Variable::new("pos")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                child: None,
                next: Some(28),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AddAssign),
                    arg: vec![
                        Value::Variable(Variable::new("index")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                child: None,
                next: None,
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable {
                            identifier: vec![b'b', b'u', b'f'],
                            index: Some(Box::new(Index::Slice(Slice {
                                start: None,
                                stop: Some(Offset::Variable(Variable::new("rem"))),
                            }))),
                        }),
                        Value::Literal(Literal::Integer(input_read as _)),
                    ],
                },
                child: None,
                next: Some(30),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("pos")),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                child: None,
                next: Some(31),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Loop),
                    arg: Vec::new(),
                },
                child: Some(32),
                next: Some(44),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::IfEq),
                    arg: vec![
                        Value::Variable(Variable::new("pos")),
                        Value::Variable(Variable::new("rem")),
                    ],
                },
                child: Some(33),
                next: Some(34),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Break),
                    arg: Vec::new(),
                },
                child: None,
                next: None,
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Sub),
                    arg: vec![
                        Value::Variable(Variable::new("diff")),
                        Value::Variable(Variable {
                            identifier: vec![b'b', b'u', b'f'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "pos",
                            ))))),
                        }),
                        Value::Variable(Variable::new("target")),
                    ],
                },
                child: None,
                next: Some(35),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Add),
                    arg: vec![
                        Value::Variable(Variable::new("diff_offset")),
                        Value::Variable(Variable::new("diff")),
                        Value::Variable(Variable::new("target_min")),
                    ],
                },
                child: None,
                next: Some(36),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::IfGt),
                    arg: vec![
                        Value::Variable(Variable {
                            identifier: vec![b'm', b'e', b'm'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "diff_offset",
                            ))))),
                        }),
                        Value::Literal(Literal::Integer(-1)),
                    ],
                },
                child: Some(37),
                next: Some(40),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Write),
                    arg: vec![
                        Value::Variable(Variable::new("_")),
                        Value::Literal(Literal::Integer(output_write as _)),
                        Value::Variable(Variable {
                            identifier: vec![b'm', b'e', b'm'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "diff_offset",
                            ))))),
                        }),
                    ],
                },
                child: None,
                next: Some(38),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Write),
                    arg: vec![
                        Value::Variable(Variable::new("_")),
                        Value::Literal(Literal::Integer(output_write as _)),
                        Value::Variable(Variable::new("index")),
                    ],
                },
                child: None,
                next: Some(39),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                child: None,
                next: None,
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Add),
                    arg: vec![
                        Value::Variable(Variable::new("buff_offset")),
                        Value::Variable(Variable {
                            identifier: vec![b'b', b'u', b'f'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "pos",
                            ))))),
                        }),
                        Value::Variable(Variable::new("target_min")),
                    ],
                },
                child: None,
                next: Some(41),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable {
                            identifier: vec![b'm', b'e', b'm'],
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new(
                                "buff_offset",
                            ))))),
                        }),
                        Value::Variable(Variable::new("index")),
                    ],
                },
                child: None,
                next: Some(42),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AddAssign),
                    arg: vec![
                        Value::Variable(Variable::new("pos")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                child: None,
                next: Some(43),
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AddAssign),
                    arg: vec![
                        Value::Variable(Variable::new("index")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                child: None,
                next: None,
            },
            Node {
                statement: Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
                child: None,
                next: None,
            },
        ];
        for (a, b) in nodes.iter().zip(expected_nodes.iter()) {
            assert_eq!(a, b);
        }
        // assert_eq!(nodes[..expected_nodes.len()], expected_nodes);
        let optimized_nodes = optimize_nodes(&nodes);
        // assert_eq!(
        //     optimized_nodes,
        //     [
        //         Node {
        //             statement: Statement {
        //                 comptime: false,
        //                 op: Op::Intrinsic(Intrinsic::Assign),
        //                 arg: vec![
        //                     Value::Variable(Variable::new("x")),
        //                     Value::Literal(Literal::String(String::from("Hello, World!\n"))),
        //                 ]
        //             },
        //             child: None,
        //             next: Some(1),
        //         },
        //         Node {
        //             statement: Statement {
        //                 comptime: false,
        //                 op: Op::Syscall(Syscall::Write),
        //                 arg: vec![
        //                     Value::Variable(Variable::new("_")),
        //                     Value::Literal(Literal::Integer(write as _)),
        //                     Value::Variable(Variable::new("x")),
        //                     Value::Literal(Literal::Integer(14))
        //                 ]
        //             },
        //             child: None,
        //             next: Some(2),
        //         },
        //         Node {
        //             statement: Statement {
        //                 comptime: false,
        //                 op: Op::Syscall(Syscall::Exit),
        //                 arg: vec![Value::Literal(Literal::Integer(0))]
        //             },
        //             child: None,
        //             next: None,
        //         }
        //     ]
        // );

        // // Parse AST to assembly
        // let expected_assembly = format!(
        //     "\
        //     .global _start\n\
        //     _start:\n\
        //     mov x8, #64\n\
        //     mov x0, #{input_read}\n\
        //     ldr x1, =x\n\
        //     mov x2, #14\n\
        //     svc #0\n\
        //     mov x8, #93\n\
        //     mov x0, #0\n\
        //     svc #0\n\
        //     .data\n\
        //     x:\n\
        //     .byte 72,101,108,108,111,44,32,87,111,114,108,100,33,10\n\
        // "
        // );
        // assemble(&optimized_nodes, &expected_assembly, 0);

        // // Read the value from pipe
        // let expected_out = b"Hello, World!\n";
        // let mut buffer = [0u8; 14];
        // let res = unsafe { libc::read(output_read, buffer.as_mut_ptr().cast(), 14) };
        // assert_eq!(res, 14);
        // assert_eq!(&buffer, expected_out);
        // unsafe {
        //     libc::close(input_read);
        //     libc::close(input_write);
        //     libc::close(output_read);
        //     libc::close(output_write);
        // }
    }
}
