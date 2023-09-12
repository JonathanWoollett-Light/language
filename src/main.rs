#![feature(test)]
#![feature(let_chains)]
#![feature(int_roundings)]
#![feature(if_let_guard)]

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
const LOOP_LIMIT: usize = 20;

#[allow(unreachable_code)]
fn main() {
    let empty = std::io::empty();
    let reader = std::io::BufReader::new(empty);
    let mut iter = reader.bytes().peekable();
    let nodes = get_nodes(&mut iter);
    let nodes = optimize_nodes(&nodes);
    let _assembly = assembly_from_node(&nodes);
    todo!()
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
                    runtime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(0))]
                },
                child: None,
                next: None,
            }]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [Node {
                statement: Statement {
                    runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 0);
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
                    runtime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(1))]
                },
                child: None,
                next: None,
            }]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [Node {
                statement: Statement {
                    runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 1);
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
                    runtime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal(12))]
                },
                child: None,
                next: None,
            }]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [Node {
                statement: Statement {
                    runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 12);
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(1))]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(2))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(1))]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 1);
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
                        runtime: false,
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(0))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        runtime: false,
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
                        runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 0);
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
                        runtime: false,
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x'])))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        runtime: false,
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
                        runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 1);
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
                        runtime: false,
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
                        runtime: false,
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x'])))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        runtime: false,
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
                        runtime: false,
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
                        runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 2);
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
                        runtime: false,
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
                        runtime: false,
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(1))]
                    },
                    child: None,
                    next: None,
                },
                Node {
                    statement: Statement {
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(0))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        runtime: false,
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
                        runtime: false,
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(1))]
                    },
                    child: None,
                    next: None,
                },
                Node {
                    statement: Statement {
                        runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 0);
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
                        runtime: false,
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
                        runtime: false,
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(1))]
                    },
                    child: None,
                    next: None,
                },
                Node {
                    statement: Statement {
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(0))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        runtime: false,
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
                        runtime: false,
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(1))]
                    },
                    child: None,
                    next: None,
                },
                Node {
                    statement: Statement {
                        runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 1);
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Read),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(read as _)),
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x'])))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        runtime: false,
                        op: Op::Syscall(Syscall::Read),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(read as _)),
                            Value::Literal(Literal(4))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        runtime: false,
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
            mov x2, #4\n\
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
        assemble(&optimized_nodes, &expected_assembly, data);
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
                        runtime: false,
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
                        runtime: false,
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
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(0))]
                    },
                    child: None,
                    next: None,
                },
            ]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        runtime: false,
                        op: Op::Syscall(Syscall::Read),
                        arg: vec![
                            Value::Variable(Variable(Vec::from([b'x']))),
                            Value::Literal(Literal(read as _)),
                            Value::Literal(Literal(4))
                        ]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        runtime: false,
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
                        runtime: false,
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
            mov x2, #4\n\
            svc #0\n\
            mov x8, #64\n\
            mov x0, #{write}\n\
            ldr x1, =x\n\
            mov x2, #4\n\
            svc #0\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
            .bss\n\
            x:\n\
            .skip 4\n\
        "
        );
        assemble(&optimized_nodes, &expected_assembly, 0);

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
                        runtime: false,
                        op: Op::Syscall(Syscall::MemfdCreate),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x']))),]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        runtime: false,
                        op: Op::Syscall(Syscall::Exit),
                        arg: vec![Value::Literal(Literal(0))]
                    },
                    child: None,
                    next: None,
                }
            ]
        );
        let optimized_nodes = optimize_nodes(&nodes);
        assert_eq!(
            optimized_nodes,
            [
                Node {
                    statement: Statement {
                        runtime: false,
                        op: Op::Syscall(Syscall::MemfdCreate),
                        arg: vec![Value::Variable(Variable(Vec::from([b'x']))),]
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        runtime: false,
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
        assemble(&optimized_nodes, expected_assembly, 0);
    }
}
