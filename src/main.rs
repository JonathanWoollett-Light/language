#![feature(test)]
#![feature(let_chains)]
#![feature(int_roundings)]
#![feature(if_let_guard)]
#![feature(iter_intersperse)]
#![feature(box_patterns)]
#![feature(exclusive_range_pattern)]
#![feature(array_chunks)]

extern crate test;

use clap::Parser;
use data_encoding::HEXUPPER;
use ring::digest::{Context, Digest, SHA256};
use std::fs::OpenOptions;
use std::io::{Read, Write};
use std::path::PathBuf;

mod ast;
mod frontend;
use frontend::*;
mod middle;
use middle::*;
mod backend;
use backend::*;

#[cfg(debug_assertions)]
const LOOP_LIMIT: usize = 200;

#[derive(Parser, Debug)]
struct Args {
    #[arg(long)]
    source: Option<String>,
    #[arg(long)]
    path: Option<PathBuf>,
    #[arg(long)]
    new: Option<PathBuf>,
}

fn sha256_digest<R: Read>(mut reader: R) -> std::io::Result<Digest> {
    let mut context = Context::new(&SHA256);
    let mut buffer = [0; 1024];

    loop {
        let count = reader.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        context.update(&buffer[..count]);
    }

    Ok(context.finish())
}

const LANGUAGE_EXTENSION: &str = "abc";
const BUILD_DIR: &str = "build";

fn write_file(dir: &PathBuf, file: PathBuf, bytes: &[u8], lock_file: &mut std::fs::File) {
    // Write data
    let mut data = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(dir.join(BUILD_DIR).join(&file))
        .unwrap();
    data.write_all(bytes).unwrap();
    // Write data hash
    let hash = HEXUPPER.encode(sha256_digest(bytes).unwrap().as_ref());
    let file_name = file.file_stem().unwrap();
    writeln!(lock_file, "{},{hash}", file_name.to_str().unwrap()).unwrap();
}

#[allow(unreachable_code)]
fn main() {
    let args = Args::parse();

    if let Some(new) = args.new {
        if !new.exists() {
            std::fs::create_dir(&new).unwrap();
        }
        let mut source = OpenOptions::new()
            .create(true)
            .write(true)
            .open(new.join("source"))
            .unwrap();
        source.write_all(b"\
            include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\n\
            x := \"Hello, World!\\n\"\n\
            write 1 x\n\
            exit 0\n\
        ").unwrap();
        return;
    }

    let (source, path_opt) = if let Some(source) = args.source {
        (source, None)
    } else {
        let project_path = args.path.unwrap_or_else(|| PathBuf::from("./"));
        let source_path = project_path.join("source");
        (
            std::fs::read_to_string(source_path).unwrap(),
            Some(project_path),
        )
    };

    unsafe {
        // Includes dependencies
        let mut bytes = source.into_bytes();
        get_includes(&mut bytes);
        let mut lock = if let Some(path) = &path_opt {
            let build_dir = path.join(BUILD_DIR);
            if !build_dir.exists() {
                std::fs::create_dir(build_dir).unwrap();
            }
            let mut lock_file = OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(path.join("lock.csv"))
                .unwrap();
            writeln!(&mut lock_file, "file,hash").unwrap();

            write_file(
                path,
                PathBuf::from("included").with_extension(LANGUAGE_EXTENSION),
                &bytes,
                &mut lock_file,
            );
            Some(lock_file)
        } else {
            None
        };

        // Parses AST
        let reader = std::io::BufReader::new(bytes.as_slice());
        let mut iter = reader.bytes().peekable();
        let nodes = get_nodes(&mut iter).unwrap();

        // Inlines functions
        let inlined = inline_functions(nodes);
        if let Some(path) = &path_opt {
            let inlined_string = display_ast(inlined);
            write_file(
                path,
                PathBuf::from("inlined").with_extension(LANGUAGE_EXTENSION),
                inlined_string.as_bytes(),
                lock.as_mut().unwrap(),
            );
        }

        // Explores states
        let roots = roots(inlined);
        let mut explorer = Explorer::new(&roots);
        let path = loop {
            match explorer.next() {
                Explore::Current(_) => continue,
                Explore::Finished(x) => break x,
            }
        };

        // Optimize source
        let optimized = optimize(path);
        if let Some(path) = &path_opt {
            let optimized_string = display_ast(optimized);
            write_file(
                path,
                PathBuf::from("optimized").with_extension(LANGUAGE_EXTENSION),
                optimized_string.as_bytes(),
                lock.as_mut().unwrap(),
            );
        }

        // Construct assembly
        let assembly = assembly_from_node(optimized);
        if let Some(path) = path_opt {
            write_file(
                &path,
                PathBuf::from("assembly").with_extension("s"),
                assembly.as_bytes(),
                lock.as_mut().unwrap(),
            );
        }

        std::io::stdout().write_all(assembly.as_bytes()).unwrap();
    }
}

fn display_ast(node: std::ptr::NonNull<crate::ast::NewNode>) -> String {
    unsafe {
        use std::fmt::Write;
        let mut stack = vec![(node, 0)];
        let mut string = String::new();
        while let Some((current, indent)) = stack.pop() {
            writeln!(
                &mut string,
                "{}{}",
                "    ".repeat(indent),
                current.as_ref().statement
            )
            .unwrap();

            if let Some(next) = current.as_ref().next {
                stack.push((next, indent));
            }
            if let Some(child) = current.as_ref().child {
                stack.push((child, indent + 1));
            }
        }
        string
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::middle::TypeKey;
    use std::collections::HashSet;
    use std::fs::remove_file;
    use std::fs::OpenOptions;
    use std::io::Write;
    use std::mem::size_of;

    use std::process::Command;
    use std::ptr::NonNull;
    use uuid::Uuid;

    fn parse(text: &str) -> Option<NonNull<NewNode>> {
        let reader = std::io::BufReader::new(text.as_bytes());
        let mut iter = reader.bytes().peekable();
        get_nodes(&mut iter)
    }

    impl Variable {
        pub fn new(s: &str) -> Self {
            Self {
                addressing: Addressing::Direct,
                identifier: Vec::from(s.as_bytes()),
                index: None,
            }
        }
    }

    fn match_nodes(actual: NonNull<NewNode>, expected: &[Statement]) {
        // println!("match_nodes:");
        let mut stack = vec![(actual, 0)];

        println!("Checking nodes");
        let mut any_check = true;
        let mut expected_iter = expected.iter();

        loop {
            let node = stack.pop();
            let expected = expected_iter.next();
            match (node, expected) {
                (Some((current, s)), None) => {
                    let node = unsafe { current.as_ref() };
                    if let Some(next) = node.next {
                        stack.push((next, s));
                    }
                    if let Some(child) = node.child {
                        stack.push((child, s + 1));
                    }
                    let actual_statement = &node.statement;

                    any_check = false;
                    println!("✗   {actual_statement:?}\n    None");
                }
                (None, Some(expected_statement)) => {
                    any_check = false;
                    println!("✗   None\n    {expected_statement:?}");
                }
                (Some((current, s)), Some(expected_statement)) => {
                    let node = unsafe { current.as_ref() };
                    if let Some(next) = node.next {
                        stack.push((next, s));
                    }
                    if let Some(child) = node.child {
                        stack.push((child, s + 1));
                    }

                    let actual_statement = &node.statement;
                    let check = actual_statement == expected_statement;
                    if !check {
                        any_check = false;
                    }
                    let mark = if check { "✓" } else { "✗" };
                    println!("{mark}   {actual_statement:?}\n    {expected_statement:?}");
                }
                (None, None) => break,
            }
        }
        println!();
        assert!(any_check);
    }

    fn test_parsing(s: &str, expected: &[Statement]) -> NonNull<NewNode> {
        println!("test_parsing");
        let nodes = parse(s).unwrap();
        match_nodes(nodes, expected);
        nodes
    }

    unsafe fn check_states(actual: NonNull<NewStateNode>, expected: &[TypeValueState]) {
        println!("    Checking states");
        let mut any_check = true;
        let mut stack = vec![actual];
        let mut expected_iter = expected.iter();
        while let Some(current) = stack.pop() {
            let node = current.as_ref();
            if let Some(next_one) = node.next.0 {
                stack.push(next_one);
            }
            if let Some(next_two) = node.next.1 {
                stack.push(next_two);
            }

            let expected_item = expected_iter.next();
            let check = Some(&node.state) == expected_item;
            if !check {
                any_check = false;
            }
            let mark = if check { "✓" } else { "✗" };
            println!(
                "    {mark}   {:?}\n        {expected_item:?}",
                Some(&node.state)
            );
            println!();
        }
        println!();
        assert!(any_check);
    }

    fn test_inlining(nodes: NonNull<NewNode>, expected: &[Statement]) -> NonNull<NewNode> {
        println!("test_inlining");
        let actual = unsafe { inline_functions(nodes) };
        match_nodes(actual, expected);
        actual
    }

    fn test_exploration(
        nodes: NonNull<NewNode>,
        expected_roots: &[TypeValueState],
        expected_states_opt: Option<&[&[TypeValueState]]>,
        expected_path: &[TypeValueState],
    ) -> NonNull<NewStateNode> {
        println!("test_exploration");
        unsafe {
            let roots = roots(nodes);

            println!("Checking roots");
            let mut any_check = true;
            for (actual, expected) in roots.iter().zip(expected_roots.iter()) {
                let actual_state = &actual.as_ref().state;
                let check = actual_state == expected;
                if !check {
                    any_check = false;
                }
                let mark = if check { "✓" } else { "✗" };
                println!("{mark}   {actual_state:?}\n    {expected:?}");
            }
            println!();
            assert!(any_check);

            println!("Checking exploration");
            let mut explorer = Explorer::new(&roots);

            let finished = if let Some(expected_states) = expected_states_opt {
                let mut expected_iter = expected_states.iter();
                loop {
                    match (explorer.next(), expected_iter.next()) {
                        (Explore::Current(current), None) => {
                            println!("Missing current.");
                            check_states(current, &[]);
                        }
                        (Explore::Current(current), Some(expected)) => {
                            check_states(current, expected)
                        }
                        (Explore::Finished(_), Some(_)) => panic!("Missing expected."),
                        (Explore::Finished(finished), None) => break finished,
                    }
                }
            } else {
                loop {
                    match explorer.next() {
                        Explore::Current(current) => {}
                        Explore::Finished(finished) => break finished,
                    }
                }
            };
            println!("checking finish");
            check_states(finished, expected_path);
            finished
        }
    }

    fn test_optimization(
        nodes: NonNull<NewStateNode>,
        expected_build: &[Statement],
        expected_read: HashSet<Variable>,
        expected_finish: &[Statement],
    ) -> NonNull<NewNode> {
        println!("test_optimization");
        unsafe {
            let (new_nodes_opt, read) = build_optimized_tree(nodes);
            let new_nodes = new_nodes_opt.unwrap();
            // assert!(false);

            match_nodes(new_nodes, expected_build);
            assert_eq!(read, expected_read);

            let finish = finish_optimized_tree(new_nodes, read).unwrap();

            match_nodes(finish, expected_finish);

            finish
        }
    }

    fn test_assembling(nodes: NonNull<NewNode>, expected_assembly: &str, expected_exitcode: i32) {
        println!("test_assembling");
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

    fn ident(s: &str) -> TypeKey {
        TypeKey::Variable(Variable {
            addressing: Addressing::Direct,
            identifier: s.bytes().collect::<Vec<_>>(),
            index: None,
        })
    }

    #[test]
    fn one() {
        const SOURCE: &str = "exit 0";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(0))],
            }],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(0))],
            }],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[TypeValueState::new()],
            Some(&[&[TypeValueState::new()]]),
            &[TypeValueState::new()],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(0))],
            }],
            HashSet::from([]),
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(0))],
            }],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
        ",
            0,
        );
    }

    #[test]
    fn two() {
        const SOURCE: &str = "exit 1";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(1))],
            }],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(1))],
            }],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[TypeValueState::new()],
            Some(&[&[TypeValueState::new()]]),
            &[TypeValueState::new()],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(1))],
            }],
            HashSet::from([]),
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(1))],
            }],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
        ",
            1,
        );
    }

    #[test]
    fn three() {
        const SOURCE: &str = "exit 12";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(12))],
            }],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(12))],
            }],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[TypeValueState::new()],
            Some(&[&[TypeValueState::new()]]),
            &[TypeValueState::new()],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(12))],
            }],
            HashSet::from([]),
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(12))],
            }],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #12\n\
            svc #0\n\
        ",
            12,
        );
    }

    #[test]
    fn four() {
        const SOURCE: &str = "exit 1\nexit 2";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(2))],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(2))],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[TypeValueState::new()],
            Some(&[&[TypeValueState::new()]]),
            &[TypeValueState::new()],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(1))],
            }],
            HashSet::from([]),
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(1))],
            }],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
        ",
            1,
        );
    }

    #[test]
    fn six() {
        const SOURCE: &str = "x := 1\nexit 0";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1i64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i32))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i16))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u32))]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
            ],
            Some(&[
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u8))])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i64))])],
            ]),
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
            HashSet::from([]),
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(0))],
            }],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
        ",
            0,
        );
    }

    #[test]
    fn seven() {
        const SOURCE: &str = "x := 1\nexit x";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Variable(Variable::new("x"))],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Variable(Variable::new("a"))],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1i64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i32))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i16))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u32))]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
            ],
            Some(&[
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u8))])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i64))])],
            ]),
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
            ],
            HashSet::from([]),
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(1))],
            }],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
        ",
            1,
        );
    }

    #[test]
    fn eight() {
        const SOURCE: &str = "x := 1\nx += 1\nexit x";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AddAssign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Variable(Variable::new("x"))],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AddAssign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Variable(Variable::new("a"))],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1i64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i32))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i16))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u32))]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
            ],
            Some(&[
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u8))])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i64))])],
            ]),
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2u8))]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(2))],
                },
            ],
            HashSet::from([]),
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(2))],
            }],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #2\n\
            svc #0\n\
        ",
            2,
        );
    }

    #[test]
    fn nine() {
        const SOURCE: &str = "x := 1\nif x = 2\n    exit 1\nexit 0";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1i64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i32))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i16))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u32))]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
            ],
            Some(&[
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u8))])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1i64))])],
            ]),
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
            HashSet::from([]),
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(0))],
            }],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
        ",
            0,
        );
    }

    #[test]
    fn ten() {
        const SOURCE: &str = "x := 2\nif x = 2\n    exit 1\nexit 0";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(2i64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2i32))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2i16))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2i8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2u64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2u32))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u16))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2u8))]),
            ],
            Some(&[
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(1u16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2u64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i8))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i16))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i32))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i64))])],
                &[TypeValueState::from([(ident("a"), TypeValue::from(2i64))])],
            ]),
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(2u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2u8))]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(1))],
                },
            ],
            HashSet::from([]),
            &[Statement {
                comptime: false,
                op: Op::Syscall(Syscall::Exit),
                arg: vec![Value::Literal(Literal::Integer(1))],
            }],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #1\n\
            svc #0\n\
        ",
            1,
        );
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
        let source = format!("x := read {read}\nexit x");

        // Parsing
        let nodes = test_parsing(
            &source,
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(read as _)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Variable(Variable::new("x"))],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(read as _)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Variable(Variable::new("a"))],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I64(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I32(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I16(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I8(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U64(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U32(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )]),
            ],
            Some(&[
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U32(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U64(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I32(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I32(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I64(MyRange::any())),
                )])],
            ]),
            &[
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(read as _)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Variable(Variable::new("a"))],
                },
            ],
            HashSet::from([Variable::from("a")]),
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(read as _)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Variable(Variable::new("a"))],
                },
            ],
        );

        // Assembly
        test_assembling(
            optimized,
            &format!(
                "\
        .global _start\n\
        _start:\n\
        mov x8, #63\n\
        mov x0, #{read}\n\
        ldr x1, =a\n\
        mov x2, #1\n\
        svc #0\n\
        mov x8, #93\n\
        ldr x0, =a\n\
        ldrb w0, [x0]\n\
        svc #0\n\
        .bss\n\
        a: .skip 1\n\
    "
            ),
            data as i32,
        );

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
        let source = format!("x := read {read}\nwrite {write} x\nexit 0");

        // Parsing
        let nodes = test_parsing(
            &source,
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(read as _)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Write),
                    arg: vec![
                        Value::Literal(Literal::Integer(write as _)),
                        Value::Variable(Variable::new("x")),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(read as _)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Write),
                    arg: vec![
                        Value::Literal(Literal::Integer(write as _)),
                        Value::Variable(Variable::new("a")),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I64(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I32(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I16(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I8(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U64(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U32(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )]),
            ],
            Some(&[
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U32(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U32(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U32(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U64(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U64(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U64(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I8(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I16(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I32(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I32(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I32(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I64(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I64(MyRange::any())),
                )])],
                &[TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::I64(MyRange::any())),
                )])],
            ]),
            &[
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )]),
                TypeValueState::from([(
                    ident("a"),
                    TypeValue::Integer(TypeValueInteger::U8(MyRange::any())),
                )]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(read as _)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Write),
                    arg: vec![
                        Value::Literal(Literal::Integer(write as _)),
                        Value::Variable(Variable::new("a")),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
            HashSet::from([Variable::from("a")]),
            &[
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Read),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(read as _)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Write),
                    arg: vec![
                        Value::Literal(Literal::Integer(write as _)),
                        Value::Variable(Variable::new("a")),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Syscall(Syscall::Exit),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
            ],
        );

        // Assembly
        test_assembling(
            optimized,
            &format!(
                "\
        .global _start\n\
        _start:\n\
        mov x8, #63\n\
        mov x0, #{read}\n\
        ldr x1, =a\n\
        mov x2, #1\n\
        svc #0\n\
        mov x8, #64\n\
        mov x0, #{write}\n\
        ldr x1, =a\n\
        mov x2, #1\n\
        svc #0\n\
        mov x8, #93\n\
        mov x0, #0\n\
        svc #0\n\
        .bss\n\
        a: .skip 1\n\
        "
            ),
            0,
        );

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

    #[test]
    fn thirteen() {
        const SOURCE: &str =
            "def exit\n    mov x8 93\n    mov x0 in[0]\n    svc 0\n    unreachable\nx := 1\nx += 2\nx *= 3\nx /= 4\nx -= 5\nx &= 6\nx |= 7\nx ^= 8\nexit x";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Def),
                    arg: vec![Value::Variable(Variable::from("exit"))],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Variable(Variable {
                            addressing: Addressing::Direct,
                            identifier: Vec::from(b"in"),
                            index: Some(Box::new(Index::Offset(Offset::Integer(0)))),
                        }),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AddAssign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::MulAssign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(3)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::DivAssign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(4)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::SubAssign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(5)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AndAssign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(6)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::OrAssign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(7)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::XorAssign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Literal(Literal::Integer(8)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Call),
                    arg: vec![
                        Value::Variable(Variable::new("exit")),
                        Value::Variable(Variable::new("x")),
                    ],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AddAssign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::MulAssign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(3)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::DivAssign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(4)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::SubAssign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(5)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::AndAssign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(6)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::OrAssign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(7)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::XorAssign),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Literal(Literal::Integer(8)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Variable(Variable::from("a")),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1i64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i32))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i16))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1i8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u64))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u32))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u16))]),
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
            ],
            None,
            &[
                TypeValueState::from([(ident("a"), TypeValue::from(1u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(3u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(9u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(2u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(253u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(4u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(7u8))]),
                TypeValueState::from([(ident("a"), TypeValue::from(15u8))]),
                TypeValueState::from([
                    (ident("a"), TypeValue::from(15u8)),
                    (TypeKey::Register(Register::X8), TypeValue::from(93u64)),
                ]),
                TypeValueState::from([
                    (ident("a"), TypeValue::from(15u8)),
                    (TypeKey::Register(Register::X0), TypeValue::from(15u64)),
                    (TypeKey::Register(Register::X8), TypeValue::from(93u64)),
                ]),
                TypeValueState::from([
                    (ident("a"), TypeValue::from(15u8)),
                    (TypeKey::Register(Register::X0), TypeValue::from(15u64)),
                    (TypeKey::Register(Register::X8), TypeValue::from(93u64)),
                ]),
                TypeValueState::from([
                    (ident("a"), TypeValue::from(15u8)),
                    (TypeKey::Register(Register::X0), TypeValue::from(15u64)),
                    (TypeKey::Register(Register::X8), TypeValue::from(93u64)),
                ]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(15)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
            HashSet::from([]),
            &[
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(15)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #15\n\
            svc #0\n\
            ",
            15,
        );
    }

    #[test]
    fn fourteen() {
        const SOURCE: &str = "def exit\n    mov x8 93\n    mov x0 in[0]\n    svc 0\n    unreachable\ndef add\n    out := in[0] + in[1]\nx := add 1 2\nexit x";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Def),
                    arg: vec![Value::Variable(Variable::from("exit"))],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Variable(Variable {
                            addressing: Addressing::Direct,
                            identifier: Vec::from(b"in"),
                            index: Some(Box::new(Index::Offset(Offset::Integer(0)))),
                        }),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Def),
                    arg: vec![Value::Variable(Variable::new("add"))],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Add),
                    arg: vec![
                        Value::Variable(Variable::new("out")),
                        Value::Variable(Variable {
                            addressing: Addressing::Direct,
                            identifier: Vec::from(b"in"),
                            index: Some(Box::new(Index::Offset(Offset::Integer(0)))),
                        }),
                        Value::Variable(Variable {
                            addressing: Addressing::Direct,
                            identifier: Vec::from(b"in"),
                            index: Some(Box::new(Index::Offset(Offset::Integer(1)))),
                        }),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("x")),
                        Value::Variable(Variable::new("add")),
                        Value::Literal(Literal::Integer(1)),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Call),
                    arg: vec![
                        Value::Variable(Variable::new("exit")),
                        Value::Variable(Variable::new("x")),
                    ],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("b")),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::new("c")),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Add),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Variable(Variable::new("b")),
                        Value::Variable(Variable::new("c")),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Variable(Variable::from("a")),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(ident("b"), TypeValue::from(1i64))]),
                TypeValueState::from([(ident("b"), TypeValue::from(1i32))]),
                TypeValueState::from([(ident("b"), TypeValue::from(1i16))]),
                TypeValueState::from([(ident("b"), TypeValue::from(1i8))]),
                TypeValueState::from([(ident("b"), TypeValue::from(1u64))]),
                TypeValueState::from([(ident("b"), TypeValue::from(1u32))]),
                TypeValueState::from([(
                    ident("b"),
                    TypeValue::Integer(TypeValueInteger::U16(MyRange::from(1))),
                )]),
                TypeValueState::from([(ident("b"), TypeValue::from(1u8))]),
            ],
            None,
            &[
                TypeValueState::from([(ident("b"), TypeValue::from(1u8))]),
                TypeValueState::from([
                    (ident("b"), TypeValue::from(1u8)),
                    (ident("c"), TypeValue::from(2u8)),
                ]),
                TypeValueState::from([
                    (ident("b"), TypeValue::from(1u8)),
                    (ident("c"), TypeValue::from(2u8)),
                    (ident("a"), TypeValue::from(3u8)),
                ]),
                TypeValueState::from([
                    (ident("b"), TypeValue::from(1u8)),
                    (ident("c"), TypeValue::from(2u8)),
                    (ident("a"), TypeValue::from(3u8)),
                    (TypeKey::Register(Register::X8), TypeValue::from(93u64)),
                ]),
                TypeValueState::from([
                    (ident("b"), TypeValue::from(1u8)),
                    (ident("c"), TypeValue::from(2u8)),
                    (ident("a"), TypeValue::from(3u8)),
                    (TypeKey::Register(Register::X8), TypeValue::from(93u64)),
                    (TypeKey::Register(Register::X0), TypeValue::from(3u64)),
                ]),
                TypeValueState::from([
                    (ident("b"), TypeValue::from(1u8)),
                    (ident("c"), TypeValue::from(2u8)),
                    (ident("a"), TypeValue::from(3u8)),
                    (TypeKey::Register(Register::X8), TypeValue::from(93u64)),
                    (TypeKey::Register(Register::X0), TypeValue::from(3u64)),
                ]),
                TypeValueState::from([
                    (ident("b"), TypeValue::from(1u8)),
                    (ident("c"), TypeValue::from(2u8)),
                    (ident("a"), TypeValue::from(3u8)),
                    (TypeKey::Register(Register::X8), TypeValue::from(93u64)),
                    (TypeKey::Register(Register::X0), TypeValue::from(3u64)),
                ]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::new("b")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(1)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::new("c")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(2)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::new("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(3)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(3)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
            HashSet::from([]),
            &[
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(3)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #3\n\
            svc #0\n\
        ",
            3,
        );
    }

    #[test]
    fn fifteen() {
        const SOURCE: &str = "mov x8 93\nmov x0 0\nsvc 0\nunreachable";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[TypeValueState::from([(
                TypeKey::from(Register::X8),
                TypeValue::from(93u64),
            )])],
            Some(&[
                &[TypeValueState::from([(
                    TypeKey::from(Register::X8),
                    TypeValue::try_from(93u64).unwrap(),
                )])],
                &[TypeValueState::from([
                    (TypeKey::from(Register::X8), TypeValue::from(93u64)),
                    (TypeKey::from(Register::X0), TypeValue::from(0u64)),
                ])],
                &[TypeValueState::from([
                    (TypeKey::from(Register::X8), TypeValue::from(93u64)),
                    (TypeKey::from(Register::X0), TypeValue::from(0u64)),
                ])],
                &[TypeValueState::from([
                    (TypeKey::from(Register::X8), TypeValue::from(93u64)),
                    (TypeKey::from(Register::X0), TypeValue::from(0u64)),
                ])],
            ]),
            &[
                TypeValueState::from([(
                    TypeKey::from(Register::X8),
                    TypeValue::try_from(93u64).unwrap(),
                )]),
                TypeValueState::from([
                    (
                        TypeKey::from(Register::X8),
                        TypeValue::try_from(93u64).unwrap(),
                    ),
                    (
                        TypeKey::from(Register::X0),
                        TypeValue::try_from(0u64).unwrap(),
                    ),
                ]),
                TypeValueState::from([
                    (
                        TypeKey::from(Register::X8),
                        TypeValue::try_from(93u64).unwrap(),
                    ),
                    (
                        TypeKey::from(Register::X0),
                        TypeValue::try_from(0u64).unwrap(),
                    ),
                ]),
                TypeValueState::from([
                    (
                        TypeKey::from(Register::X8),
                        TypeValue::try_from(93u64).unwrap(),
                    ),
                    (
                        TypeKey::from(Register::X0),
                        TypeValue::try_from(0u64).unwrap(),
                    ),
                ]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
            HashSet::from([]),
            &[
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
        ",
            0,
        );
    }

    #[test]
    fn sixteen() {
        const SOURCE: &str =
            "def exit\n    mov x8 93\n    mov x0 in[0]\n    svc 0\n    unreachable\nexit 0";

        // Parsing
        let nodes = test_parsing(
            SOURCE,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Def),
                    arg: vec![Value::Variable(Variable::from("exit"))],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Variable(Variable {
                            addressing: Addressing::Direct,
                            identifier: Vec::from(b"in"),
                            index: Some(Box::new(Index::Offset(Offset::Integer(0)))),
                        }),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Call),
                    arg: vec![
                        Value::Variable(Variable::from("exit")),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
            ],
        );

        // Function inlining
        let inlined = test_inlining(
            nodes,
            &[
                Statement {
                    comptime: false,
                    op: Op::Intrinsic(Intrinsic::Assign),
                    arg: vec![
                        Value::Variable(Variable::from("a")),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Variable(Variable::from("a")),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
        );

        // Exploration
        let path = test_exploration(
            inlined,
            &[
                TypeValueState::from([(TypeKey::from(Variable::from("a")), TypeValue::from(0i64))]),
                TypeValueState::from([(TypeKey::from(Variable::from("a")), TypeValue::from(0i32))]),
                TypeValueState::from([(TypeKey::from(Variable::from("a")), TypeValue::from(0i16))]),
                TypeValueState::from([(TypeKey::from(Variable::from("a")), TypeValue::from(0i8))]),
                TypeValueState::from([(TypeKey::from(Variable::from("a")), TypeValue::from(0u64))]),
                TypeValueState::from([(TypeKey::from(Variable::from("a")), TypeValue::from(0u32))]),
                TypeValueState::from([(TypeKey::from(Variable::from("a")), TypeValue::from(0u16))]),
                TypeValueState::from([(TypeKey::from(Variable::from("a")), TypeValue::from(0u8))]),
            ],
            None,
            &[
                TypeValueState::from([(TypeKey::from(Variable::from("a")), TypeValue::from(0u8))]),
                TypeValueState::from([
                    (TypeKey::from(Variable::from("a")), TypeValue::from(0u8)),
                    (
                        TypeKey::from(Register::X8),
                        TypeValue::try_from(93u64).unwrap(),
                    ),
                ]),
                TypeValueState::from([
                    (TypeKey::from(Variable::from("a")), TypeValue::from(0u8)),
                    (
                        TypeKey::from(Register::X8),
                        TypeValue::try_from(93u64).unwrap(),
                    ),
                    (
                        TypeKey::from(Register::X0),
                        TypeValue::try_from(0u64).unwrap(),
                    ),
                ]),
                TypeValueState::from([
                    (TypeKey::from(Variable::from("a")), TypeValue::from(0u8)),
                    (
                        TypeKey::from(Register::X8),
                        TypeValue::try_from(93u64).unwrap(),
                    ),
                    (
                        TypeKey::from(Register::X0),
                        TypeValue::try_from(0u64).unwrap(),
                    ),
                ]),
                TypeValueState::from([
                    (TypeKey::from(Variable::from("a")), TypeValue::from(0u8)),
                    (
                        TypeKey::from(Register::X8),
                        TypeValue::try_from(93u64).unwrap(),
                    ),
                    (
                        TypeKey::from(Register::X0),
                        TypeValue::try_from(0u64).unwrap(),
                    ),
                ]),
            ],
        );

        // Optimization
        let optimized = test_optimization(
            path,
            &[
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Type),
                    arg: vec![
                        Value::Variable(Variable::from("a")),
                        Value::Type(Type::U8),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
            HashSet::from([]),
            &[
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X8),
                        Value::Literal(Literal::Integer(93)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Mov),
                    arg: vec![
                        Value::Register(Register::X0),
                        Value::Literal(Literal::Integer(0)),
                    ],
                },
                Statement {
                    comptime: false,
                    op: Op::Assembly(Assembly::Svc),
                    arg: vec![Value::Literal(Literal::Integer(0))],
                },
                Statement {
                    comptime: false,
                    op: Op::Special(Special::Unreachable),
                    arg: vec![],
                },
            ],
        );

        // Assembly
        test_assembling(
            optimized,
            "\
            .global _start\n\
            _start:\n\
            mov x8, #93\n\
            mov x0, #0\n\
            svc #0\n\
        ",
            0,
        );
    }

    #[cfg(feature = "false")]
    fn thirteen() {
        const THIRTEEN: &str = "x := memfd_create\nexit 0";

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

    #[cfg(feature = "false")]
    fn fourteen() {
        const FOURTEEN: &str = "\
            x := 0\n\
            x -= 1\n\
            if x < 0\n    require x >= -128\n    exit 1\n\
            require x <= 255\n\
            exit 0\n\
        ";

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

    // // If condition then break
    // const LOOP_ONE: &str = "\
    //     x := 0\n\
    //     loop\n\
    //     \x20   x += 1\n\
    //     \x20   if x = 2\n\
    //     \x20       break\n\
    //     exit 0\n\
    // ";

    // // Break if condition
    // const LOOP_TWO: &str = "\
    //     x := 0\n\
    //     loop\n\
    //     \x20   x += 1\n\
    //     \x20   break x = 2\n\
    //     exit 0\n\
    // ";

    // #[cfg(feature="false")]
    // fn loop_one() {
    //     let nodes = parse(LOOP_ONE);
    //
    //     assert_eq!(
    //         nodes,
    //         [
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Intrinsic(Intrinsic::Assign),
    //                     arg: vec![
    //                         Value::Variable(Variable::new("x")),
    //                         Value::Literal(Literal::Integer(0)),
    //                     ],
    //                 },
    //                 child: None,
    //                 next: Some(1),
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Intrinsic(Intrinsic::Loop),
    //                     arg: vec![],
    //                 },
    //                 child: Some(2),
    //                 next: Some(5),
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Intrinsic(Intrinsic::AddAssign),
    //                     arg: vec![
    //                         Value::Variable(Variable::new("x")),
    //                         Value::Literal(Literal::Integer(1)),
    //                     ],
    //                 },
    //                 child: None,
    //                 next: Some(3),
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
    //                     arg: vec![
    //                         Value::Variable(Variable::new("x")),
    //                         Value::Literal(Literal::Integer(2)),
    //                     ],
    //                 },
    //                 child: Some(4),
    //                 next: None,
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Intrinsic(Intrinsic::Break),
    //                     arg: vec![],
    //                 },
    //                 child: None,
    //                 next: None,
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Syscall(Syscall::Exit),
    //                     arg: vec![Value::Literal(Literal::Integer(0)),],
    //                 },
    //                 child: None,
    //                 next: None,
    //             },
    //         ]
    //     );
    //     let optimized_nodes = optimize(&nodes);
    //     assert_eq!(
    //         optimized_nodes,
    //         [
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Special(Special::Type),
    //                     arg: vec![
    //                         Value::Variable(Variable::new("x")),
    //                         Value::Type(Type::U8),
    //                         Value::Literal(Literal::Integer(0)),
    //                     ],
    //                 },
    //                 child: None,
    //                 next: Some(1),
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Intrinsic(Intrinsic::Loop),
    //                     arg: vec![],
    //                 },
    //                 child: Some(2),
    //                 next: Some(4),
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Intrinsic(Intrinsic::AddAssign),
    //                     arg: vec![
    //                         Value::Variable(Variable::new("x")),
    //                         Value::Literal(Literal::Integer(1)),
    //                     ],
    //                 },
    //                 child: None,
    //                 next: Some(3),
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Intrinsic(Intrinsic::If(Cmp::Eq)),
    //                     arg: vec![
    //                         Value::Variable(Variable::new("x")),
    //                         Value::Literal(Literal::Integer(3)),
    //                     ],
    //                 },
    //                 child: Some(5),
    //                 next: None,
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Intrinsic(Intrinsic::Break),
    //                     arg: vec![],
    //                 },
    //                 child: None,
    //                 next: None,
    //             },
    //             Node {
    //                 statement: Statement {
    //                     comptime: false,
    //                     op: Op::Syscall(Syscall::Exit),
    //                     arg: vec![Value::Literal(Literal::Integer(0)),],
    //                 },
    //                 child: None,
    //                 next: None,
    //             },
    //         ]
    //     );
    //     let expected_assembly = "\
    //         .global _start\n\
    //         _start:\n\
    //         ldr x0, =x\n\
    //         ldr w1, [x0]\n\
    //         sub w1, w1, #1\n\
    //         strb w1, [x0]\n\
    //         mov x8, #93\n\
    //         mov x0, #0\n\
    //         svc #0\n\
    //         .data\n\
    //         x: .byte 0\n\
    //     ";
    //     assemble(&optimized_nodes, expected_assembly, 0);
    // }

    #[cfg(feature = "false")]
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

    #[cfg(feature = "false")]
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
    #[cfg(feature = "false")]
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
