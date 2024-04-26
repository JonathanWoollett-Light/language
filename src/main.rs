#![feature(test)]
#![feature(let_chains)]
#![feature(int_roundings)]
#![feature(if_let_guard)]
#![feature(iter_intersperse)]
#![feature(box_patterns)]
#![feature(exclusive_range_pattern)]
#![feature(array_chunks)]

extern crate test;

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
mod draw_dag;

mod new_middle;

#[cfg(debug_assertions)]
const LOOP_LIMIT: usize = 1000;

enum Args {
    Build(Option<PathBuf>),
    New(Option<PathBuf>),
    Run(Option<PathBuf>),
}
impl From<Vec<String>> for Args {
    fn from(args: Vec<String>) -> Self {
        match args.as_slice() {
            [_, arg] if arg == "build" => Self::Build(None),
            [_, arg] if arg == "new" => Self::New(None),
            [_, arg] if arg == "run" => Self::Run(None),
            [_, arg, path] if arg == "build" => Self::Build(Some(PathBuf::from(path))),
            [_, arg, path] if arg == "new" => Self::New(Some(PathBuf::from(path))),
            [_, arg, path] if arg == "run" => Self::Run(Some(PathBuf::from(path))),
            _ => todo!(),
        }
    }
}

fn sha256_digest<R: Read>(mut reader: R) -> std::io::Result<Digest> {
    let mut context = Context::new(&SHA256);
    let mut buffer = [0; 1024];

    #[cfg(debug_assertions)]
    let mut i = 0;
    loop {
        #[cfg(debug_assertions)]
        {
            assert!(i < LOOP_LIMIT);
            i += 1;
        }

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

fn build(path: Option<PathBuf>) {
    let project_path = path.unwrap_or(PathBuf::from("./"));
    let source_path = project_path.join("source").with_extension(LANGUAGE_EXTENSION);
    let source = std::fs::read_to_string(source_path).unwrap();

    // Create build directory
    let build_dir = project_path.join(BUILD_DIR);
    if !build_dir.exists() {
        std::fs::create_dir(build_dir).unwrap();
    }

    // Create lock file
    let mut lock_file = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(project_path.join("lock.csv"))
        .unwrap();
    writeln!(&mut lock_file, "file,hash").unwrap();

    // Includes dependencies
    let mut bytes = source.into_bytes();
    get_includes(&mut bytes);
    write_file(
        &project_path,
        PathBuf::from("included").with_extension(LANGUAGE_EXTENSION),
        &bytes,
        &mut lock_file,
    );

    // Parses AST
    let reader = std::io::BufReader::new(bytes.as_slice());
    let mut iter = reader.bytes().peekable();
    let nodes = get_nodes(&mut iter).unwrap();

    // Inlines functions
    let inlined = unsafe { inline_functions(nodes) };
    let inlined_string = display_ast(inlined);
    write_file(
        &project_path,
        PathBuf::from("inlined").with_extension(LANGUAGE_EXTENSION),
        inlined_string.as_bytes(),
        &mut lock_file,
    );

    // Exploration record
    let exploration_dir = project_path.join(BUILD_DIR).join("exploration");
    if !exploration_dir.exists() {
        std::fs::create_dir(&exploration_dir).unwrap();
    }

    let mut explorer = unsafe { new_middle::Explorer::new(inlined) };
    let mut i = 0;
    let explored = loop {
        explorer = match unsafe { explorer.next() } {
            new_middle::ExplorationResult::Continue(e) => e,
            new_middle::ExplorationResult::Done(e) => break e,
        };

        i += 1;
        assert!(i < 50);
    };
    let explored_string = display_ast(explored);
    write_file(
        &project_path,
        PathBuf::from("explored").with_extension(LANGUAGE_EXTENSION),
        explored_string.as_bytes(),
        &mut lock_file,
    );

    // Optimize source
    // let optimized = unsafe { optimize(explored) };
    // let optimized_string = display_ast(optimized);
    // write_file(
    //     &project_path,
    //     PathBuf::from("optimized").with_extension(LANGUAGE_EXTENSION),
    //     optimized_string.as_bytes(),
    //     &mut lock_file,
    // );

    // Construct assembly
    let assembly = assembly_from_node(explored);
    let assembly_path = PathBuf::from("assembly").with_extension("s");
    write_file(
        &project_path,
        assembly_path.clone(),
        assembly.as_bytes(),
        &mut lock_file,
    );

    // Make object file
    let object_path = project_path.join("build").join("object").with_extension("o");
    let object_output = std::process::Command::new("as")
        .args([
            "-o",
            &object_path.display().to_string(),
            &project_path.join("build").join(assembly_path).display().to_string(),
        ])
        .output()
        .unwrap();
    assert_eq!(
        object_output.stdout,
        [],
        "{}",
        std::str::from_utf8(&object_output.stdout).unwrap()
    );
    assert_eq!(
        object_output.stderr,
        [],
        "{}",
        std::str::from_utf8(&object_output.stderr).unwrap()
    );
    assert_eq!(object_output.status.code(), Some(0));
    // Write object hash
    let mut object_buffer = Vec::new();
    let mut object_file = OpenOptions::new().read(true).open(&object_path).unwrap();
    object_file.read_to_end(&mut object_buffer).unwrap();
    let object_hash = HEXUPPER.encode(sha256_digest(object_buffer.as_slice()).unwrap().as_ref());
    let object_file_name = object_path.file_stem().unwrap();
    writeln!(&mut lock_file, "{},{object_hash}", object_file_name.to_str().unwrap()).unwrap();

    // Make binary file
    let binary_path = project_path.join("build").join("binary");
    let binary_output = std::process::Command::new("ld")
        .args([
            "-s",
            "-o",
            &binary_path.display().to_string(),
            &object_path.display().to_string(),
        ])
        .output()
        .unwrap();
    assert_eq!(
        binary_output.stdout,
        [],
        "{}",
        std::str::from_utf8(&binary_output.stdout).unwrap()
    );
    assert_eq!(
        binary_output.stderr,
        [],
        "{}",
        std::str::from_utf8(&binary_output.stderr).unwrap()
    );
    assert_eq!(binary_output.status.code(), Some(0));
    // Write binary hash
    let mut binary_buffer = Vec::new();
    let mut binary_file = OpenOptions::new().read(true).open(&binary_path).unwrap();
    binary_file.read_to_end(&mut binary_buffer).unwrap();
    let binary_hash = HEXUPPER.encode(sha256_digest(binary_buffer.as_slice()).unwrap().as_ref());
    let binary_file_name = binary_path.file_stem().unwrap();
    writeln!(&mut lock_file, "{},{binary_hash}", binary_file_name.to_str().unwrap()).unwrap();
}

fn run(path: Option<PathBuf>) {
    build(path.clone());
    // let project_path = path.unwrap_or(PathBuf::from("./"));
    // let binary_path = project_path.join("build").join("binary");
    // let binary_path_cstring = std::ffi::CString::new(binary_path.display().to_string()).unwrap();
    // unsafe {
    //     libc::execv(binary_path_cstring.into_raw(), std::ptr::null());
    // }
}

#[allow(unreachable_code)]
fn main() {
    let args = Args::from(std::env::args().collect::<Vec<_>>());

    match args {
        Args::New(Some(path)) => {
            if !path.exists() {
                std::fs::create_dir(&path).unwrap();
            }
            let mut source = OpenOptions::new()
                .create(true)
                .write(true)
                .open(path.join("source").with_extension(LANGUAGE_EXTENSION))
                .unwrap();
            source
                .write_all(
                    b"\
                include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\n\
                x := \"Hello, World!\\n\"\n\
                write 1 x\n\
                exit 0\n\
            ",
                )
                .unwrap();
        }
        Args::Build(path) => build(path),
        Args::Run(path) => run(path),
        _ => todo!(),
    }
}

fn display_ast(node: std::ptr::NonNull<crate::ast::NewNode>) -> String {
    unsafe {
        use std::fmt::Write;
        let mut stack = vec![(node, 0)];
        let mut string = String::new();
        while let Some((current, indent)) = stack.pop() {
            writeln!(&mut string, "{}{}", "    ".repeat(indent), current.as_ref().statement).unwrap();

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

#[allow(dead_code)] // This is used for debugging
fn display_ast_addresses(node: std::ptr::NonNull<crate::ast::NewNode>) -> String {
    unsafe {
        use std::fmt::Write;
        let mut stack = vec![(node, 0)];
        let mut string = String::new();
        while let Some((current, indent)) = stack.pop() {
            writeln!(
                &mut string,
                "{current:?}    {}{}",
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
                        arg: vec![Value::Variable(Variable::new("x")), Value::Literal(Literal::Integer(0)),],
                    },
                    child: None,
                    next: Some(1),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::SubAssign),
                        arg: vec![Value::Variable(Variable::new("x")), Value::Literal(Literal::Integer(1)),],
                    },
                    child: None,
                    next: Some(2),
                },
                Node {
                    statement: Statement {
                        comptime: false,
                        op: Op::Intrinsic(Intrinsic::If(Cmp::Lt)),
                        arg: vec![Value::Variable(Variable::new("x")), Value::Literal(Literal::Integer(0)),],
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
                        arg: vec![Value::Variable(Variable::new("x")), Value::Literal(Literal::Integer(1))]
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("pos"))))),
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("diff_offset"))))),
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("diff_offset"))))),
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("pos"))))),
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("buff_offset"))))),
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("pos"))))),
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("diff_offset"))))),
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("diff_offset"))))),
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("pos"))))),
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
                            index: Some(Box::new(Index::Offset(Offset::Variable(Variable::new("buff_offset"))))),
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
