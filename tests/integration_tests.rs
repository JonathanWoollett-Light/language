use std::fs::{remove_dir_all, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use std::str::from_utf8;
use uuid::Uuid;

const BINARY: &str = env!("CARGO_BIN_EXE_language");

const SOURCE_FILE: &str = "source.abc";

const SYSCALLS: &str = "./syscalls.abc";
// const SYSCALLS: &str = "https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.abc";

macro_rules! source {
    ($source: expr) => {
        format!("include {SYSCALLS}\n{}", $source).as_bytes()
    };
}

#[test]
fn exit_zero() {
    build_and_run(source!("exit 0"), b"", 0);
}

#[test]
fn exit_one() {
    build_and_run(source!("exit 1"), b"", 1);
}

#[test]
fn exit_twelve() {
    build_and_run(source!("exit 12"), b"", 12);
}

#[test]
fn exit_one_two() {
    build_and_run(
        source!(
            "\
            exit 1\n\
            exit 2"
        ),
        b"",
        1,
    );
}

#[test]
fn zero_variable() {
    build_and_run(
        source!(
            "\
            x := 1\n\
            exit 0"
        ),
        b"",
        0,
    );
}

#[test]
fn exit_variable() {
    build_and_run(
        source!(
            "\
            x := 1\n\
            exit x"
        ),
        b"",
        1,
    );
}

#[test]
fn variable_addition() {
    build_and_run(
        source!(
            "\
            x := 1\n\
            x += 1\n\
            exit x"
        ),
        b"",
        2,
    );
}

#[test]
fn variable_if_false() {
    build_and_run(
        source!(
            "\
            x := 1\n\
            if x = 2\n\
            \x20   exit 1\n\
            exit 0"
        ),
        b"",
        0,
    );
}

#[test]
fn variable_if_true() {
    build_and_run(
        source!(
            "\
            x := 2\n\
            if x = 2\n\
            \x20   exit 1\n\
            exit 0"
        ),
        b"",
        1,
    );
}

// #[test]
// fn read() {
//     // Create pipe
//     let mut pipe_out = [0, 0];
//     let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
//     assert_eq!(res, 0);
//     let [read, write] = pipe_out;
//     // Write u8 to pipe.
//     let data = 27u8;
//     let bytes = data.to_ne_bytes();
//     let res = unsafe { libc::write(write, bytes.as_ptr().cast(), std::mem::size_of::<u8>() as _) };
//     assert_eq!(res, std::mem::size_of::<u8>() as _);

//     let source_string = format!(
//         "\
//         x := read {read}\n\
//         exit x"
//     );
//     build_and_run(source!(source_string), b"", 27);

//     // Close pipe.
//     unsafe {
//         libc::close(read);
//         libc::close(write);
//     }
// }

// #[test]
// fn read_write() {
//     // Create pipe
//     let mut pipe_out = [0, 0];
//     let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
//     assert_eq!(res, 0);
//     let [read, write] = pipe_out;
//     // Write u8 to pipe.
//     let data = 27u8;
//     let bytes = data.to_ne_bytes();
//     let res = unsafe { libc::write(write, bytes.as_ptr().cast(), std::mem::size_of::<u8>() as _) };
//     assert_eq!(res, std::mem::size_of::<u8>() as _);

//     let source_string = format!(
//         "\
//         x := read {read}\n\
//         write {write} &x\n\
//         exit 0"
//     );
//     build_and_run(source!(source_string), b"", 0);

//     // Read u8 from pipe.
//     let mut buffer = [0u8; std::mem::size_of::<u8>()];
//     let res = unsafe { libc::read(read, buffer.as_mut_ptr().cast(), std::mem::size_of::<u8>() as _) };
//     assert_eq!(res, std::mem::size_of::<u8>() as _);
//     assert_eq!(buffer, bytes);
//     // Close pipe.
//     unsafe {
//         libc::close(read);
//         libc::close(write);
//     }
// }

// #[test]
// fn arithmetic() {
//     build_and_run(
//         source!(
//             "\
//             x := 1\n\
//             x += 2\n\
//             x *= 3\n\
//             x /= 4\n\
//             x -= 5\n\
//             x &= 6\n\
//             x |= 7\n\
//             x ^= 8\n\
//             exit x"
//         ),
//         b"",
//         15,
//     );
// }

// #[test]
// fn helloworld_new() {
//     build_and_run(
//         source!(
//             "\
//             x := \"Hello, World!\\n\"\n\
//             write 1 &x\n\
//             exit 0"
//         ),
//         b"Hello, World!\n",
//         0,
//     );
// }

fn build_and_run(source: &[u8], expected_stdout: &[u8], expected_code: i32) {
    let directory = PathBuf::from(format!("/tmp/a{}", Uuid::new_v4()));
    println!("directory: {}", directory.display());
    let output = Command::new(BINARY)
        .args(["new", &directory.display().to_string()])
        .output()
        .unwrap();
    assert_eq!(output.stderr, &[], "{}", from_utf8(&output.stderr).unwrap());
    assert_eq!(output.stdout, &[], "{}", from_utf8(&output.stdout).unwrap());
    assert_eq!(output.status.code(), Some(0));

    let mut source_file = OpenOptions::new()
        .truncate(true)
        .write(true)
        .open(directory.join(SOURCE_FILE))
        .unwrap();
    source_file.write_all(source).unwrap();

    let output = Command::new(BINARY)
        .args(["run", &directory.display().to_string()])
        .output()
        .unwrap();

    use std::fs::read_to_string;
    if let Ok(included) = read_to_string(directory.join("build").join("included.abc")) {
        println!("--- included ---\n{included}\n-----------",);
    }
    if let Ok(inlined) = read_to_string(directory.join("build").join("inlined.abc")) {
        println!("--- inlined ---\n{inlined}\n-----------",);
    }
    if let Ok(optimized) = read_to_string(directory.join("build").join("optimized.abc")) {
        println!("--- optimized ---\n{optimized}\n-----------",);
    }
    if let Ok(explored) = read_to_string(directory.join("build").join("explored.abc")) {
        println!("--- explored ---\n{explored}\n-----------",);
    }
    if let Ok(assembly) = read_to_string(directory.join("build").join("assembly.s")) {
        println!("--- assembly ---\n{assembly}\n-----------",);
    }

    assert_eq!(output.stderr, &[], "{}", from_utf8(&output.stderr).unwrap());
    assert_eq!(output.stdout, expected_stdout, "{}", from_utf8(&output.stdout).unwrap());
    assert_eq!(output.status.code(), Some(expected_code));

    remove_dir_all(directory).unwrap();
}
