use std::fs::{remove_dir_all, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use uuid::Uuid;

const BINARY: &str = env!("CARGO_BIN_EXE_language");

const SOURCE_FILE: &str = "source.abc";
use std::str::from_utf8;

#[test]
fn zero() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nexit 0",
        b"",
        0
    );
}

#[test]
fn one() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nexit 1",
        b"",
        1
    );
}

#[test]
fn twelve() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nexit 12",
        b"",
        12
    );
}

#[test]
fn one_two() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nexit 1\nexit 2",
        b"",
        1
    );
}

#[test]
fn zero_variable() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := 1\nexit 0",
        b"",
        0
    );
}

#[test]
fn variable() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := 1\nexit x",
        b"",
        1
    );
}

#[test]
fn variable_addition() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := 1\nx += 1\nexit x",
        b"",
        2
    );
}

#[test]
fn variable_if_false() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := 1\nif x = 2\n    exit 1\nexit 0",
        b"",
        0
    );
}

#[test]
fn variable_if_true() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := 2\nif x = 2\n    exit 1\nexit 0",
        b"",
        1
    );
}

#[test]
fn read() {
    // Create pipe
    let mut pipe_out = [0, 0];
    let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
    assert_eq!(res, 0);
    let [read, write] = pipe_out;
    // Write u8 to pipe.
    let data = 27u8;
    let bytes = data.to_ne_bytes();
    let res = unsafe { libc::write(write, bytes.as_ptr().cast(), std::mem::size_of::<u8>() as _) };
    assert_eq!(res, std::mem::size_of::<u8>() as _);

    build_and_run(
        format!("include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := read {read}\nexit x").as_bytes(),
        b"",
        27
    );

    // Close pipe.
    unsafe {
        libc::close(read);
        libc::close(write);
    }
}

#[test]
fn read_write() {
    // Create pipe
    let mut pipe_out = [0, 0];
    let res = unsafe { libc::pipe(pipe_out.as_mut_ptr()) };
    assert_eq!(res, 0);
    let [read, write] = pipe_out;
    // Write u8 to pipe.
    let data = 27u8;
    let bytes = data.to_ne_bytes();
    let res = unsafe { libc::write(write, bytes.as_ptr().cast(), std::mem::size_of::<u8>() as _) };
    assert_eq!(res, std::mem::size_of::<u8>() as _);

    build_and_run(
        format!("include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := read {read}\nwrite x\nexit 0").as_bytes(),
        b"",
        0
    );

    // Read u8 from pipe.
    let mut buffer = [0u8; std::mem::size_of::<u8>()];
    let res = unsafe {
        libc::read(
            read,
            buffer.as_mut_ptr().cast(),
            std::mem::size_of::<u8>() as _,
        )
    };
    assert_eq!(res, std::mem::size_of::<u8>() as _);
    assert_eq!(buffer, bytes);
    // Close pipe.
    unsafe {
        libc::close(read);
        libc::close(write);
    }
}

#[test]
fn arithmetic() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := 1\nx += 2\nx *= 3\nx /= 4\nx -= 5\nx &= 6\nx |= 7\nx ^= 8\nexit x",
        b"",
        15
    );
}

#[test]
fn helloworld_new() {
    build_and_run(
        b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := \"Hello, World!\\n\"\nwrite 1 &x\nexit 0",
        b"Hello, World!\n",
        0
    );
}

fn build_and_run(source: &[u8], expected_stdout: &[u8], expected_code: i32) {
    let directory = PathBuf::from(format!("/tmp/a{}", Uuid::new_v4()));
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
    println!(
        "--- included ---\n{}\n-----------",
        read_to_string(directory.join("build").join("included.abc")).unwrap()
    );
    // println!("--- inlined ---\n{}\n-----------",read_to_string(directory.join("build").join("inlined.abc")).unwrap());
    // println!("--- optimized ---\n{}\n-----------",read_to_string(directory.join("build").join("optimized.abc")).unwrap());
    // println!("--- assembly ---\n{}\n-----------",read_to_string(directory.join("build").join("assembly.s")).unwrap());

    assert_eq!(output.stderr, &[], "{}", from_utf8(&output.stderr).unwrap());
    assert_eq!(
        output.stdout,
        expected_stdout,
        "{}",
        from_utf8(&output.stdout).unwrap()
    );
    assert_eq!(output.status.code(), Some(expected_code));

    remove_dir_all(directory).unwrap();
}
