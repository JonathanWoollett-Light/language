use std::fs::{remove_dir_all, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use uuid::Uuid;

const BINARY: &str = env!("CARGO_BIN_EXE_language");

const SOURCE_FILE: &str = "source.abc";
use std::str::from_utf8;

#[test]
fn helloworld_new() {
    const SOURCE: &[u8] = b"include https://raw.githubusercontent.com/JonathanWoollett-Light/language/master/syscalls.lang\nx := \"Hello, World!\\n\"\nwrite 1 &x\nexit 0";

    let directory = PathBuf::from(format!("/tmp/a{}", Uuid::new_v4()));
    let output = Command::new(BINARY)
        .args(["--new", &directory.display().to_string()])
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
    source_file.write_all(SOURCE).unwrap();

    let output = Command::new(BINARY)
        .args(["--run", &directory.display().to_string()])
        .output()
        .unwrap();
    assert_eq!(output.stderr, &[], "{}", from_utf8(&output.stderr).unwrap());
    assert_eq!(
        output.stdout,
        b"Hello, World!\n",
        "{}",
        from_utf8(&output.stdout).unwrap()
    );
    assert_eq!(output.status.code(), Some(0));

    remove_dir_all(directory).unwrap();
}
