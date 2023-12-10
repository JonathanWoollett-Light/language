use std::fs::{remove_file, OpenOptions};
use std::io::Write;
use std::process::Command;
use uuid::Uuid;

const BINARY: &str = env!("CARGO_BIN_EXE_language");

#[test]
fn helloworld() {
    const SOURCE: &str = "x := \"Hello, World!\n\"\nwrite 1 x\nexit 0";
    const EXPECTED_ASSEMBLY: &[u8] = b".global _start\n_start:\nmov x8, #64\nmov x0, #1\nldr x1, =a\nmov x2, #14\nsvc #0\nmov x8, #93\nmov x0, #0\nsvc #0\n.data\na: .byte 72,101,108,108,111,44,32,87,111,114,108,100,33,10\n";
    const EXPECTED_EXIT_CODE: i32 = 0;
    const EXPECTED_STDOUT: &[u8] = b"Hello, World!\n";

    let output = Command::new(BINARY)
        .args(["--source", SOURCE])
        .output()
        .unwrap();
    assert_eq!(output.stderr, &[]);
    assert_eq!(
        output.stdout,
        EXPECTED_ASSEMBLY,
        "\n{:?} !=\n{:?}",
        std::str::from_utf8(&output.stdout).unwrap(),
        std::str::from_utf8(EXPECTED_ASSEMBLY).unwrap()
    );
    assert_eq!(output.status.code(), Some(0));

    let path = format!("/tmp/{}", Uuid::new_v4());
    let assembly_path = format!("{path}.s");
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .open(&assembly_path)
        .unwrap();
    file.write_all(&output.stdout).unwrap();
    let object_path = format!("{path}.o");
    let output = Command::new("as")
        .args(["-o", &object_path, &assembly_path])
        .output()
        .unwrap();
    assert_eq!(
        output.stderr,
        [],
        "{}",
        std::str::from_utf8(&output.stderr).unwrap()
    );
    assert_eq!(
        output.stdout,
        [],
        "{}",
        std::str::from_utf8(&output.stdout).unwrap()
    );
    assert_eq!(output.status.code(), Some(0));
    remove_file(assembly_path).unwrap();

    let output = Command::new("ld")
        .args(["-s", "-o", &path, &object_path])
        .output()
        .unwrap();
    assert_eq!(
        output.stderr,
        [],
        "{}",
        std::str::from_utf8(&output.stderr).unwrap()
    );
    assert_eq!(
        output.stdout,
        [],
        "{}",
        std::str::from_utf8(&output.stdout).unwrap()
    );
    assert_eq!(output.status.code(), Some(0));
    remove_file(object_path).unwrap();

    let output = Command::new(&path).output().unwrap();
    assert_eq!(
        output.stderr,
        [],
        "{}",
        std::str::from_utf8(&output.stderr).unwrap()
    );
    assert_eq!(
        output.stdout,
        EXPECTED_STDOUT,
        "{}",
        std::str::from_utf8(&output.stdout).unwrap()
    );
    assert_eq!(output.status.code(), Some(EXPECTED_EXIT_CODE));
    remove_file(path).unwrap();
}
