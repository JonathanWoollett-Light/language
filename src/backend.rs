use crate::ast::*;
use crate::LOOP_LIMIT;
use std::fmt::Write;

pub fn assembly_from_node(nodes: &[Node]) -> String {
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

pub fn instruction_from_node(
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
                Some(Value::Literal(Literal::Integer(x))) => write!(
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
                Some([Value::Variable(Variable(x)), Value::Literal(Literal::Integer(y))]) => {
                    write!(
                        data,
                        "\
                    {}:\n\
                    .word {y}\n\
                ",
                        std::str::from_utf8(x).unwrap()
                    )
                    .unwrap()
                }
                Some([Value::Variable(Variable(x)), rest @ ..]) => write!(
                    data,
                    "\
                    {}:\n\
                    .byte {}\n\
                ",
                    std::str::from_utf8(x).unwrap(),
                    rest.iter()
                        .flat_map(|c| match c {
                            Value::Literal(Literal::Integer(c)) => vec![c.to_string()],
                            Value::Literal(Literal::String(s)) =>
                                s.bytes().map(|b| b.to_string()).collect::<Vec<_>>(),
                            _ => todo!(),
                        })
                        .intersperse(String::from(","))
                        .collect::<String>()
                )
                .unwrap(),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Add) => match current.statement.arg.get(..) {
                Some([Value::Variable(Variable(x)), Value::Literal(Literal::Integer(y))]) => {
                    write!(
                        &mut assembly,
                        "\
                    ldr x0, ={}\n\
                    ldr x1, [x0]\n\
                    add x1, x1, #{y}\n\
                    str x1, [x0]\n\
                ",
                        std::str::from_utf8(x).unwrap()
                    )
                    .unwrap()
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::IfEq) => match current.statement.arg.get(..) {
                Some([Value::Variable(Variable(x)), Value::Literal(Literal::Integer(y))]) => {
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
                Some(
                    [Value::Variable(Variable(x)), Value::Literal(Literal::Integer(fd)), Value::Literal(Literal::Integer(n))],
                ) => {
                    write!(
                        &mut assembly,
                        "\
                        mov x8, #{}\n\
                        mov x0, #{fd}\n\
                        ldr x1, ={}\n\
                        mov x2, #{n}\n\
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
                        .skip {n}\n\
                    ",
                        std::str::from_utf8(x).unwrap()
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Write) => match current.statement.arg.get(..) {
                Some(
                    [Value::Variable(Variable(v)), Value::Literal(Literal::Integer(fd)), Value::Variable(Variable(x)), Value::Literal(Literal::Integer(n))],
                ) if v == b"_" => {
                    write!(
                        &mut assembly,
                        "\
                        mov x8, #{}\n\
                        mov x0, #{fd}\n\
                        ldr x1, ={}\n\
                        mov x2, #{n}\n\
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
