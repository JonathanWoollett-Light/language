use crate::ast::*;
use crate::LOOP_LIMIT;
use std::collections::HashMap;
use std::fmt::Write;

pub fn assembly_from_node(nodes: &[Node]) -> String {
    let mut data = String::new();
    let mut bss = String::new();
    let mut block_counter = 0;
    // Have we defined an empty string in the data section to use for anonymous `memfd_create`.
    let mut empty = false;
    let mut type_data = HashMap::new();
    let assembly = instruction_from_node(
        nodes,
        0,
        &mut data,
        &mut bss,
        &mut block_counter,
        &mut empty,
        &mut type_data,
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
    type_data: &mut HashMap<Identifier, Type>,
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
            Op::Special(Special::Type) => match current.statement.arg.as_slice() {
                [Value::Variable(Variable { identifier, .. }), Value::Type(value_type)] => {
                    type_data.insert(identifier.clone(), value_type.clone());
                    write!(
                        bss,
                        "\
                        {}:\n\
                        .skip {}\n\
                    ",
                        std::str::from_utf8(identifier).unwrap(),
                        value_type.bytes()
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Exit) => match current.statement.arg.as_slice() {
                [Value::Literal(Literal::Integer(x))] => write!(
                    &mut assembly,
                    "\
                    mov x8, #{}\n\
                    mov x0, #{x}\n\
                    svc #0\n\
                ",
                    libc::SYS_exit
                )
                .unwrap(),
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                })] => write!(
                    &mut assembly,
                    "\
                    mov x8, #{}\n\
                    ldr x0, ={}\n\
                    ldr x0, [x0]\n\
                    svc #0\n\
                ",
                    libc::SYS_exit,
                    std::str::from_utf8(identifier).unwrap()
                )
                .unwrap(),
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Assign) => match current.statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                }), Value::Literal(Literal::Integer(y))] => {
                    match type_data.get(identifier).unwrap() {
                        Type::U8 => write!(
                            assembly,
                            "\
                            ldr x0, ={}\n\
                            mov w1, #{y}\n\
                            strb w1, [x0]\n\
                        ",
                            std::str::from_utf8(identifier).unwrap()
                        )
                        .unwrap(),
                        Type::U64 => write!(
                            assembly,
                            "\
                            ldr x0, ={}\n\
                            mov x1, #{y}\n\
                            str x1, [x0]\n\
                        ",
                            std::str::from_utf8(identifier).unwrap()
                        )
                        .unwrap(),
                        _ => todo!(),
                    }
                }

                [Value::Variable(Variable {
                    identifier: _,
                    index: None,
                }), Value::Literal(Literal::String(_))] => {
                    todo!()
                }
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                }), rest @ ..] => match type_data.get(identifier).unwrap() {
                    Type::Array(box Array {
                        item: Type::U8,
                        len,
                    }) => {
                        assert_eq!(*len, rest.len());

                        // Loads the address of the array.
                        writeln!(
                            assembly,
                            "ldr x0, ={}",
                            std::str::from_utf8(identifier).unwrap()
                        )
                        .unwrap();

                        // Packs 4 bytes into stores of the full 64 bit register.
                        let mut chunks = rest.array_chunks::<4>();
                        let full_chunks = (0..).step_by(4).zip(chunks.by_ref());
                        for (offset, [a, b, c, d]) in full_chunks {
                            let e = (*a.literal().unwrap().integer().unwrap() as u64) << 24;
                            let f = (*b.literal().unwrap().integer().unwrap() as u64) << 16;
                            let g = (*c.literal().unwrap().integer().unwrap() as u64) << 8;
                            let h = *d.literal().unwrap().integer().unwrap() as u64;
                            let overall = e & f & g & h;

                            write!(
                                assembly,
                                "\
                                mov x1, #{overall}\n\
                                str x1, [x0, #{offset}]\n\
                            ",
                            )
                            .unwrap();
                        }

                        // Does individual stores for remaining bytes.
                        let rem_offset = 4 * (rest.len() / 4);
                        for (offset, x) in chunks.remainder().iter().enumerate() {
                            let y = *x.literal().unwrap().integer().unwrap() as u8;
                            write!(
                                assembly,
                                "\
                                mov x1 #{y}\n\
                                strb w1, [x0 #{}] 
                                ",
                                rem_offset + offset
                            )
                            .unwrap();
                        }
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::AddAssign) => match current.statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                }), Value::Literal(Literal::Integer(y))] => {
                    match type_data.get(identifier).unwrap() {
                        Type::U8 => write!(
                            &mut assembly,
                            "\
                            ldr x0, ={}\n\
                            ldr w1, [x0]\n\
                            add w1, w1, #{y}\n\
                            strb w1, [x0]\n\
                        ",
                            std::str::from_utf8(identifier).unwrap()
                        )
                        .unwrap(),
                        Type::U64 => write!(
                            &mut assembly,
                            "\
                            ldr x0, ={}\n\
                            ldr x1, [x0]\n\
                            add x1, x1, #{y}\n\
                            str x1, [x0]\n\
                        ",
                            std::str::from_utf8(identifier).unwrap()
                        )
                        .unwrap(),
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::SubAssign) => match current.statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                }), Value::Literal(Literal::Integer(y))] => {
                    match type_data.get(identifier).unwrap() {
                        Type::U8 => write!(
                            &mut assembly,
                            "\
                            ldr x0, ={}\n\
                            ldr w1, [x0]\n\
                            sub w1, w1, #{y}\n\
                            strb w1, [x0]\n\
                        ",
                            std::str::from_utf8(identifier).unwrap()
                        )
                        .unwrap(),
                        Type::U64 => write!(
                            &mut assembly,
                            "\
                            ldr x0, ={}\n\
                            ldr x1, [x0]\n\
                            sub x1, x1, #{y}\n\
                            str x1, [x0]\n\
                        ",
                            std::str::from_utf8(identifier).unwrap()
                        )
                        .unwrap(),
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::If(Cmp::Eq)) => match current.statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                }), Value::Literal(Literal::Integer(y))] => {
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
                        std::str::from_utf8(identifier).unwrap(),
                        if let Some(child) = current.child {
                            instruction_from_node(
                                nodes,
                                child,
                                data,
                                bss,
                                block_counter,
                                empty,
                                type_data,
                            )
                        } else {
                            String::new()
                        }
                    )
                    .unwrap();
                    *block_counter += 1;
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Read) => match current.statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                }), Value::Literal(Literal::Integer(fd))] => {
                    write!(
                        &mut assembly,
                        "\
                        mov x8, #{}\n\
                        mov x0, #{fd}\n\
                        ldr x1, ={}\n\
                        mov x2, #{}\n\
                        svc #0\n\
                    ",
                        libc::SYS_read,
                        std::str::from_utf8(identifier).unwrap(),
                        type_data.get(identifier).unwrap().bytes()
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Write) => match current.statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier: v,
                    index: None,
                }), Value::Literal(Literal::Integer(fd)), Value::Variable(Variable {
                    identifier: x,
                    index: None,
                }), Value::Literal(Literal::Integer(n))]
                    if v == b"_" =>
                {
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
            Op::Syscall(Syscall::MemfdCreate) => match current.statement.arg.as_slice() {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                })] => {
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
                        std::str::from_utf8(identifier).unwrap()
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
                        std::str::from_utf8(identifier).unwrap()
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
