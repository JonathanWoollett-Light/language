use crate::ast::*;
use crate::LOOP_LIMIT;
use std::collections::HashMap;
use std::fmt::Write;

use std::iter::once;
use std::ptr::NonNull;

pub fn assembly_from_node(nodes: NonNull<NewNode>) -> String {
    let mut data = String::new();
    let mut bss = String::new();
    let mut block_counter = 0;
    // Have we defined an empty string in the data section to use for anonymous `memfd_create`.
    let mut empty = false;
    let mut type_data = HashMap::new();

    let assembly = instruction_from_node(
        nodes,
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

// pub fn empty(
//     _assembly: &mut String,
//     _data: &mut String,
//     _bss: &mut String,
//     _block_counter: &mut usize,
//     _empty: &mut bool,
//     _type_data: &mut HashMap<Identifier, Type>,
// ) {

// }
// pub fn if_callback(
//     assembly: &mut String,
//     data: &mut String,
//     bss: &mut String,
//     block_counter: &mut usize,
//     empty: &mut bool,
//     type_data: &mut HashMap<Identifier, Type>,
// ) {

// }

pub fn instruction_from_node(
    nodes: NonNull<NewNode>,
    data: &mut String,
    bss: &mut String,
    block_counter: &mut usize,
    empty: &mut bool,
    type_data: &mut HashMap<Identifier, Type>,
) -> String {
    let mut assembly = String::new();

    let mut stack = vec![nodes];
    #[cfg(debug_assertions)]
    let mut i = 0;

    let mut write_stack = Vec::new();

    while let Some(current) = stack.pop() {
        #[cfg(debug_assertions)]
        {
            assert!(i < LOOP_LIMIT);
            i += 1;
        }

        let current_ref = unsafe { current.as_ref() };
        let statement = &current_ref.statement;
        let arg = statement.arg.as_slice();

        match statement.op {
            Op::Special(Special::Type) => match arg {
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
                [Value::Variable(Variable { identifier, .. }), Value::Type(value_type), Value::Literal(literal)] =>
                {
                    type_data.insert(identifier.clone(), value_type.clone());
                    let data_value = match value_type {
                        Type::U8 => format!(".byte {}", literal.integer().unwrap()),
                        Type::U16 => format!(".2byte {}", literal.integer().unwrap()),
                        Type::U32 => format!(".4byte {}", literal.integer().unwrap()),
                        Type::U64 => format!(".8byte {}", literal.integer().unwrap()),
                        Type::Array(box Array { item, len }) => {
                            assert_eq!(*item, Type::U8);
                            let string = literal.string().unwrap();
                            assert_eq!(string.as_bytes().len(), *len);

                            format!(
                                ".byte {}",
                                string
                                    .as_bytes()
                                    .iter()
                                    .map(|b| b.to_string())
                                    .intersperse(String::from(","))
                                    .collect::<String>()
                            )
                        }
                        // TODO I'm pretty sure this is wrong.
                        Type::I8 => format!(".byte {}", literal.integer().unwrap()),
                        Type::I16 => format!(".2byte {}", literal.integer().unwrap()),
                        Type::I32 => format!(".4byte {}", literal.integer().unwrap()),
                        Type::I64 => format!(".8byte {}", literal.integer().unwrap()),
                    };

                    writeln!(
                        data,
                        "{}: {data_value}",
                        std::str::from_utf8(identifier).unwrap(),
                    )
                    .unwrap();
                }
                [Value::Variable(Variable { identifier, .. }), Value::Type(value_type), Value::Literal(literal), tail @ ..] =>
                {
                    type_data.insert(identifier.clone(), value_type.clone());
                    let iter = once(literal).chain(tail.iter().map(|v| v.literal().unwrap()));

                    let data_value = match value_type {
                        Type::Array(box Array { item: Type::U8, .. }) => format!(
                            ".byte {}",
                            iter.map(|l| l.integer().unwrap().to_string())
                                .intersperse(String::from(","))
                                .collect::<String>()
                        ),
                        Type::Array(box Array {
                            item: Type::U16, ..
                        }) => format!(
                            ".2byte {}",
                            iter.map(|l| l.integer().unwrap().to_string())
                                .intersperse(String::from(","))
                                .collect::<String>()
                        ),
                        Type::Array(box Array {
                            item: Type::U32, ..
                        }) => format!(
                            ".4byte {}",
                            iter.map(|l| l.integer().unwrap().to_string())
                                .intersperse(String::from(","))
                                .collect::<String>()
                        ),
                        Type::Array(box Array {
                            item: Type::U64, ..
                        }) => format!(
                            ".8byte {}",
                            iter.map(|l| l.integer().unwrap().to_string())
                                .intersperse(String::from(","))
                                .collect::<String>()
                        ),
                        _ => todo!(),
                    };

                    writeln!(
                        data,
                        "{}: {data_value}",
                        std::str::from_utf8(identifier).unwrap(),
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Exit) => match arg {
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
                })] => match type_data.get(identifier).unwrap() {
                    Type::U8 => write!(
                        &mut assembly,
                        "\
                            mov x8, #{}\n\
                            ldr x0, ={}\n\
                            ldrb w0, [x0]\n\
                            svc #0\n\
                        ",
                        libc::SYS_exit,
                        std::str::from_utf8(identifier).unwrap()
                    )
                    .unwrap(),
                    _ => todo!(),
                },
                _ => todo!(),
            },
            Op::Intrinsic(Intrinsic::Assign) => match arg {
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
                    identifier,
                    index: None,
                }), Value::Literal(Literal::String(string))] => {
                    let bytes = string.as_bytes();
                    let Type::Array(box Array {
                        item: Type::U8,
                        len,
                    }) = type_data.get(identifier).unwrap()
                    else {
                        panic!("Can't assign string to non-U8 array")
                    };
                    assert_eq!(*len, bytes.len());

                    // Loads the address of the array.
                    writeln!(
                        assembly,
                        "ldr x0, ={}",
                        std::str::from_utf8(identifier).unwrap()
                    )
                    .unwrap();

                    // Packs 2 bytes into stores of the full 32 bit register.
                    let mut chunks = bytes.array_chunks::<2>();
                    let full_chunks = (0..).step_by(2).zip(chunks.by_ref());
                    for (offset, [a, b]) in full_chunks {
                        let d = (*b as u32) << 8;
                        let c = *a as u32;
                        let overall = c | d;

                        write!(
                            assembly,
                            "\
                            mov w1, #{overall}\n\
                            str w1, [x0, {offset}]\n\
                        ",
                        )
                        .unwrap();
                    }

                    // Does individual stores for remaining bytes.
                    let rem_offset = 2 * (bytes.len() / 2);
                    for (offset, x) in chunks.remainder().iter().enumerate() {
                        write!(
                            assembly,
                            "\
                            mov w1, #{x}\n\
                            strb w1, [x0, {}]\n\
                            ",
                            rem_offset + offset
                        )
                        .unwrap();
                    }
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

                        // Packs 2 bytes into stores of the full 32 bit register.
                        let mut chunks = rest.array_chunks::<2>();
                        let full_chunks = (0..).step_by(2).zip(chunks.by_ref());
                        for (offset, [a, b]) in full_chunks {
                            let d = (*b.literal().unwrap().integer().unwrap() as u32) << 8;
                            let c = *a.literal().unwrap().integer().unwrap() as u32;
                            let overall = c | d;

                            write!(
                                assembly,
                                "\
                                mov w1, #{overall}\n\
                                str w1, [x0, {offset}]\n\
                            ",
                            )
                            .unwrap();
                        }

                        // Does individual stores for remaining bytes.
                        let rem_offset = 2 * (rest.len() / 2);
                        for (offset, x) in chunks.remainder().iter().enumerate() {
                            let y = *x.literal().unwrap().integer().unwrap() as u8;
                            write!(
                                assembly,
                                "\
                                mov w1, #{y}\n\
                                strb w1, [x0, {}]\n\
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
            Op::Intrinsic(Intrinsic::AddAssign) => match arg {
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
            Op::Intrinsic(Intrinsic::SubAssign) => match arg {
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
            Op::Intrinsic(Intrinsic::If(Cmp::Eq)) => match arg {
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
                    ",
                        std::str::from_utf8(identifier).unwrap(),
                    )
                    .unwrap();

                    let end = format!("block{block_counter}:\n");
                    *block_counter += 1;

                    if let Some(child) = current_ref.next {
                        write_stack.push((current, end));
                        stack.push(child);
                    }
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Read) => match arg {
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
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                }), Value::Type(variable_type), Value::Literal(Literal::Integer(fd))] => {
                    type_data.insert(identifier.clone(), variable_type.clone());
                    write!(
                        bss,
                        "\
                        {}:\n\
                        .skip {}\n\
                    ",
                        std::str::from_utf8(identifier).unwrap(),
                        variable_type.bytes()
                    )
                    .unwrap();

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
                        variable_type.bytes()
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::Write) => match arg {
                [Value::Variable(Variable {
                    identifier: v,
                    index: None,
                }), Value::Literal(Literal::Integer(fd)), Value::Variable(Variable {
                    identifier,
                    index: None,
                })] if v == b"_" => {
                    write!(
                        &mut assembly,
                        "\
                        mov x8, #{}\n\
                        mov x0, #{fd}\n\
                        ldr x1, ={}\n\
                        mov x2, #{}\n\
                        svc #0\n\
                    ",
                        libc::SYS_write,
                        std::str::from_utf8(identifier).unwrap(),
                        type_data.get(identifier).unwrap().bytes()
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            Op::Syscall(Syscall::MemfdCreate) => match arg {
                [Value::Variable(Variable {
                    identifier,
                    index: None,
                })] => match type_data.get(identifier) {
                    Some(Type::I64) => {
                        write!(
                            &mut assembly,
                            "\
                            mov x8, #{}\n\
                            ldr x0, =empty\n\
                            mov x1, #0\n\
                            svc #0\n\
                            ldr x1, ={}\n\
                            str w0, [x1, 4]\n\
                        ",
                            libc::SYS_memfd_create,
                            std::str::from_utf8(identifier).unwrap()
                        )
                        .unwrap();

                        // Define an empty null terminated string.
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
                    }
                    Some(Type::I32) => {
                        write!(
                            &mut assembly,
                            "\
                            mov x8, #{}\n\
                            ldr x0, =empty\n\
                            mov x1, #0\n\
                            svc #0\n\
                            ldr x1, ={}\n\
                            str w0, [x1]\n\
                        ",
                            libc::SYS_memfd_create,
                            std::str::from_utf8(identifier).unwrap()
                        )
                        .unwrap();

                        // Define an empty null terminated string.
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
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            },
            _ => todo!(),
        }

        if let Some(next) = current_ref.next {
            stack.push(next);
        } else {
            // Get parent
            let mut preceding = current_ref.preceding;
            while let Some(Preceding::Previous(previous)) = preceding {
                preceding = unsafe { previous.as_ref().preceding };
            }

            match preceding {
                None => {}
                Some(Preceding::Parent(parent)) => {
                    while let Some((node, end)) = write_stack.pop() {
                        assembly.push_str(&end);
                        if node == parent {
                            break;
                        }
                    }
                }
                Some(Preceding::Previous(_)) => unreachable!(),
            }
        }
    }

    assembly
}
