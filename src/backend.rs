use crate::ast::*;
use crate::LOOP_LIMIT;
use std::collections::HashMap;
use std::fmt::Write;

use std::ptr::NonNull;

pub fn assembly_from_node(nodes: NonNull<AstNode>) -> String {
    let mut data = String::new();
    let mut bss = String::new();
    let mut block_counter = 0;
    // Have we defined an empty string in the data section to use for anonymous `memfd_create`.
    let mut empty = false;
    let mut type_data = HashMap::new();

    let assembly = unsafe {
        instruction_from_node(
            nodes,
            &mut data,
            &mut bss,
            &mut block_counter,
            &mut empty,
            &mut type_data,
        )
    };

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

pub unsafe fn instruction_from_node(
    nodes: NonNull<AstNode>,
    _data: &mut String,
    bss: &mut String,
    block_counter: &mut usize,
    _empty: &mut bool,
    type_data: &mut HashMap<Identifier, Type>,
) -> String {
    let mut assembly = String::new();

    let mut stack = vec![nodes];
    #[cfg(debug_assertions)]
    let mut i = 0;

    // The list of nodes that created new scopes and their scope endings.
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

        // eprintln!("type_data: {type_data:?}");
        // eprintln!("statement.op: {:?}",statement.op);
        // eprintln!("arg: {arg:?}");

        match statement.op {
            Op::Assign => match arg {
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: Some(cast),
                })] => {
                    let type_cast = match cast {
                        Cast::As(type_cast) => type_cast,
                        Cast::Prev => todo!(),
                    };
                    type_data.insert(identifier.clone(), type_cast.clone());
                    writeln!(bss, "{identifier}: .skip {}", type_cast.bytes()).unwrap();
                }
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: _,
                }), Value::Literal(Literal::Integer(y))] => match type_data.get(identifier).unwrap() {
                    Type::Integer(IntegerType::U8) => write!(
                        assembly,
                        "\
                            ldr x0, ={identifier}\n\
                            mov w1, #{y}\n\
                            strb w1, [x0]\n\
                        "
                    )
                    .unwrap(),
                    Type::Integer(IntegerType::U64) => write!(
                        assembly,
                        "\
                            ldr x0, ={identifier}\n\
                            mov x1, #{y}\n\
                            str x1, [x0]\n\
                        "
                    )
                    .unwrap(),
                    _ => todo!(),
                },
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: _,
                }), Value::Literal(Literal::String(string))] => {
                    let bytes = string.as_bytes();
                    let Type::Array(box Array(vec)) = type_data.get(identifier).unwrap() else {
                        panic!("Can't assign string to non-U8 array")
                    };
                    assert!(vec.iter().all(|t| *t == Type::Integer(IntegerType::U8)));
                    assert_eq!(vec.len(), bytes.len());

                    // Loads the address of the array.
                    writeln!(assembly, "ldr x0, ={identifier}").unwrap();

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
                    addressing: Addressing::Direct,
                    identifier: lhs_identifier,
                    index: None,
                    cast: _,
                }), Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier: rhs_identifier,
                    index: None,
                    cast: _,
                })] => {
                    let rhs_type = type_data.get(rhs_identifier).unwrap();
                    match rhs_type {
                        Type::Integer(IntegerType::U8) => {
                            // Load data and store data in new location.
                            write!(
                                assembly,
                                "\
                                ldr x0, ={rhs_identifier}\n\
                                ldrb w1, [x0]\n\
                                ldr x0, ={lhs_identifier}\n\
                                strb w1, [x0]\n\
                            "
                            )
                            .unwrap();
                            writeln!(bss, "{lhs_identifier}: .skip 1").unwrap();
                        }
                        _ => todo!(),
                    }
                    type_data.insert(lhs_identifier.clone(), rhs_type.clone());
                }
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: _,
                }), rest @ ..] => match type_data.get(identifier).unwrap() {
                    Type::Array(box Array(vec)) if vec.iter().all(|t| *t == Type::Integer(IntegerType::U8)) => {
                        assert_eq!(vec.len(), rest.len());

                        // Loads the address of the array.
                        writeln!(assembly, "ldr x0, ={identifier}").unwrap();

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
            Op::AddAssign => match arg {
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: _,
                }), Value::Literal(Literal::Integer(y))] => match type_data.get(identifier).unwrap() {
                    Type::Integer(IntegerType::U8) => write!(
                        &mut assembly,
                        "\
                            ldr x0, ={identifier}\n\
                            ldr w1, [x0]\n\
                            add w1, w1, #{y}\n\
                            strb w1, [x0]\n\
                        "
                    )
                    .unwrap(),
                    Type::Integer(IntegerType::U64) => write!(
                        &mut assembly,
                        "\
                            ldr x0, ={identifier}\n\
                            ldr x1, [x0]\n\
                            add x1, x1, #{y}\n\
                            str x1, [x0]\n\
                        "
                    )
                    .unwrap(),
                    _ => todo!(),
                },
                _ => todo!(),
            },
            Op::SubAssign => match arg {
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: _,
                }), Value::Literal(Literal::Integer(y))] => match type_data.get(identifier).unwrap() {
                    Type::Integer(IntegerType::U8) => write!(
                        &mut assembly,
                        "\
                            ldr x0, ={identifier}\n\
                            ldr w1, [x0]\n\
                            sub w1, w1, #{y}\n\
                            strb w1, [x0]\n\
                        "
                    )
                    .unwrap(),
                    Type::Integer(IntegerType::U64) => write!(
                        &mut assembly,
                        "\
                            ldr x0, ={identifier}\n\
                            ldr x1, [x0]\n\
                            sub x1, x1, #{y}\n\
                            str x1, [x0]\n\
                        "
                    )
                    .unwrap(),
                    _ => todo!(),
                },
                _ => todo!(),
            },
            Op::If(Cmp::Eq) => match arg {
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: _,
                }), Value::Literal(Literal::Integer(y))] => {
                    write!(
                        &mut assembly,
                        "\
                        ldr x0, ={identifier}\n\
                        ldr x0, [x0]\n\
                        cmp w0, #{y}\n\
                        bne block{block_counter}\n\
                    "
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
            Op::Mov => match arg {
                [Value::Register(register), Value::Literal(Literal::Integer(integer))] => {
                    writeln!(&mut assembly, "mov {register}, #{integer}").unwrap();
                }
                [Value::Register(register), Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: _,
                })] => match type_data.get(identifier).unwrap() {
                    Type::Integer(IntegerType::U8) => {
                        // Load data and store data in new location.
                        write!(
                            assembly,
                            "\
                            ldr x0, ={identifier}\n\
                            ldrb {}, [x0]\n\
                        ",
                            register.w()
                        )
                        .unwrap();
                    }
                    _ => todo!(),
                },
                [Value::Register(register), Value::Variable(Variable {
                    addressing: Addressing::Reference,
                    identifier,
                    index: None,
                    cast: _,
                })] => {
                    writeln!(&mut assembly, "ldr {register}, ={identifier}",).unwrap();
                }
                x @ _ => todo!("{x:?}"),
            },
            Op::Svc => match arg {
                [Value::Literal(Literal::Integer(integer))] => {
                    writeln!(&mut assembly, "svc #{integer}").unwrap();
                }
                _ => todo!(),
            },
            Op::Unreachable => {
                assert!(arg.is_empty());
            }
            _ => todo!(),
        }

        if let Some(next) = current_ref.next {
            stack.push(next);
        } else {
            // Get parent
            let parent_opt = {
                let mut preceding = current_ref.preceding;
                #[cfg(debug_assertions)]
                let mut checker = 0;

                loop {
                    #[cfg(debug_assertions)]
                    {
                        assert!(checker < 100);
                        checker += 1;
                    }
                    preceding = match preceding {
                        Some(Preceding::Previous(p)) => p.as_ref().preceding,
                        Some(Preceding::Parent(p)) => break Some(p),
                        None => break None,
                    };
                }
            };
            // If there is no parent the write stack should be empty.
            debug_assert!(
                (parent_opt.is_none() && write_stack.is_empty()) || parent_opt.is_some(),
                "({:?} && {:?}) || {:?}",
                parent_opt.is_none(),
                write_stack.is_empty(),
                parent_opt.is_some()
            );
            // Pop scopes from the write stack.
            if let Some(parent) = parent_opt {
                while let Some((node, end)) = write_stack.pop() {
                    assembly.push_str(&end);
                    if node == parent {
                        break;
                    }
                }
            }
        }
    }

    assembly
}
