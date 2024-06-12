use crate::ast::*;
use std::alloc::alloc;
use std::alloc::dealloc;
use std::alloc::Layout;
use std::collections::HashMap;
use std::iter::once;
use std::ptr;
use std::ptr::NonNull;

pub unsafe fn inline_functions(node: NonNull<AstNode>) -> NonNull<AstNode> {
    let mut first = node;

    // The map from function identifiers to the first node of their definition.
    let mut functions = HashMap::new();

    // We collect the function definitions we find to ultimately dealloc.
    let mut definitions = Vec::new();

    // Generates unique identifiers.
    const N: u8 = b'z' - b'a';
    let mut identifier_iterator = (0..).map(|index| {
        Identifier(
            (0..index / N)
                .map(|_| 'z')
                .chain(std::iter::once(char::from_u32(((index % N) + b'a') as u32).unwrap()))
                .collect::<Vec<_>>(),
        )
    });

    let mut stack = vec![node];
    let mut carry = Vec::new();
    let mut maps = vec![HashMap::<Identifier, Identifier>::new()];

    while let Some(mut current) = stack.pop() {
        // TODO We need a better way to mark end of functions when inlining. This approach doesn't
        // work for idented functions which dont have a next.

        // When reaching the node after the last node from an inlined function the context switches back.
        if let Some(c) = carry.last_mut() {
            *c -= 1;
            if *c == 0 {
                carry.pop().unwrap();
                maps.pop().unwrap();
                stack.push(current);
                continue;
            }
        }

        let current_ref = current.as_mut();

        // Update first node.
        if current_ref.preceding.is_none() {
            first = current;
        }

        // let s = current_ref.statement.to_string();
        // eprintln!(
        //     "{:?} {s}{}{:?}",
        //     current.as_ptr(),
        //     " ".repeat(20 - s.chars().count()),
        //     maps[carry.len()]
        //         .iter()
        //         .map(|(k, v)| (k.clone(), v.clone()))
        //         .collect::<std::collections::BTreeMap<_, _>>()
        // );

        // Update variables.
        let args = current_ref.statement.arg.as_mut_slice();
        for variable in args.iter_mut().filter_map(Value::variable_mut) {
            let variable_map = maps.get_mut(carry.len()).unwrap();
            if variable_map.len() > 10 {
                panic!();
            }

            let new_identifier = variable_map
                .entry(variable.identifier.clone())
                .or_insert_with(|| identifier_iterator.next().unwrap());
            variable.identifier = new_identifier.clone();
        }

        // eprintln!("{}", current_ref.statement);

        // Handle continuation.
        let args = current_ref.statement.arg.as_mut_slice();
        match current_ref.statement.op {
            Op::Def => {
                // Definition statements should only contain a function identifier.
                let [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                    cast: None,
                })] = args
                else {
                    todo!()
                };

                // Update the function map.
                functions.insert(identifier.clone(), current);

                // Record the definition to later dealloc.
                definitions.push(current);

                // Remove links to the definition.
                if let Some(preceding) = current_ref.preceding {
                    match preceding {
                        Preceding::Previous(mut previous) => {
                            previous.as_mut().child = current_ref.next;
                        }
                        Preceding::Parent(mut parent) => {
                            parent.as_mut().next = current_ref.next;
                        }
                    }
                }

                if let Some(mut next) = current_ref.next.take() {
                    next.as_mut().preceding = current_ref.preceding;
                    stack.push(next);
                }
            }
            Op::Call => {
                // A function call will always be a function identifier followed by a tail of values.
                let [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier: fn_ident,
                    index: None,
                    cast: _,
                }), tail @ ..] = args
                else {
                    todo!()
                };

                // eprintln!("\n\ndefinitions\n\n{}",definitions.iter().copied().map(|def|format!("{}\n\n",crate::display_ast_addresses(def))).collect::<String>());

                let fn_def = functions.get(fn_ident).unwrap();

                assert_eq!(current_ref.child, None);

                // Push fn map
                // E.g. `x = 1` becomes `a = 1` then `exit x` becomes `exit a` this then becomes
                // `in = a` which we then want to become `b = a` so to avoid re-creating `a` here
                // the function map needs to carry through the identifiers passed to it.
                maps.push(
                    tail.iter()
                        .filter_map(|t| t.variable().map(|v| (v.identifier.clone(), v.identifier.clone())))
                        .collect(),
                );

                // Change call node to the entry node (e.g. `write 2 b` becomes `in = 2 b`).
                current_ref.statement = Statement {
                    op: Op::Assign,
                    arg: once(Value::Variable(Variable {
                        addressing: Addressing::Direct,
                        identifier: Identifier::fn_in(),
                        index: None,
                        cast: None,
                    }))
                    .chain(tail.iter().cloned())
                    .collect(),
                };

                // Push next node to the carry.
                stack.push(current);

                // Replace the next node leading into the function.
                let outer_next = current_ref.next;
                current_ref.next = fn_def.as_ref().child;

                // The function stack contains clones of the functions nodes that are inlined.
                let mut fn_stack = vec![(current, 0)];
                let mut carry_counter = 0;
                while let Some((mut fn_node, fn_depth)) = fn_stack.pop() {
                    carry_counter += 1;
                    if let Some(child) = fn_node.as_ref().child {
                        // Clone child node.
                        let new_child_ptr = alloc(Layout::new::<AstNode>()).cast();
                        ptr::write(
                            new_child_ptr,
                            AstNode {
                                statement: child.as_ref().statement.clone(),
                                preceding: Some(Preceding::Parent(fn_node)),
                                child: child.as_ref().child,
                                next: child.as_ref().next,
                            },
                        );
                        let new_child = NonNull::new(new_child_ptr).unwrap();
                        assert_ne!(child, new_child);
                        // Point to new child.
                        fn_node.as_mut().child = Some(new_child);
                        // Push child to fn stack.
                        fn_stack.push((new_child, fn_depth + 1));
                    }
                    if let Some(next) = fn_node.as_ref().next {
                        // Clone next node.
                        let new_next_ptr = alloc(Layout::new::<AstNode>()).cast();
                        ptr::write(
                            new_next_ptr,
                            AstNode {
                                statement: next.as_ref().statement.clone(),
                                preceding: Some(Preceding::Previous(fn_node)),
                                child: next.as_ref().child,
                                next: next.as_ref().next,
                            },
                        );
                        let new_next = NonNull::new(new_next_ptr).unwrap();
                        assert_ne!(next, new_next);
                        // Point to new next.
                        fn_node.as_mut().next = Some(new_next);
                        // Push next to fn stack.
                        fn_stack.push((new_next, fn_depth));
                    } else if fn_depth == 0 {
                        assert_eq!(fn_stack, []);
                        fn_node.as_mut().next = outer_next;
                    }
                }
                assert_ne!(current.as_ref().next, fn_def.as_ref().child);

                // Push carry node.
                carry.push(carry_counter);

                // eprintln!("{}",crate::display_ast_addresses(first));
                // eprintln!("\n\ndefinitions\n{}",definitions.iter().copied().map(|def|format!("{}\n\n",crate::display_ast(def))).collect::<String>());
            }
            Op::Assign => match args {
                [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier: var_ident,
                    ..
                }), Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier: fn_ident,
                    ..
                }), tail @ ..]
                    if let Some(fn_def) = functions.get(fn_ident) =>
                {
                    assert_eq!(current_ref.child, None);

                    // Push fn map
                    // E.g. `x = 1` becomes `a = 1` then `exit x` becomes `exit a` this then becomes
                    // `in = a` which we then want to become `b = a` so to avoid re-creating `a` here
                    // the function map needs to carry through the identifiers passed to it.
                    // Here we also need to pass-through the `out` ident map.
                    maps.push(
                        once((Identifier::fn_out(), var_ident.clone()))
                            .chain(
                                tail.iter()
                                    .filter_map(|t| t.variable().map(|v| (v.identifier.clone(), v.identifier.clone()))),
                            )
                            .collect(),
                    );

                    // Change call node to the entry node (e.g. `a = read 2 b` becomes `in = 2 b`).
                    current_ref.statement = Statement {
                        op: Op::Assign,
                        arg: once(Value::Variable(Variable {
                            addressing: Addressing::Direct,
                            identifier: Identifier::fn_in(),
                            index: None,
                            cast: None,
                        }))
                        .chain(tail.iter().cloned())
                        .collect(),
                    };

                    // Push next node to the carry.
                    stack.push(current);

                    // Replace the next node leading into the function.
                    let outer_next = current_ref.next;
                    current_ref.next = fn_def.as_ref().child;

                    // The function stack contains clones of the functions nodes that are inlined.
                    let mut fn_stack = vec![(current, 0)];
                    let mut carry_counter = 0;
                    while let Some((mut fn_node, fn_depth)) = fn_stack.pop() {
                        carry_counter += 1;
                        if let Some(child) = fn_node.as_ref().child {
                            // Clone child node.
                            let new_child_ptr = alloc(Layout::new::<AstNode>()).cast();
                            ptr::write(
                                new_child_ptr,
                                AstNode {
                                    statement: child.as_ref().statement.clone(),
                                    preceding: Some(Preceding::Parent(fn_node)),
                                    child: child.as_ref().child,
                                    next: child.as_ref().next,
                                },
                            );
                            let new_child = NonNull::new(new_child_ptr).unwrap();
                            assert_ne!(child, new_child);
                            // Point to new child.
                            fn_node.as_mut().child = Some(new_child);
                            // Push child to fn stack.
                            fn_stack.push((new_child, fn_depth + 1));
                        }
                        if let Some(next) = fn_node.as_ref().next {
                            // Clone next node.
                            let new_next_ptr = alloc(Layout::new::<AstNode>()).cast();
                            ptr::write(
                                new_next_ptr,
                                AstNode {
                                    statement: next.as_ref().statement.clone(),
                                    preceding: Some(Preceding::Previous(fn_node)),
                                    child: next.as_ref().child,
                                    next: next.as_ref().next,
                                },
                            );
                            let new_next = NonNull::new(new_next_ptr).unwrap();
                            assert_ne!(next, new_next);
                            // Point to new next.
                            fn_node.as_mut().next = Some(new_next);
                            // Push next to fn stack.
                            fn_stack.push((new_next, fn_depth));
                        } else if fn_depth == 0 {
                            assert_eq!(fn_stack, []);
                            fn_node.as_mut().next = outer_next;
                        }
                    }
                    assert_ne!(current.as_ref().next, fn_def.as_ref().child);

                    // Push carry node.
                    carry.push(carry_counter);
                }
                _ => {
                    if let Some(child) = current_ref.child {
                        stack.push(child);
                    }
                    if let Some(next) = current_ref.next {
                        stack.push(next);
                    }
                }
            },
            _ => {
                if let Some(child) = current_ref.child {
                    stack.push(child);
                }
                if let Some(next) = current_ref.next {
                    stack.push(next);
                }
            }
        }
    }

    for definition in definitions {
        let mut def_stack = vec![definition];
        while let Some(def_node) = def_stack.pop() {
            if let Some(child) = def_node.as_ref().child {
                def_stack.push(child);
            }
            if let Some(next) = def_node.as_ref().next {
                def_stack.push(next);
            }
            dealloc(def_node.as_ptr().cast(), Layout::new::<AstNode>());
        }
    }
    first
}
