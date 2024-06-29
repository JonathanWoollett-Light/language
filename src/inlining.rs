use crate::ast::*;
use std::alloc::alloc;
use std::alloc::dealloc;
use std::alloc::Layout;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::iter::once;
use std::ptr;
use std::ptr::NonNull;
use std::rc::Rc;

unsafe fn update_variables(
    args: &mut Vec<Value>,
    map: &mut HashMap<Identifier, Identifier>,
    identifier_iterator: &mut impl Iterator<Item = Identifier>,
) {
    for variable in args.iter_mut().filter_map(Value::variable_mut) {
        let new_identifier = map
            .entry(variable.identifier.clone())
            .or_insert_with(|| identifier_iterator.next().unwrap());
        variable.identifier = new_identifier.clone();
    }
}

type IdentifierMap = Rc<RefCell<HashMap<Identifier, Identifier>>>;

unsafe fn inline_expression(
    _child: &mut Option<(NonNull<AstNode>, IdentifierMap)>,
    next: &mut VecDeque<(NonNull<AstNode>, IdentifierMap)>,
    (mut current, map): (NonNull<AstNode>, IdentifierMap),
    mut identifier_iterator: &mut impl Iterator<Item = Identifier>,
) {
    let current_mut = current.as_mut();
    let Line::Source(Expression { lhs, rhs, op, .. }) = &mut current_mut.line else {
        todo!()
    };

    let rhs = match rhs {
        Nested::Expression(box expr) => {
            let out = Variable::from(identifier_iterator.next().unwrap());
            let mut new_expr = expr.clone();
            new_expr.out = Some(out.clone());
            let ptr = alloc(Layout::new::<AstNode>()).cast();
            ptr::write(
                ptr,
                AstNode {
                    line: Line::Source(new_expr),
                    preceding: current_mut.preceding,
                    child: None,
                    next: Some(current),
                },
            );
            let new_node = NonNull::new(ptr).unwrap();
            if let Some(preceding) = current_mut.preceding {
                match preceding {
                    Preceding::Parent(mut parent) => {
                        parent.as_mut().child = Some(new_node);
                    }
                    Preceding::Previous(mut previous) => {
                        previous.as_mut().next = Some(new_node);
                    }
                }
            }
            current_mut.preceding = Some(Preceding::Previous(new_node));
            *rhs = Nested::Values(vec![Value::Variable(out)]);
            next.push_front((new_node, map.clone()));
            return;
        }
        Nested::Values(values) => values,
    };

    // Update variable names
    update_variables(lhs, &mut *map.borrow_mut(), &mut identifier_iterator);
    update_variables(rhs, &mut *map.borrow_mut(), &mut identifier_iterator);

    // Inline ops/operands/functions
    todo!()
}

pub unsafe fn inline_functions(node: NonNull<AstNode>) -> NonNull<AstNode> {
    // The map of function identifiers to the map of functions definitions when the function of a
    // given identifier was defined (a function only uses functions defined before it and a later
    // function may use a different function with the same name).
    let mut functions = HashMap::new();
    // The map of functions identifiers to function definitions.
    let mut function_map = HashMap::new();

    // The collection of function definitions so they can be deallocated.
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

    let mut first = node;
    let mut child = None;
    let mut next = VecDeque::from([(node, IdentifierMap::default())]);

    while let Some((mut current, map)) = child.take().or_else(|| next.pop_front()) {
        let current_mut = current.as_mut();
        let Line::Source(Expression { rhs, lhs, op, .. }) = &mut current_mut.line else {
            todo!()
        };

        // TODO: Is this really the best way to do this?
        if current_mut.preceding.is_none() {
            first = current;
        }

        // If the rhs is a nested expression unroll this.
        match op.as_string().as_str() {
            "break" => unreachable!(),
            // Since `valueof` is used to get the AST structure from some given code it shouldn't
            // unroll becuase this would change the returned structure.
            "valueof" => {
                child = current_mut.child.map(|c| (c, map.clone()));
                if let Some(n) = current_mut.next {
                    next.push_front((n, map.clone()));
                }
                continue;
            }
            // Assume should contain a singlely nested expression and when unrolling for doubly
            // nested expression these unrolled expressions should still be prefixed with `assume`
            // and thus single nested.
            "assume" => {
                todo!()
            }
            "if" => {
                inline_expression(&mut child, &mut next, (current, map), &mut identifier_iterator);
                todo!();
            }
            "loop" => {
                inline_expression(&mut child, &mut next, (current, map), &mut identifier_iterator);
                todo!();
            }
            "def" => {
                // Get function name
                assert!(lhs.is_empty());
                let Nested::Values(rhs) = rhs else { panic!() };
                let [Value::Variable(Variable {
                    addressing: Addressing::Direct,
                    identifier,
                    index: None,
                })] = rhs.as_slice()
                else {
                    panic!()
                };
                // Extract function AST
                if let Some(preceding) = current_mut.preceding {
                    match preceding {
                        Preceding::Parent(mut parent) => {
                            parent.as_mut().child = current_mut.next;
                        }
                        Preceding::Previous(mut previous) => {
                            previous.as_mut().next = current_mut.next;
                        }
                    }
                }
                if let Some(mut next_node) = current_mut.next {
                    next_node.as_mut().preceding = current_mut.preceding;
                    next.push_front((next_node, map)); // Enque next node
                }
                // Insert new function name
                let function = current_mut.child.map(|mut first| {
                    first.as_mut().preceding = None;
                    first
                });
                definitions.push(function);
                functions.insert(identifier.clone(), function_map.clone());
                function_map.insert(identifier.clone(), function);

                // Deallocate definition
                dealloc(current.as_ptr().cast(), Layout::new::<AstNode>());
            }
            "fail" => {
                assert!(lhs.is_empty());
                assert!(matches!(rhs, Nested::Values(rhsv) if rhsv.is_empty()));
                assert!(current_mut.child.is_none());
                if let Some(next_node) = current_mut.next {
                    next.push_front((next_node, map)); // Enque next node
                }
            }
            _ => {
                inline_expression(&mut child, &mut next, (current, map), &mut identifier_iterator);
            }
        };
    }

    // Deallocate function definitions
    for definition in definitions.into_iter().filter_map(|c| c) {
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
