use crate::ast::*;
use crate::LOOP_LIMIT;
use std::cell::OnceCell;
use std::collections::HashSet;
use std::ptr::NonNull;
use thiserror::Error;

#[derive(Debug, Error)]
enum ParseAstError {
    #[error("Indentation must be a multiple of 4 spaces.")]
    IndentationSpacing,
    #[error("Indentation can only be 1 greater than a parent.")]
    IndetationDepth,
    #[error("Failed to parse line: {0}")]
    Line(ParseLineError),
}

pub unsafe fn parse_ast(chars: &[char]) -> Result<Option<NonNull<AstNode>>, ParseAstError> {
    let mut definitions = HashSet::new();
    let mut first = OnceCell::new();
    let mut last = None;
    let mut i = 0;
    // The stack of parent nodes e.g.
    // ```
    // a
    //     b
    //         c
    // ```
    // forms `[a,b,c]`.
    let mut parent_stack = Vec::new();

    #[cfg(debug_assertions)]
    let mut limit = 0..LOOP_LIMIT;
    'outer: loop {
        debug_assert!(limit.next().is_some());
        // Handle indentation.
        let mut spaces = 0;
        loop {
            debug_assert!(limit.next().is_some());
            match chars.get(i + spaces) {
                Some(' ') => {}
                Some(_) => break,
                None => break 'outer,
            }
            spaces += 1
        }
        i += spaces;

        // Handle comments.
        if let Some('#') = chars.get(i) {
            loop {
                debug_assert!(limit.next().is_some());
                i += 1;
                match chars.get(i) {
                    Some('\n') => break,
                    Some(_) => continue,
                    None => break 'outer,
                }
            }
            continue;
        }

        // Handle newlines.
        if let Some('\n') = chars.get(i) {
            i += 1;
            continue;
        }

        // Parse line.
        let mut line_length = 0;
        let line_characters = loop {
            debug_assert!(limit.next().is_some());
            match chars.split_at(i + line_length) {
                (x, ['\n'] | []) => break x,
                _ => {}
            }
            line_length += 1;
        };
        i += line_length;

        let line = parse_line(line_characters, &mut definitions).map_err(ParseAstError::Line)?;
        let node = NonNull::new(Box::into_raw(Box::new(AstNode {
            line,
            preceding: None,
            child: None,
            next: None,
        })))
        .unwrap();

        // Insert into the AST.
        let tabs = div_rem(spaces, 4).ok_or(ParseAstError::IndentationSpacing)?;
        let after = split_off_checked(&mut parent_stack, tabs).ok_or(ParseAstError::IndetationDepth)?;
        if let Some(previous) = after.first_mut() {
            assert_eq!(previous.as_mut().next, None);
            previous.as_mut().next = Some(node);
            node.as_mut().preceding = Some(Preceding::Previous(*previous));
        } else if let Some(parent) = parent_stack.last_mut() {
            assert_eq!(parent.as_mut().child, None);
            parent.as_mut().child = Some(node);
            node.as_mut().preceding = Some(Preceding::Parent(*parent));
        } else {
            first.set(node).unwrap();
        }
        parent_stack.push(node);
    }
    Ok(first.take())
}

#[derive(Debug, Error)]
enum ParseLineError {
    #[error("Failed to parse statement: {0}")]
    Expression(ParseExpressionError),
    #[error("Failed to parse instruction: {0}")]
    Instruction(crate::ParseInstructionError),
}

pub fn parse_line(chars: &[char], definitions: &mut HashSet<Identifier>) -> Result<Line, ParseLineError> {
    let line = if let Some((lhs, rhs)) = chars.split_at_checked(3)
        && lhs == ['a','s','m']
    {
        Line::Assembly(crate::parse_instruction(rhs).map_err(ParseLineError::Instruction)?)
    } else {
        Line::Source(parse_expression(chars, definitions).map_err(ParseLineError::Expression)?)
    };
    Ok(line)
}

#[derive(Debug, Error)]
enum ParseExpressionError {
    #[error("Empty statement")]
    Empty,
    #[error("Badly formed definition")]
    Def,
    #[error("Failed to parse variable: {0}")]
    Variable(ParseVariableError),
    #[error("Failed to parse value: {0}")]
    Value(ParseValueError)
}

pub fn parse_expression(
    chars: &[char],
    definitions: &mut HashSet<Identifier>,
) -> Result<Expression, ParseExpressionError> {
    let mut values = chars.split(|c|*c==' ').collect::<Vec<_>>();

    if let Some(['d','e','f']) = values.first() {
        let [_, key] = values.as_slice() else {
            return Err(ParseExpressionError::Def);
        };
        definitions.insert(Identifier::from(*key));
        return Ok(Expression {
            op: Identifier::from(['d','e','f'].as_slice()),
            lhs: Arg::new(),
            rhs: Nested::Values(vec![Value::Variable(Variable::from(*key))]),
            out: None,
        });
    }

    let out = match values.as_slice() {
        [.., a, b] if *a == ['@'] => {
            let var = parse_variable(b).map_err(ParseExpressionError::Variable)?;
            values.pop();
            values.pop();
            Some(var)
        }
        _ => None,
    };

    let mut partial_opt: Option<Expression> = None;
    let mut section = Arg::new();
    for value in values.into_iter().rev() {
        if let Some(op) = definitions.get(&Identifier::from(value)) {
            if let Some(partial) = &mut partial_opt {
                partial.lhs = section.clone();
                let child = Box::new(partial.clone());
                *partial = Expression {
                    op: op.clone(),
                    lhs: Arg::new(),
                    rhs: Nested::Expression(child),
                    out: out.clone(),
                };
            } else {
                partial_opt = Some(Expression {
                    op: op.clone(),
                    lhs: Arg::new(),
                    rhs: Nested::Values(section.clone()),
                    out: out.clone(),
                });
            }
        } else {
            section.push(parse_value(value).map_err(ParseExpressionError::Value)?);
        }
    }

    todo!()
}



#[derive(Debug, Error)]
enum ParseValueError {
    #[error("Generic failure")]
    Generic
}

pub fn parse_value(chars: &[char]) -> Result<Value, ParseValueError> {
    todo!()
}

#[derive(Debug, Error)]
enum ParseVariableError {
    #[error("Generic failure")]
    Generic,
    #[error("An index was opened with `[` but not terminated with `]`.")]
    NonTerminatedIndex,
    #[error("Failed to parse index: {0}")]
    Offset(ParseOffsetError),
}

pub fn parse_variable(chars: &[char]) -> Result<Variable, ParseVariableError> {
    let mut identifier = Identifier::new();

    let addressing = match chars.get(0).ok_or(ParseVariableError::Generic)? {
        '&' => Addressing::Reference,
        '*' => Addressing::Dereference,
        c => {
            identifier.push(*c);
            Addressing::Direct
        }
    };

    let mut index = None;
    let mut cast = None;

    #[cfg(debug_assertions)]
    let mut limit = 0..LOOP_LIMIT;

    let mut i = 0;
    while let Some(c) = chars.get(i) {
        debug_assert!(limit.next().is_some());
        i += 1;
        match c {
            '[' => {
                let n = chars
                    .iter()
                    .enumerate()
                    .skip(i)
                    .find_map(|(j, c)| (*c == ']').then_some(j))
                    .ok_or(ParseVariableError::NonTerminatedIndex)?;
                index = Some(parse_offset(&chars[i..n]).map_err(ParseVariableError::Offset)?);
                cast = match chars.get(n + 1) {
                    Some(':') => Some(parse_type(&chars[n + 2..])?),
                    None => None,
                    _ => return Err(()),
                };
                break;
            }
            ':' => {
                cast = Some(parse_type(&chars[i..]).map_err(ParseVariableError::Type)?);
                break;
            }
            _ => {
                identifier.push(c);
            }
        }
    }

    Ok(Variable {
        addressing,
        identifier,
        index,
        cast,
    })
}


#[derive(Debug, Error)]
enum ParseOffsetError {
    #[error("Failed to parse integer.")]
    Integer,
    #[error("Failed tp parse variable: {0}")]
    Variable(Box<ParseVariableError>)
}

pub fn parse_offset(chars: &[char]) -> Result<Offset, ParseOffsetError> {
    #[cfg(debug_assertions)]
    let mut limit = 0..LOOP_LIMIT;
    const BASE: u32 = 10;
    let first = chars.get(0).ok_or(())?;
    if let Some(mut digit) = first.to_digit(BASE) {
        let mut iter = chars[1..].iter();
        loop {
            debug_assert!(limit.next().is_some());
            let next_opt = iter.next();
            let Some(next) = next_opt else { break };
            let next_digit = next.to_digit(BASE).ok_or(ParseOffsetError::Integer)?;
            digit *= BASE;
            digit += next_digit;
        }
        Ok(Offset::Integer(digit as u64))
    } else {
        // TODO This uses recursion, fix this so it doesn't use recursion.
        Ok(Offset::Variable(parse_variable(chars).map_err(|err|ParseOffsetError::Variable(Box::new(err)))?))
    }
}

fn div_rem(rhs: usize, lhs: usize) -> Option<usize> {
    match rhs % lhs {
        0 => Some(rhs / lhs),
        _ => None,
    }
}
/// [`slice::split_at`] has [`slice::split_at_checked`] this is the equivalent
/// for [`Vec::split_off`].
fn split_off_checked(vec: &mut Vec<T>, index: usize) -> Option<Vec<T>> {
    if index > vec.len() {
        None
    } else {
        Some(vec.split_at(index))
    }
}

#[derive(Debug, Error)]
#[error("Failed to parse type.")]
struct ParseTypeError;

pub fn parse_type(chars: &[char]) -> Result<Type,ParseTypeError> {
    let 
    loop {
        match chars {
            ['i','8'] => Ok(Type::Integer(IntegerType::I8)),
            ['i','1','6'] => Ok(Type::Integer(IntegerType::I16)),
            ['i','3','2'] => Ok(Type::Integer(IntegerType::I32)),
            ['i','6','4'] => Ok(Type::Integer(IntegerType::I64)),
            ['u','8'] => Ok(Type::Integer(IntegerType::I8)),
            ['u','1','6'] => Ok(Type::Integer(IntegerType::U16)),
            ['u','3','2'] => Ok(Type::Integer(IntegerType::U32)),
            ['u','6','4'] => Ok(Type::Integer(IntegerType::U64)),
            ['b','o','o','l'] => Ok(Type::Boolean),
            ['&', tail @ ..] => 
            _ => Err(ParseTypeError)
        }
    }
    
}