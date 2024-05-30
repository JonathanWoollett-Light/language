use crate::frontend::*;
use crate::ast::*;
use crate::LOOP_LIMIT;
use thiserror::Error;
use std::cell::OnceCell;

#[derive(Debug, Error)]
enum ParseAstError {
    #[error("Indentation must be a multiple of 4 spaces.")]
    IndentationSpacing,
    #[error("Indentation can only be 1 greater than a parent.")]
    IndetationDepth
}

pub unsafe fn parse_ast(chars: &[char]) -> Result<Option<NonNull<AstNode>>, ParseError> {
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

    'outer: for _ in 0..LOOP_LIMIT {
        // Handle indentation.
        let mut spaces = 0;
        for _ in 0..LOOP_LIMIT {
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
            for _ in 0..LOOP_LIMIT {
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
        let line_characters = for _ in 0..LOOP_LIMIT {
            match chars.split_at(i + line_length) {
                (x, ['\n'] | []) => break x,
                _ => {},
            }
            line_length += 1;
        };
        i += line_length;

        let line = parse_line(line_characters, &mut definitions);
        let node = nonnull(AstNode {
            line,
            preceding: None,
            child: None,
            next: None,
        });

        // Insert into the AST.
        let tabs = div_rem(spaces, 4).unwrap_or(ParseAstError::IndentationSpacing)?;
        let after = split_off_checked(&mut parent_stack, tabs).unwrap_or(ParseAstError::IndetationDepth)?;
        if let Some(previous) = after.first_mut() {
            assert_eq!(previous.as_mut().next, None);
            previous.as_mut().next = Some(node);
            node.as_mut().preceding = Some(Preceding::Previous(*previous));
        }
        else if let Some(parent) = parent_stack.last_mut() {
            assert_eq!(parent.as_mut().child, None);
            parent.as_mut().child = Some(node);
            node.as_mut().preceding = Some(Preceding::Parent(*previous));
        }
        else {
            first.set(node).unwrap();
        }
        parent_stack.push(node);
    }
    Ok(first.take())
}

#[derive(Debug, Error)]
enum ParseLineError {
    #[error("Failed to parse statement: {0}")]
    Statement(ParseStatementError),
    Instruction(crate::main::ParseInstructionError)
}

pub fn parse_line(chars: &[char], definitions: &mut HashSet<String>) -> Result<Line,ParseLineError> {
    let line = if let Some((lhs,rhs)) = chars.split_at_checked(3) && lhs == "asm" {
        Line::Assembly(crate::parse_instruction(rhs).map_err(ParseLineError::Instruction)?)
    }
    else {
        Line::Source(parse_statement(chars, definitions).map_err(ParseLineError::Statement)?)
    };
}

#[derive(Debug, Error)]
enum ParseStatementError {
    #[error("Empty statement")]
    Empty
}

pub fn parse_statement(chars: &[char], definitions: &mut HashSet<String>) -> Result<Statement, ParseStatementError> {
    let mut i = chars.len().checked_sub(1).unwrap_or(ParseStatementError::Empty)?;
    for _ in 0..LOOP_LIMIT {
        
    }
}


fn div_rem(rhs: usize, lhs: usize) -> Option<usize> {
    match rhs % lhs {
        0 => Some(rhs / lhs),
        _ => None
    }
}
/// [`slice::split_at`] has [`slice::split_at_checked`] this is the equivalent
/// for [`Vec::split_off`].
fn split_off_checked(vec: &mut Vec<T>, index: usize) -> Option<Vec<T>> {
    if index > vec.len() {
        None
    }
    else {
        Some(vec.split_at(index))
    }
}