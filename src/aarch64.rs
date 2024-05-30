use thiserror::Error;

#[derive(Debug, Clone)]
pub struct Instruction {

}

#[derive(Debug, Error)]
pub enum ParseInstructionError {

}

pub fn parse_instruction(_chars: &[char]) -> Result<Instruction, ParseInstructionError> {
    todo!()
}