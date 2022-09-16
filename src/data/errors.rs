use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct CommandUnknownError;
impl Error for CommandUnknownError {}
impl fmt::Display for CommandUnknownError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "No clue how to do that!")
    }
}

#[derive(Debug)]
pub struct CommandEmptyError;
impl Error for CommandEmptyError {}
impl fmt::Display for CommandEmptyError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}
