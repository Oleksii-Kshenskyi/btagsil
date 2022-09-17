use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct ActionUnknownError;
impl Error for ActionUnknownError {}
impl fmt::Display for ActionUnknownError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "No clue how to do that!")
    }
}

#[derive(Debug)]
pub struct ActionEmptyError;
impl Error for ActionEmptyError {}
impl fmt::Display for ActionEmptyError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}
