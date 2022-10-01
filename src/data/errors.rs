use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum ErrorType {
    CLIUsage(CliError),
    Parsing(ParsingError),
    System(String),
}
impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CLIUsage(x) => write!(f, "{}", x),
            Self::Parsing(x) => write!(f, "{}", x),
            Self::System(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug)]
pub enum CliError {
    ActionUnknown,
    ActionEmpty,
}
impl fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            Self::ActionUnknown => "No clue how to do that!",
            Self::ActionEmpty => "",
        };
        write!(f, "{}", message)
    }
}

#[derive(Debug)]
pub enum ParsingError {
    RootActionUnknown(String),
    DoesNotTakeDirectObject(String, String),
    DoesNotAcceptInfix(String, String),
}
impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            Self::RootActionUnknown(r) => {
                format!("Uhhh... What does '{}'ing actually accomplish here?", r)
            }
            Self::DoesNotTakeDirectObject(r, dobj) => {
                format!("Hmmm... How would you '{}' a '{}'?", r, dobj)
            }
            Self::DoesNotAcceptInfix(r, inf) => {
                format!("Hmmm... How would you '{}' '{}' something?", r, inf)
            }
        };
        write!(f, "{}", message)
    }
}
impl From<std::io::Error> for ErrorType {
    fn from(e: std::io::Error) -> Self {
        ErrorType::System(e.to_string())
    }
}

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

#[derive(Debug)]
pub struct RootActionUnknownError {
    root: String,
}
impl Error for RootActionUnknownError {}
impl fmt::Display for RootActionUnknownError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Uhhh... What does '{}'ing actually accomplish here?",
            self.root
        )
    }
}
impl RootActionUnknownError {
    pub fn new(root: String) -> Self {
        Self { root }
    }
}

#[derive(Debug)]
pub struct DoesNotTakeDirectObjectError {
    root: String,
    direct_object: String,
}
impl Error for DoesNotTakeDirectObjectError {}
impl fmt::Display for DoesNotTakeDirectObjectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Hmmm... How would you '{}' a '{}'?",
            self.root, self.direct_object
        )
    }
}
impl DoesNotTakeDirectObjectError {
    pub fn new(root: String, direct_object: String) -> Self {
        Self {
            root,
            direct_object,
        }
    }
}

#[derive(Debug)]
pub struct DoesNotAcceptInfixError {
    root: String,
    infix: String,
}
impl Error for DoesNotAcceptInfixError {}
impl fmt::Display for DoesNotAcceptInfixError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Hmmm... How would you '{}' '{}' something?",
            self.root, self.infix
        )
    }
}
impl DoesNotAcceptInfixError {
    pub fn new(root: String, infix: String) -> Self {
        Self { root, infix }
    }
}
