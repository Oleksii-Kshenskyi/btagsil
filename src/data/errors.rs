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
    DirectObjectNotProvided(String),
    DoesNotAcceptInfix(String, String),
    DirectObjectIsInvalid(String, String, Option<String>),
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
            Self::DirectObjectNotProvided(r) => {
                format!(
                    "You need to specify something to '{}'. Try '{} <something>'.",
                    r, r
                )
            }
            Self::DoesNotAcceptInfix(r, inf) => {
                format!("Hmmm... How would you '{}' '{}' something?", r, inf)
            }
            Self::DirectObjectIsInvalid(r, dobj, None) => {
                format!("Hmm... No, you can't {} a {} :(", r, dobj)
            }
            Self::DirectObjectIsInvalid(r, dobj, Some(suggestion)) => {
                format!(
                    "You can't {} a {}! Maybe you meant '{}'?",
                    r, dobj, suggestion
                )
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
