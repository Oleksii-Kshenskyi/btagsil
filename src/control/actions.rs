use std::boxed::Box;
use std::collections::HashMap;
use std::io::Write;
use std::process;

use crate::comprehension::lexer::Lexer;
use crate::comprehension::parser::{ActionConfig, Parser};
use crate::data::errors::{CliError, ErrorType};

pub struct ActionCapsule {
    pub command_line: String,
    pub tags: Vec<String>,
}
impl ActionCapsule {
    pub fn new(command_line: String, tags: Vec<String>) -> Self {
        Self { command_line, tags }
    }
}

pub trait Action {
    fn execute(&self, capsule: ActionConfig, writer: &mut dyn Write) -> Result<(), ErrorType>;

    fn footer(&self) -> String {
        String::from("\n")
    }
}

pub struct Echo;
impl Action for Echo {
    fn execute(&self, config: ActionConfig, writer: &mut dyn Write) -> Result<(), ErrorType> {
        write!(writer, "{}", config.direct_object.join(" "))?;
        Ok(())
    }
}

pub struct Exit;
impl Action for Exit {
    fn execute(&self, _capsule: ActionConfig, writer: &mut dyn Write) -> Result<(), ErrorType> {
        writeln!(writer, "Come visit again!\n")?;
        process::exit(0);
    }
}

fn action_mapping() -> HashMap<String, Box<dyn Action>> {
    let mut mapping: HashMap<String, Box<dyn Action>> = HashMap::new();

    mapping.insert("echo".to_owned(), Box::new(Echo {}));
    mapping.insert("exit".to_owned(), Box::new(Exit {}));

    mapping
}

pub fn execute_from_capsule(
    capsule: ActionCapsule,
    writer: &mut dyn Write,
) -> Result<(), ErrorType> {
    capsule
        .tags
        .get(0)
        .ok_or(ErrorType::CLIUsage(CliError::ActionEmpty))?;

    let owned_tags = &capsule.tags.iter().map(String::as_str).collect::<Vec<_>>();
    let mut lexer = Lexer::new(owned_tags);
    let lexed = lexer.lex()?;

    let parsed_config = Parser::new().parse(lexed)?;

    let mut the_map = action_mapping();
    let the_action = the_map
        .get_mut(&parsed_config.root_action)
        .ok_or(ErrorType::CLIUsage(CliError::ActionUnknown))?
        .as_mut();

    the_action.execute(parsed_config, writer)?;
    writeln!(writer, "{}", the_action.footer())?;
    Ok(())
}
