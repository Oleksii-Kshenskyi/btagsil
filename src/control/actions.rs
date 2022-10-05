use std::boxed::Box;
use std::collections::HashMap;
use std::io::Write;
use std::process;

use crate::comprehension::lexer::Lexer;
use crate::comprehension::parser::{ActionConfig, Parser};
use crate::data::errors::{CliError, ErrorType, ParsingError};
use crate::world::World;

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
    fn execute(
        &self,
        config: ActionConfig,
        writer: &mut dyn Write,
        world: &mut World,
    ) -> Result<(), ErrorType>;

    fn footer(&self) -> String {
        String::from("\n")
    }
}

pub struct Echo;
impl Action for Echo {
    fn execute(
        &self,
        config: ActionConfig,
        writer: &mut dyn Write,
        _world: &mut World,
    ) -> Result<(), ErrorType> {
        write!(writer, "{}", config.direct_object.join(" "))?;
        Ok(())
    }
}

pub struct Exit;
impl Action for Exit {
    fn execute(
        &self,
        _config: ActionConfig,
        writer: &mut dyn Write,
        _world: &mut World,
    ) -> Result<(), ErrorType> {
        writeln!(writer, "Come visit again!\n")?;
        process::exit(0);
    }
}

pub struct Where;
impl Action for Where {
    fn execute(
        &self,
        config: ActionConfig,
        writer: &mut dyn Write,
        world: &mut World,
    ) -> Result<(), ErrorType> {
        let do_str = config.direct_object.join(" ");
        let player_loc = world.player.location();
        if do_str == "am i" {
            write!(
                writer,
                "{}",
                world
                    .locations
                    .get(&player_loc.as_str())
                    .expect("Where Action, execute(): player's current location is corrupted.")
                    .description()
            )?;
            return Ok(());
        } else {
            return Err(ErrorType::Parsing(ParsingError::DirectObjectIsInvalid(
                "where".to_owned(),
                config.direct_object.join(" "),
                Some("where am i".to_owned()),
            )));
        }
    }
}

pub struct Who;
impl Action for Who {
    fn execute(
        &self,
        config: ActionConfig,
        writer: &mut dyn Write,
        world: &mut World,
    ) -> Result<(), ErrorType> {
        let do_str = config.direct_object.join(" ");
        if do_str == "am i" {
            write!(writer, "You are {}.", world.player.name())?;
            return Ok(());
        } else {
            return Err(ErrorType::Parsing(ParsingError::DirectObjectIsInvalid(
                "who".to_owned(),
                config.direct_object.join(" "),
                Some("who am i".to_owned()),
            )));
        }
    }
}

pub struct Name;
impl Action for Name {
    fn execute(
        &self,
        config: ActionConfig,
        writer: &mut dyn Write,
        world: &mut World,
    ) -> Result<(), ErrorType> {
        let do_vec = config.direct_object;
        if do_vec.get(0).unwrap() != "myself" {
            return Err(ErrorType::Parsing(ParsingError::DirectObjectIsInvalid(
                "name".to_owned(),
                do_vec.join(" "),
                Some("name myself <some name>".to_owned()),
            )));
        } else {
            if do_vec.len() < 2 {
                return Err(ErrorType::System(
                    "What name do you want for yourself?".to_owned(),
                ));
            } else {
                let name = world.player.name();
                if name != "nameless" {
                    return Err(ErrorType::System(format!(
                        "You already have a name. You are {}.",
                        name
                    )));
                } else {
                    let new_name = &do_vec[1..].join(" ");
                    write!(writer, "From now on, you are {}.", &new_name)?;
                    world.player.new_name(new_name.to_string())?;
                    return Ok(());
                }
            }
        }
    }
}

fn action_mapping() -> HashMap<String, Box<dyn Action>> {
    let mut mapping: HashMap<String, Box<dyn Action>> = HashMap::new();

    mapping.insert("echo".to_owned(), Box::new(Echo {}));
    mapping.insert("exit".to_owned(), Box::new(Exit {}));
    mapping.insert("where".to_owned(), Box::new(Where {}));
    mapping.insert("who".to_owned(), Box::new(Who {}));
    mapping.insert("name".to_owned(), Box::new(Name {}));

    mapping
}

pub fn execute_from_capsule(
    capsule: ActionCapsule,
    writer: &mut dyn Write,
    world: &mut World,
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

    the_action.execute(parsed_config, writer, world)?;
    writeln!(writer, "{}", the_action.footer())?;
    Ok(())
}
