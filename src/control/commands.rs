use std::boxed::Box;
use std::collections::HashMap;
use std::io::Write;
use std::process;

use anyhow::{Ok, Result};

pub struct CommandCapsule {
    pub command_line: String,
    pub tags: Vec<String>,
}
impl CommandCapsule {
    pub fn new(command_line: String, tags: Vec<String>) -> Self {
        Self { command_line, tags }
    }
}

pub trait Command {
    fn execute(&mut self, capsule: CommandCapsule, writer: &mut dyn Write) -> Result<()>;
}

pub struct Echo;
impl Command for Echo {
    fn execute(&mut self, capsule: CommandCapsule, writer: &mut dyn Write) -> Result<()> {
        write!(
            writer,
            "{}\n",
            capsule
                .command_line
                .replacen(capsule.tags.get(0).unwrap(), "", 1)
                .trim()
        )?;
        Ok(())
    }
}

pub struct Exit;
impl Command for Exit {
    fn execute(&mut self, _capsule: CommandCapsule, writer: &mut dyn Write) -> Result<()> {
        write!(writer, "Come visit again!\n")?;
        process::exit(0);
    }
}

fn command_mapping() -> HashMap<String, Box<dyn Command>> {
    let mut mapping: HashMap<String, Box<dyn Command>> = HashMap::new();

    mapping.insert(String::from("echo"), Box::new(Echo {}));
    mapping.insert(String::from("exit"), Box::new(Exit {}));

    mapping
}

pub fn execute_from_capsule(capsule: CommandCapsule, writer: &mut dyn Write) -> Result<()> {
    let searchby = capsule.tags.get(0).unwrap().clone();
    let mut the_map = command_mapping();
    let the_command = the_map.get_mut(&searchby).unwrap().as_mut();

    the_command.execute(capsule, writer)?;
    Ok(())
}
