use std::boxed::Box;
use std::collections::HashMap;
use std::error::Error;
use std::io::Write;
use std::process;

use crate::data::errors;

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
    fn execute(&self, capsule: ActionCapsule, writer: &mut dyn Write)
        -> Result<(), Box<dyn Error>>;

    fn footer(&self) -> String {
        String::from("\n")
    }
}

pub struct Echo;
impl Action for Echo {
    fn execute(
        &self,
        capsule: ActionCapsule,
        writer: &mut dyn Write,
    ) -> Result<(), Box<dyn Error>> {
        write!(
            writer,
            "{}",
            capsule
                .command_line
                .replacen(capsule.tags.get(0).unwrap(), "", 1)
                .trim()
        )?;
        Ok(())
    }
}

pub struct Exit;
impl Action for Exit {
    fn execute(
        &self,
        _capsule: ActionCapsule,
        writer: &mut dyn Write,
    ) -> Result<(), Box<dyn Error>> {
        writeln!(writer, "Come visit again!\n")?;
        process::exit(0);
    }
}

fn action_mapping() -> HashMap<String, Box<dyn Action>> {
    let mut mapping: HashMap<String, Box<dyn Action>> = HashMap::new();

    mapping.insert(String::from("echo"), Box::new(Echo {}));
    mapping.insert(String::from("exit"), Box::new(Exit {}));

    mapping
}

pub fn execute_from_capsule(
    capsule: ActionCapsule,
    writer: &mut dyn Write,
) -> Result<(), Box<dyn Error>> {
    let searchby = capsule.tags.get(0).ok_or(errors::ActionEmptyError {});
    let mut the_map = action_mapping();
    let the_action = the_map
        .get_mut(searchby?)
        .ok_or(errors::ActionUnknownError)?
        .as_mut();

    the_action.execute(capsule, writer)?;
    writeln!(writer, "{}", the_action.footer())?;
    Ok(())
}
