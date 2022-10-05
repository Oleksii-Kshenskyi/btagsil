mod comprehension;
mod control;
mod data;
mod world;

use std::error::Error;
use std::io::{stdout, Write};
use std::process;
use std::result::Result;
use text_io::read;

use control::actions;
use data::errors::{CliError, ErrorType};
use world::World;

fn try_main() -> Result<(), Box<dyn Error>> {
    let mut world = World::new();
    loop {
        print!("=>> ");
        stdout().flush()?;

        let writer = &mut stdout();
        let user_input: String = read!("{}\n");
        let tags = user_input
            .split_whitespace()
            .map(|s| s.to_owned())
            .collect::<Vec<_>>();

        match actions::execute_from_capsule(
            actions::ActionCapsule::new(user_input, tags),
            writer,
            &mut world,
        ) {
            Err(ErrorType::CLIUsage(CliError::ActionEmpty)) => (),
            Err(e) => writeln!(writer, "{}\n", e)?,
            Ok(()) => (),
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    if let Err(e) = try_main() {
        eprintln!("Oh no, WHOOPSIE: {}", e);
        process::exit(-1);
    }
    Ok(())
}
