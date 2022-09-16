mod control;
mod data;

use std::error::Error;
use std::io::{stdout, Write};
use std::process;
use std::result::Result;
use text_io::read;

use control::commands;
use data::errors;

fn try_main() -> Result<(), Box<dyn Error>> {
    loop {
        print!("=>> ");
        stdout().flush()?;

        let writer = &mut stdout();
        let user_input: String = read!("{}\n");
        let tags = user_input
            .split_whitespace()
            .map(|s| s.to_owned())
            .collect::<Vec<_>>();

        match commands::execute_from_capsule(
            commands::CommandCapsule::new(user_input, tags),
            writer,
        ) {
            Err(ref e) if e.is::<errors::CommandEmptyError>() => (),
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
