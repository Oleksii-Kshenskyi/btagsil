mod control;

use anyhow::Result;
use std::io::{stdout, Write};
use text_io::read;

use control::commands;

fn main() -> Result<()> {
    loop {
        print!("=>> ");
        stdout().flush()?;

        let user_input: String = read!("{}\n");
        let tags = user_input
            .split_whitespace()
            .map(|s| s.to_owned())
            .collect::<Vec<_>>();
        commands::execute_from_capsule(
            commands::CommandCapsule::new(user_input, tags),
            &mut stdout(),
        )?;
    }
}
