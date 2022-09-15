use anyhow::Result;
use std::io::{stdout, Write};
use text_io::read;

fn main() -> Result<()> {
    loop {
        print!("=>> ");
        stdout().flush()?;

        let user_input: String = read!("{}\n");
        println!("The command was: [{}].", user_input);
    }
}
