use anyhow::Result;
use std::io::{stdout, Write};
use text_io::read;

fn main() -> Result<()> {
    loop {
        print!("=>> ");
        stdout().flush()?;

        let user_input: String = read!("{}\n");
        let tags = user_input
            .split_whitespace()
            .map(|s| s.to_owned())
            .collect::<Vec<_>>();
        println!("The command was: [{}].", user_input);
        println!("The tags are: {:?}", tags);
    }
}
