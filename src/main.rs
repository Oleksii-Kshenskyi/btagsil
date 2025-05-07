pub mod errors;
pub mod knowledge;
pub mod parser;
pub mod pipeline;

use crate::parser::{Command, parser_stage};
use crate::pipeline::*;
use anyhow::Result;

fn main() -> Result<()> {
    let starter = "?!   f   oko    j.    nn ?..".to_owned();

    let mut pipeline = Pipeline::new();
    pipeline.push_stage(parser_stage())?;

    let got = *pipeline
        .run(anyr(starter))?
        .value
        .downcast::<Command>()
        .expect("Final main downcast failed!!");

    println!("{}", pipeline.visualize());
    dbg!(got);
    Ok(())
}
