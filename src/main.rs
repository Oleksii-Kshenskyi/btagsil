pub mod common;
pub mod errors;
pub mod knowledge;
pub mod parser;
pub mod pipeline;
pub mod resolver;
pub mod world;

use std::rc::Rc;

use crate::common::anyr;
use crate::parser::{Command, parser_stage};
use crate::pipeline::*;
use anyhow::Result;
use resolver::package_command_stage;

fn main() -> Result<()> {
    let starter =
        "!maybe take fucking home certainly dog certainly fucking fucking up down word?".to_owned();

    let mut pipeline = Pipeline::new();
    pipeline.push_stage(parser_stage())?;
    // [ ]: Actually implement creating the World
    //      I need several (at least two) entity types for that.
    //      I can create the "init_world()" function that both creates and populates world
    pipeline.push_stage(package_command_stage(Rc::new(world)));

    let got = *pipeline
        .run(anyr(starter))?
        .value
        .downcast::<Command>()
        .expect("Final main downcast failed!!");

    println!("{}", pipeline.visualize());
    dbg!(got);
    Ok(())
}
