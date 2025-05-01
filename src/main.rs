pub mod errors;
pub mod pipeline;

use crate::pipeline::*;
use anyhow::Result;

// First stage: takes 13, adds 10
// Second stage: takes result, creates vector of 10 strings of S1
// Third stage: takes vector of strings, joins them by space
fn main() -> Result<()> {
    let starter = 13;

    let mut pipeline = Pipeline::new();
    pipeline.push_stage(stage("+10".to_owned(), |i: i32| i + 10))?;
    pipeline.push_stage(stage("->vec".to_owned(), |i: i32| {
        [i; 10].iter().map(|ie| ie.to_string()).collect::<Vec<_>>()
    }))?;
    pipeline.push_stage(stage("join vec".to_owned(), |v: Vec<String>| v.join(" ")))?;

    let got = *pipeline
        .run(anyr(starter))?
        .value
        .downcast::<String>()
        .expect("Final main downcast failed!!");

    println!("GOT!! => `{}`", got);

    println!("{}", pipeline.visualize());
    Ok(())
}
