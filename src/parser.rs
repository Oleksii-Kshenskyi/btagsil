use crate::knowledge::*;
use crate::pipeline::*;

/// Parser's main task is to produce a pipeline stage
/// that converts User Input { String } => Parsed Command { Command }
///
/// We don't assign any meanings or intent to the user input yet, as that
/// is the responsibility of the next stage. This is just parsing the unstructured
/// user input into a structured and easily processable format.

// TODO: need to start thinking about integrating word lists in text file / JSON form
//       ASAP instead of just hardcoding them in the code.
#[derive(Debug, Clone)]
pub struct Command {
    pub verb: String,
    pub objects: Vec<String>,
    pub indirect_objects: Vec<String>,
    pub modifiers: Vec<Modifier>,
}

impl Command {
    pub fn new(input: &str) -> Self {
        Self {
            verb: Self::back_punct(&input),
            objects: Vec::new(),
            indirect_objects: Vec::new(),
            modifiers: Self::extract_modifiers(input),
        }
    }

    fn extract_modifiers(input: &str) -> Vec<Modifier> {
        let mut modifiers = vec![];

        let modmap = modifier_map();
        let punct = Self::back_punct(input);
        if modmap.contains_key(&punct) {
            modifiers.extend(modmap[&punct].clone());
        }

        modifiers
    }
    fn back_punct(input: &str) -> String {
        (input
            .chars()
            .rev()
            .take_while(|c| c.is_ascii_punctuation())
            .collect::<String>())
        .chars()
        .rev()
        .collect::<String>()
    }
}

// This could later be a common interface for all the pipeline stages,
// but for now this is left as just a function that returns a stage.
pub fn parser_stage() -> Box<dyn Stage> {
    stage("Parser", |input: String| -> Command {
        Command::new(&input)
    })
}

// TODO: think about implementing tests for testing the basic corner cases of the parser
