use crate::knowledge::*;
use crate::pipeline::*;

use std::collections::HashSet;

/// Parser's main task is to produce a pipeline stage
/// that converts User Input { String } => Parsed Command { Command }
///
/// We don't assign any meanings or intent to the user input yet, as that
/// is the responsibility of the next stage. This is just parsing the unstructured
/// user input into a structured and easily processable format.

struct Punct {
    subject: String,
}

impl Punct {
    pub fn new(subject: &str) -> Self {
        Self {
            subject: subject.to_owned(),
        }
    }

    pub fn split_three(&self) -> (Option<String>, Vec<String>, Option<String>) {
        let trimmed = self.subject.trim();
        let front = Self::front(&trimmed);
        let back = Self::back(&trimmed);

        let no_prefix = match &front {
            Some(prefix) => &trimmed[prefix.len()..],
            None => &trimmed,
        };

        let middle = match &back {
            Some(suffix) => &no_prefix[..no_prefix.len() - suffix.len()],
            None => no_prefix,
        };
        let words = middle
            .split_whitespace()
            .map(str::to_owned)
            .collect::<Vec<_>>();

        (front, words, back)
    }

    pub fn front(subject: &str) -> Option<String> {
        let punct = subject
            .chars()
            .take_while(|c| c.is_ascii_punctuation())
            .collect::<String>();

        (!punct.is_empty()).then_some(punct)
    }

    pub fn back(subject: &str) -> Option<String> {
        let backpunct = (subject
            .chars()
            .rev()
            .take_while(|c| c.is_ascii_punctuation())
            .collect::<String>())
        .chars()
        .rev()
        .collect::<String>();

        (!backpunct.is_empty()).then_some(backpunct)
    }
}

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
        // TODO: first stage done: split into (punctuation, words, punctuation).
        //       - second: (punct, words, punct) => (words, Vec<Modifier>)
        //                 -- extract modifiers not just from punct, but also word modifiers
        //                 -- like "maybe", "certainly", "unsure"
        //                 -- resulting words vector needs to drop these modifier words
        //                 -- ["perhaps", "kill", "boss"] => ["kill", "boss"]
        //      - third: words need to be analyzed semantically: verbs, objects need to be
        //               extracted (for now store hardcoded verb/object maps in knowledge.rs)
        let (punct_before, words, punct_after) = Punct::new(input).split_three();
        let (words, modifiers) = Self::extract_modifiers(punct_before, words, punct_after);
        // TODO: don't forget to remove debug once parser stage is done!
        dbg!((&words, &modifiers));
        Self {
            verb: input.to_owned(),
            objects: Vec::new(),
            indirect_objects: Vec::new(),
            modifiers,
        }
    }

    fn extract_modifiers(
        punct_start: Option<String>,
        words: Vec<String>,
        punct_end: Option<String>,
    ) -> (Vec<String>, Vec<Modifier>) {
        let mut modifiers = vec![];
        let modmap = ModifierMap::new();

        match punct_start {
            Some(punct) => modifiers.extend(modmap.punct_modifier(&punct)),
            None => (),
        }

        let mut nomod_words = vec![];
        for word in words {
            let word_mods = modmap.word_modifier(&word);
            if word_mods.is_empty() {
                nomod_words.push(word);
            }
            modifiers.extend(word_mods);
        }

        match punct_end {
            Some(punct) => modifiers.extend(modmap.punct_modifier(&punct)),
            None => (),
        }

        (
            nomod_words,
            modifiers
                .into_iter()
                .collect::<HashSet<_>>()
                .into_iter()
                .collect(),
        )
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
