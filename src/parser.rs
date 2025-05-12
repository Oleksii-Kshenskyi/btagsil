/// Parser's main task is to produce a pipeline stage
/// that converts User Input { String } => Parsed Command { Command }
///
/// We don't assign any meanings or intent to the user input yet, as that
/// is the responsibility of the next stage. This is just parsing the unstructured
/// user input into a structured and easily processable format.
use crate::knowledge::*;
use crate::pipeline::*;

use std::collections::HashSet;

struct Punct {
    subject: String,
}

impl Punct {
    pub fn new(subject: &str) -> Self {
        Self {
            subject: subject.to_owned(),
        }
    }

    /// Weird method that splits user input into
    /// (maybe punctuation at start, vec of words, maybe punctuation at the end)
    pub fn split_three(&self) -> (Option<String>, Vec<String>, Option<String>) {
        let trimmed = self.subject.trim();
        let front = Self::front(trimmed);
        let back = Self::back(trimmed);

        let no_prefix = match &front {
            Some(prefix) => &trimmed[prefix.len()..],
            None => trimmed,
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
    pub verb: Verb,
    pub objects: Vec<String>,
    pub locations: Vec<String>,
    pub modifiers: Vec<Modifier>,
}

impl Command {
    pub fn new(input: &str) -> Self {
        let (punct_before, words, punct_after) = Punct::new(input).split_three();
        let (words, modifiers) = Self::extract_modifiers(punct_before, words, punct_after);
        let (verb, objects, locations) = Self::lexical_analysis(&words);

        Self {
            verb,
            objects,
            locations,
            modifiers,
        }
    }

    /// extracts verbs, list of objects and list of locations from a vec of words
    fn lexical_analysis(words: &[String]) -> (Verb, Vec<String>, Vec<String>) {
        if words.is_empty() {
            return (Verb::IDLE, vec![], vec![]);
        }

        let mut current_words = words.to_owned();
        let lexical_map = LexicalMap::new();

        let verb = lexical_map.verb(&words[0]);
        if verb != Verb::IDLE {
            current_words.remove(0);
        }

        let mut objects = vec![];
        let mut locations = vec![];
        for word in current_words {
            if lexical_map.is_object(&word) {
                objects.push(word.clone());
            }
            if lexical_map.is_location(&word) {
                locations.push(word);
            }
        }

        (verb, objects, locations)
    }
    fn extract_modifiers(
        punct_start: Option<String>,
        words: Vec<String>,
        punct_end: Option<String>,
    ) -> (Vec<String>, Vec<Modifier>) {
        let mut modifiers = vec![];
        let modmap = ModifierMap::new();

        if let Some(punct) = punct_start {
            modifiers.extend(modmap.punct_modifier(&punct));
        }

        let mut nomod_words = vec![];
        for word in words {
            let word_mods = modmap.word_modifier(&word);
            if word_mods.is_empty() {
                nomod_words.push(word);
            }
            modifiers.extend(word_mods);
        }

        if let Some(punct) = punct_end {
            modifiers.extend(modmap.punct_modifier(&punct));
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
