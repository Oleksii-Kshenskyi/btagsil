use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Modifier {
    Question,
    Uncertainty,
    Exclamation,
    Disbelief,
    Certainty,
    Excitement,
    Composure,
    Dumbfuckery,
}

pub struct ModifierMap {
    punct_modmap: HashMap<String, Vec<Modifier>>,
    word_modmap: HashMap<String, Vec<Modifier>>,
}

impl ModifierMap {
    pub fn new() -> Self {
        Self {
            punct_modmap: Self::punct_modmap(),
            word_modmap: Self::word_modmap(),
        }
    }
    pub fn punct_modifier(&self, punct: &str) -> Vec<Modifier> {
        self.punct_modmap.get(punct).unwrap_or(&vec![]).to_vec()
    }
    pub fn word_modifier(&self, word: &str) -> Vec<Modifier> {
        self.word_modmap.get(word).unwrap_or(&vec![]).to_vec()
    }

    fn word_modmap() -> HashMap<String, Vec<Modifier>> {
        let mut map = HashMap::new();
        map.insert("perhaps".to_owned(), vec![Modifier::Uncertainty]);
        map.insert("maybe".to_owned(), vec![Modifier::Uncertainty]);
        map.insert("certainly".to_owned(), vec![Modifier::Certainty]);
        map.insert("wow".to_owned(), vec![Modifier::Disbelief]);
        map.insert("calmly".to_owned(), vec![Modifier::Composure]);
        map.insert("fucking".to_owned(), vec![Modifier::Excitement]);
        map.insert("discombobulate".to_owned(), vec![Modifier::Dumbfuckery]);
        map.insert("kek".to_owned(), vec![Modifier::Dumbfuckery]);
        map.insert("kekw".to_owned(), vec![Modifier::Dumbfuckery]);

        map.insert("wee".to_owned(), vec![Modifier::Excitement]);
        map.insert("hurrah".to_owned(), vec![Modifier::Excitement]);
        map.insert("boo-yah".to_owned(), vec![Modifier::Excitement]);
        map.insert("booyah".to_owned(), vec![Modifier::Excitement]);

        map
    }
    fn punct_modmap() -> HashMap<String, Vec<Modifier>> {
        let mut map = HashMap::new();
        map.insert("?".to_owned(), vec![Modifier::Question]);
        map.insert("!".to_owned(), vec![Modifier::Exclamation]);
        map.insert(
            "?!".to_owned(),
            vec![
                Modifier::Disbelief,
                Modifier::Question,
                Modifier::Exclamation,
            ],
        );
        map.insert(".".to_owned(), vec![Modifier::Certainty]);
        map.insert(
            "?..".to_owned(),
            vec![Modifier::Uncertainty, Modifier::Question],
        );
        map.insert(
            "!!!".to_owned(),
            vec![Modifier::Excitement, Modifier::Exclamation],
        );
        map.insert(
            "???".to_owned(),
            vec![Modifier::Question, Modifier::Excitement],
        );
        map.insert("...".to_owned(), vec![Modifier::Uncertainty]);
        map.insert("..".to_owned(), vec![Modifier::Uncertainty]);
        map
    }
}
