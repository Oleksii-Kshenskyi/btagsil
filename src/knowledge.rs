use std::collections::HashMap;
use std::collections::HashSet;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Verb {
    IDLE,
    Go,
    MoveObject,
    Jump,
    Take,
    Throw,
    LookFor,
    Inspect,
}

pub struct ModifierMap {
    punct_modmap: HashMap<&'static str, Vec<Modifier>>,
    word_modmap: HashMap<&'static str, Vec<Modifier>>,
}
impl Default for ModifierMap {
    fn default() -> Self {
        Self::new()
    }
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

    fn word_modmap() -> HashMap<&'static str, Vec<Modifier>> {
        let mut map = HashMap::new();
        map.insert("perhaps", vec![Modifier::Uncertainty]);
        map.insert("maybe", vec![Modifier::Uncertainty]);
        map.insert("certainly", vec![Modifier::Certainty]);
        map.insert("wow", vec![Modifier::Disbelief]);
        map.insert("calmly", vec![Modifier::Composure]);
        map.insert("fucking", vec![Modifier::Excitement]);
        map.insert("discombobulate", vec![Modifier::Dumbfuckery]);
        map.insert("kek", vec![Modifier::Dumbfuckery]);
        map.insert("kekw", vec![Modifier::Dumbfuckery]);

        map.insert("wee", vec![Modifier::Excitement]);
        map.insert("hurrah", vec![Modifier::Excitement]);
        map.insert("boo-yah", vec![Modifier::Excitement]);
        map.insert("booyah", vec![Modifier::Excitement]);

        map
    }
    fn punct_modmap() -> HashMap<&'static str, Vec<Modifier>> {
        let mut map = HashMap::new();
        map.insert("?", vec![Modifier::Question]);
        map.insert("!", vec![Modifier::Exclamation]);
        map.insert(
            "?!",
            vec![
                Modifier::Disbelief,
                Modifier::Question,
                Modifier::Exclamation,
            ],
        );
        map.insert(".", vec![Modifier::Certainty]);
        map.insert("?..", vec![Modifier::Uncertainty, Modifier::Question]);
        map.insert("!!!", vec![Modifier::Excitement, Modifier::Exclamation]);
        map.insert("???", vec![Modifier::Question, Modifier::Excitement]);
        map.insert("...", vec![Modifier::Uncertainty]);
        map.insert("..", vec![Modifier::Uncertainty]);
        map
    }
}

pub struct LexicalMap {
    verbs: HashMap<&'static str, Verb>,
    objects: HashSet<&'static str>,
    locations: HashSet<&'static str>,
}
impl Default for LexicalMap {
    fn default() -> Self {
        Self::new()
    }
}

impl LexicalMap {
    pub fn new() -> Self {
        Self {
            verbs: Self::verb_map(),
            objects: Self::object_set(),
            locations: Self::location_set(),
        }
    }

    pub fn verb(&self, input: &str) -> Verb {
        self.verbs.get(input).unwrap_or(&Verb::IDLE).clone()
    }

    pub fn is_object(&self, input: &str) -> bool {
        self.objects.contains(input)
    }

    pub fn is_location(&self, input: &str) -> bool {
        self.locations.contains(input)
    }

    fn object_set() -> HashSet<&'static str> {
        vec![
            "me", "myself", "weapon", "sword", "bullet", "stone", "spell", "portal", "item",
            "thing", "idiot", "monster", "mob", "dog", "cat", "problem", "joke", "ladder",
        ]
        .into_iter()
        .collect()
    }
    fn location_set() -> HashSet<&'static str> {
        vec![
            "up",
            "down",
            "left",
            "right",
            "north",
            "south",
            "west",
            "east",
            "forward",
            "back",
            "backwards",
            "place",
            "plaza",
            "market",
            "glade",
            "grove",
            "city",
            "town",
            "forest",
            "shop",
            "store",
            "home",
            "house",
            "cave",
            "dungeon",
        ]
        .into_iter()
        .collect()
    }
    fn verb_map() -> HashMap<&'static str, Verb> {
        let mut map = HashMap::new();

        map.insert("jump", Verb::Jump);
        map.insert("leap", Verb::Jump);
        map.insert("go", Verb::Go);
        map.insert("run", Verb::Go);
        map.insert("ride", Verb::Go);
        map.insert("crawl", Verb::Go);
        map.insert("move", Verb::MoveObject);
        map.insert("relocate", Verb::MoveObject);
        map.insert("push", Verb::MoveObject);
        map.insert("transfer", Verb::MoveObject);
        map.insert("bring", Verb::MoveObject);
        map.insert("transfer", Verb::MoveObject);
        map.insert("carry", Verb::MoveObject);
        map.insert("take", Verb::Take);
        map.insert("acquire", Verb::Take);
        map.insert("pick", Verb::Take);
        map.insert("grab", Verb::Take);
        map.insert("throw", Verb::Throw);
        map.insert("yeet", Verb::Throw);
        map.insert("hurl", Verb::Throw);
        map.insert("toss", Verb::Throw);
        map.insert("fling", Verb::Throw);
        map.insert("search", Verb::LookFor);
        map.insert("find", Verb::LookFor);
        map.insert("seek", Verb::LookFor);
        map.insert("locate", Verb::LookFor);
        map.insert("discover", Verb::LookFor);
        map.insert("inspect", Verb::Inspect);
        map.insert("examine", Verb::Inspect);
        map.insert("check", Verb::Inspect);
        map.insert("investigate", Verb::Inspect);

        map
    }
}
