use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Modifier {
    Question,
    Uncertainty,
    Exclamation,
    Disbelief,
    Certainty,
    Excitement,
}

pub fn modifier_map() -> HashMap<String, Vec<Modifier>> {
    let mut map = HashMap::new();
    map.insert("?".to_string(), vec![Modifier::Question]);
    map.insert("!".to_string(), vec![Modifier::Exclamation]);
    map.insert(
        "?!".to_string(),
        vec![
            Modifier::Disbelief,
            Modifier::Question,
            Modifier::Exclamation,
        ],
    );
    map.insert(".".to_string(), vec![Modifier::Certainty]);
    map.insert(
        "?..".to_string(),
        vec![Modifier::Uncertainty, Modifier::Question],
    );
    map.insert(
        "!!!".to_string(),
        vec![Modifier::Excitement, Modifier::Exclamation],
    );
    map.insert(
        "???".to_string(),
        vec![Modifier::Question, Modifier::Excitement],
    );
    map.insert("...".to_string(), vec![Modifier::Uncertainty]);
    map.insert("..".to_string(), vec![Modifier::Uncertainty]);
    map
}
