use std::collections::HashMap;

use crate::data::errors::ErrorType;

pub struct Entity {
    pub name: String,
}
pub struct Place {
    pub name: String,
    pub description: Vec<String>,
}

pub struct Player {
    entity: Option<Entity>,
    current_location: String,
}
impl Player {
    pub fn new() -> Self {
        Self {
            entity: None,
            current_location: "The Void".to_owned(),
        }
    }
    pub fn name(&self) -> String {
        match &self.entity {
            None => "nameless".to_owned(),
            Some(e) => e.name.clone(),
        }
    }
    pub fn new_name(&mut self, new_name: String) -> Result<(), ErrorType> {
        match self.entity {
            Some(_) => Err(ErrorType::System("You already have a name.".to_owned())),
            None => {
                self.entity = Some(Entity { name: new_name });
                Ok(())
            }
        }
    }
    pub fn location(&self) -> String {
        self.current_location.clone()
    }
}

pub struct Location {
    place: Place,
}
impl Location {
    pub fn new(name: String, description: Vec<String>) -> Self {
        Self {
            place: Place { name, description },
        }
    }
    pub fn description(&self) -> String {
        self.place.description.join("\n")
    }
}
pub fn the_void() -> Location {
    Location::new(
        "The Void".to_owned(),
        vec![
            "You're in a pitch black Void.".to_owned(),
            "There's nothing. No sensations, no sounds, no life.".to_owned(),
            "Distance itself needs a second object to exist.".to_owned(),
            "You don't see even that, however. Nothing exists here.".to_owned(),
        ],
    )
}

pub struct World {
    pub player: Player,
    pub locations: HashMap<&'static str, Location>,
}

impl World {
    pub fn new() -> Self {
        Self {
            player: Player::new(),
            locations: HashMap::from([("The Void", the_void())]),
        }
    }
}
