/// This file contains the mini-ECS and the World struct, which
/// contains all the entities in the world.
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::common::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityId {
    pub id: u64,
}
impl EntityId {
    pub fn new(id: u64) -> Self {
        Self { id }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityTag {
    pub tag: &'static str,
}
impl EntityTag {
    pub fn new(s: &'static str) -> Self {
        Self { tag: s }
    }
}
pub fn tag(s: &'static str) -> EntityTag {
    EntityTag::new(s)
}

// [ ]: Okay, Entity trait exists, but I have no actual Entity types.
//      I need a couple of them, like some components of the Player, for example.
pub trait Entity {
    fn id(&self) -> EntityId;
    fn get(&self) -> AnyResult;
    fn tags(&self) -> HashSet<EntityTag>;
}

pub fn ent<E>(e: E) -> Rc<dyn Entity>
where
    E: Entity + 'static,
{
    Rc::new(e)
}

pub struct World {
    last_id: u64,
    entities: HashMap<EntityId, Rc<dyn Entity>>,
    tags: HashMap<EntityTag, Vec<Rc<dyn Entity>>>,
}

impl World {
    pub fn new() -> Self {
        Self {
            last_id: 0,
            entities: HashMap::new(),
            tags: HashMap::new(),
        }
    }

    pub fn spawn<E>(&mut self, entity: E) -> EntityId
    where
        E: Entity + 'static,
    {
        let id = EntityId::new(self.bump_id());
        let tags = entity.tags().clone();
        let shared_ent = ent(entity);
        for tag in tags {
            self.tags
                .entry(tag)
                .or_insert(vec![])
                .push(shared_ent.clone());
        }

        self.entities.insert(id, shared_ent);

        id
    }

    fn query_by_tag(&self, tags: Vec<&'static str>) -> Vec<Rc<dyn Entity>> {
        tags.iter()
            .filter_map(|&tag| self.tags.get(&EntityTag::new(tag)))
            .flat_map(|ents| ents.iter().cloned())
            .collect()
    }

    fn get(&self, id: EntityId) -> Option<Rc<dyn Entity>> {
        self.entities.get(&id).cloned()
    }

    fn bump_id(&mut self) -> u64 {
        self.last_id += 1;
        self.last_id
    }
}

// [ ]: init_world() function that both creates and populates World to its initial state
// NOTE: I need a couple pre-existing entity types for this. Right now I only have the trait.
