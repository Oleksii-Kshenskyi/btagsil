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

    // TODO: write a query() function using the written pseudo-ECS system
    //       for now query() should only search for entities by tags,
    //       ... and maybe think about implementing a precise entity-by-id query

    fn bump_id(&mut self) -> u64 {
        self.last_id += 1;
        self.last_id
    }
}
