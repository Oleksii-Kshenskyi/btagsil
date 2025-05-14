use std::rc::Rc;

use crate::parser::Command;
use crate::pipeline::*;
use crate::world::World;

pub struct ResolverCapsule {
    command: Command,
    world: Rc<World>,
}
impl ResolverCapsule {
    pub fn new(command: Command, world: Rc<World>) -> Self {
        Self { command, world }
    }
}

pub fn package_command_stage(world: Rc<World>) -> Box<dyn Stage> {
    stage("Package Command For Resolver", move |command: Command| {
        ResolverCapsule::new(command, world.clone())
    })
}

// TODO: implement Resolver pipeline stage: its task is to take the parsed command + world (resolver capsule, result of package_command_stage) and produce the list of World mutations to apply + outcome (possible, impossible, ambiguous) + set of tokens for later use in narration
//       Resolver Capsule { Command, World } => Resolved { World, WorldMutationVec, OutcomeTags }
