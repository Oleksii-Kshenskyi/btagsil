use crate::pipeline::*;

// TODO: once the ECS/query system is ready, implement this and resolver
//       this takes the Command (result of Parser stage) and packages it with World
//       (and any other relevant data)
//       Parsed Command { Command } => Resolver Capsule { Command, World }
pub fn package_command_stage() {} // ????

// TODO: implement Resolver pipeline stage: its task is to take the parsed command + world (resolver capsule, result of package_command_stage) and produce the list of World mutations to apply + outcome (possible, impossible, ambiguous) + set of tokens for later use in narration
//       Resolver Capsule { Command, World } => Resolved { World, WorldMutationVec, OutcomeTags }
