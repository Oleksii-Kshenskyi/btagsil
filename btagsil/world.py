import btagsil.data as data
from btagsil.data import World, Player, Location, Forest, Square

# Working with locations

def get_current_loc(world: World) -> Location:
    return world.locations[world.player.current_location]

def current_loc_description(world: World) -> str:
    current_loc = get_current_loc(world)
    descr = current_loc.description
    name = current_loc.name
    return data.describe_loc(name, descr)

def possible_destinations(world: World) -> str:
    current_loc = get_current_loc(world)
    destinations_text = ', '.join(map(lambda x: 'the ' + x, current_loc.connected))
    return data.you_can_go_to(destinations_text)

# Actions that mutate the World

# The go action

def move_to_destination(world: World, loc_name: str) -> str:
    world.player.current_location = loc_name
    return data.you_went_to(loc_name)

def move_to(world: World, where: list[str]) -> str:
    loc_name = ' '.join(where)
    current_loc_name = world.player.current_location
    connected = get_current_loc(world).connected

    already_there = loc_name == current_loc_name
    no_such_loc = loc_name not in world.locations
    not_connected = loc_name not in connected

    match [already_there, no_such_loc, not_connected]:
        case [True, _, _]: return data.already_there(loc_name)
        case [_, True, _]: return data.no_such_loc(loc_name)
        case [_, _, True]: return data.cant_go_there_from_here(loc_name, current_loc_name)
        case _: return move_to_destination(world, loc_name)

# Initializing the World

def init_world() -> World:
    return World(player = Player(),
                 locations = {"forest": Forest(),
                              "square": Square()})