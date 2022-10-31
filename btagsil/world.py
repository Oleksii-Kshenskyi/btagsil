import btagsil.data as data
from btagsil.data import World, Player, Location, Forest, Square

# Utility functions

def articled_enumeration(things: list, article: str) -> str:
    return ', '.join(map(lambda x: article + ' ' + x, things))

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
    destinations_text = articled_enumeration(current_loc.connected, 'the')
    return data.you_can_go_to(destinations_text)

# Navigational actions

def look_around(world: World) -> str:
    object_names = list(get_current_loc(world).objects.keys())
    match object_names:
        case []: return data.nothing_to_look_at()
        case names: return data.look_around(articled_enumeration(names, 'a'))

def look_at_object(world, what):
    object_name = ' '.join(what)
    cur_objects = get_current_loc(world).objects
    object_exists = object_name in cur_objects

    if object_exists:
        the_object = cur_objects[object_name]
        return data.look_at_object(the_object.name, the_object.description)
    else:
        return data.no_object_around(object_name)


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