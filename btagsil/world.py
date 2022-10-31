import random as r

import btagsil.data as data
from btagsil.data import World, Player, Object, Location, Forest, Square, WeaponShop

# Utility functions

def articled_enumeration(things: list, article: str) -> str:
    return ', '.join(map(lambda x: article + ' ' + x, things))

def object_has_prop(loc: Location, object_name: str, prop: str) -> bool:
    object_exists = object_name in loc.objects
    the_object = loc.objects[object_name] if object_exists else None
    object_has_the_prop = prop in the_object.properties if object_exists else False
    return object_has_the_prop

def take_out_of_pandoras_box(the_box: list[str]) -> str:
    return r.choice(the_box)

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

def talk_to_guard(the_talker: Object) -> str:
    guard_says = take_out_of_pandoras_box(the_talker.behavior["pandora's box"])
    return data.entity_says(the_talker.name, guard_says)

def talk_to_shopkeeper(the_talker: Object) -> str:
    return data.entity_says(the_talker.name, data.shopkeeper_line())

def choose_talker_and_talk(world: World, entity: str) -> str:
    the_talker = get_current_loc(world).objects[entity]
    match entity:
        case "guard": return talk_to_guard(the_talker)
        case "shopkeeper": return talk_to_shopkeeper(the_talker)
        case _: ValueError(f"world.choose_talker_and_talk: Unknown talker '{entity}'")

def talk_to(world: World, entity: list[str]) -> str:
    current_loc = get_current_loc(world)
    current_objects = list(current_loc.objects.keys())
    object_name = ' '.join(entity)
    object_exists = object_name in current_objects
    object_talks = object_has_prop(current_loc, object_name, "talks")
    match [object_exists, object_talks]:
        case [True, True]: return choose_talker_and_talk(world, object_name)
        case [True, False]: return data.object_doesnt_talk(object_name)
        case [False, False]: return data.talker_object_doesnt_exist(object_name)
        case [False, True]: raise ValueError("world.talk_to(): UNREACHABLE: an object that doesn't exist but talks?!")


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
                              "square": Square(),
                              "weapon shop": WeaponShop()})