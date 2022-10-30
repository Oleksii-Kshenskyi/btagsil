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

def init_world() -> World:
    return World(player = Player(),
                 locations = {"forest": Forest(),
                              "square": Square()})