import btagsil.data as data
from btagsil.data import World, Player, Forest, Square

# Working with locations

def current_loc_description(world: World):
    current_loc = world.locations[world.player.current_location]
    descr = current_loc.description
    name = current_loc.name
    return data.describe_loc(name, descr)

def init_world() -> World:
    return World(player = Player(),
                 locations = {"forest": Forest(),
                              "square": Square()})