from dataclasses import dataclass

import game.data as d

def _current_place(TheWorld: d.World) -> d.Place:
    return TheWorld.places[TheWorld.player.current_location]
def _place_display_name(TheWorld: d.World, place: d.Place) -> str:
    return TheWorld.places[place.id].display_name
def _current_place_display_name(TheWorld: d.World) -> str:
    return _place_display_name(TheWorld, _current_place(TheWorld))
def _current_place_description(TheWorld: d.World) -> str:
    return _current_place(TheWorld).description.join()

def name_current_place(TheWorld: d.World) -> str:
    return f"You are in {_current_place_display_name(TheWorld)}."
def describe_current_place(TheWorld) -> str:
    return _current_place_description(TheWorld)

def create_world() -> d.World:
    return d.World(
        places={"void": d.void()},
        player=d.create_player()
    )