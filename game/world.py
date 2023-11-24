from dataclasses import dataclass

import game.data as d
import game.logic as l
import game.error as e
from game.logic import box

def _current_place(TheWorld: d.World) -> d.Place:
    return TheWorld.places[TheWorld.player.current_location]
def _place_display_name(TheWorld: d.World, place: d.Place) -> str:
    return TheWorld.places[place.id].display_name
def _current_place_display_name(TheWorld: d.World) -> str:
    return _place_display_name(TheWorld, _current_place(TheWorld))
def _current_place_description(TheWorld: d.World) -> str:
    curplace = _current_place(TheWorld)
    if len(curplace.description.box) > 0:
        return _current_place(TheWorld).description.to_lines()
    else: return f"You peer into the very essence of the thing called `{curplace.display_name}`.\n" + f"`{curplace.display_name}`'s essence seems rather plain."

def name_current_place(TheWorld: d.World) -> str:
    return f"You are in {_current_place_display_name(TheWorld)}."
def describe_current_place(TheWorld) -> str:
    return _current_place_description(TheWorld)

def _change_current_location(place_id: str, TheWorld: d.World):
    TheWorld.player.current_location = place_id
    return f"You went to `{_current_place(TheWorld).display_name}`."

# TODO: Develop the limitation mechanism: you can only go to a location connected to the current one.
def go_to_place(place: list[str], TheWorld: d.World) -> str:
    place_id = box(place).to_id()
    place_display_name = box(place).to_words()
    curplace = _current_place(TheWorld)
    doesnt_exist = place_id not in TheWorld.places.keys()
    already_there = place_id == curplace.id

    match [doesnt_exist, already_there]:
        case [True, _]: return f"The Void hasn't birthed any places with the name `{place_display_name}` yet."
        case [False, True]: return f"You're already in `{place_display_name}`."
        case [False, False]: return _change_current_location(place_id, TheWorld)
        case _: raise e.UnreachableError(text="Go To Place: UNREACHABLE!")

def create_world() -> d.World:
    return d.World(
        places={"void": d.void(), "white-room": d.white_room()},
        player=d.create_player()
    )