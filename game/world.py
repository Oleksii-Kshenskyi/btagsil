from dataclasses import dataclass

import game.data as d
import game.logic as l

def _current_place(TheWorld: d.World) -> d.Place:
    return TheWorld.places[TheWorld.player.current_location]
def _place_display_name(TheWorld: d.World, place: d.Place) -> str:
    return TheWorld.places[place.id].display_name
def _current_place_display_name(TheWorld: d.World) -> str:
    return _place_display_name(TheWorld, _current_place(TheWorld))
def _current_place_description(TheWorld: d.World) -> str:
    curplace = _current_place(TheWorld)
    if len(curplace.description.box) > 0:
        return _current_place(TheWorld).description.join()
    else: return f"You peer into the very essence of the thing called `{curplace.display_name}`.\n" + f"`{curplace.display_name}`'s essence seems rather plain."

def name_current_place(TheWorld: d.World) -> str:
    return f"You are in {_current_place_display_name(TheWorld)}."
def describe_current_place(TheWorld) -> str:
    return _current_place_description(TheWorld)

def create_new_place(TheWorld: d.World, name: str) -> str:
    name = name if name != None else "Place " + str(len(TheWorld.places) - 1)
    TheWorld.places[name] = d.Place(name, name, l.box([]), [])
    return f"You gaze at The Void.\nYou make It birth you a new place.\nIt emerges from the boiling Darkness of The Void as `{name}`."

def create_world() -> d.World:
    return d.World(
        places={"void": d.void()},
        player=d.create_player()
    )