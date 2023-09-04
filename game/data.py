from dataclasses import dataclass
from game.logic import box, ChanceBox

@dataclass
class Place:
    id: str
    display_name: str
    description: ChanceBox
    adjacent: list[str]

@dataclass
class Player:
    current_location: str

def create_player() -> Player:
    return Player(current_location="void")

@dataclass
class World:
    places: dict[str, Place]
    player: Player

# [LOCATIONS/PLACES]

def void() -> Place:
    return Place(
        id="void",
        display_name="The Void",
        description=box(["Boiling.", "Searing.", "Deafeningly silent.", "Pitch black.", "Nothing.", "The Void."]),
        adjacent=[]
    )