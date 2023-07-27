from dataclasses import dataclass

@dataclass
class Place:
    id: str
    display_name: str
    description: str
    adjacent: list[str]

@dataclass
class Player:
    current_location: str

@dataclass
class World:
    places: dict[str, Place]
    player: Player