from dataclasses import dataclass, field
from abc import ABC

# REPL messages

def unknown_action(what: str) -> str:
    return f"Wat? What's a '{what}'?"

def exit_message():
    return "Thanks for playing! Hope to see you again soon!\n"

def exit_with_args(what):
    return f"If you want to exit the game, you don't need any {what}s. Just write 'exit'."

def echo_what():
    return "What do you want to echo?"

# Data Classes

@dataclass
class Weapon(ABC):
    name: str
    description: str

@dataclass
class Fists(Weapon):
    name: str = "bare fists"
    description: str = "just your bare fists"

@dataclass
class Object(ABC):
    name: str
    description: str

@dataclass
class Guard(Object):
    name: str = "guard"
    description: str = "a burly muscular man in heavy armor wielding a halberd.\nHe's guarding the peace of the people who visit the square.\nHe seems friendly and doesn't mind an occasional chat with the visitors."

@dataclass
class Location(ABC):
    name: str
    description: str
    connected: list[str]
    objects: dict[str, Object]

@dataclass
class Forest(Location):
    name: str = "forest"
    description: str = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a beautiful and peaceful feeling inside."
    connected: list[str] = field(default_factory=lambda: ["square"])
    objects: dict[str, Object] = field(default_factory=dict)

@dataclass
class Square(Location):
    name: str = "square"
    description: str = "a gigantic square full of people.\nYou suddenly long for some adventure!"
    connected: list[str] = field(default_factory = lambda: ["forest"])
    objects: dict[str, Object] = field(default_factory = lambda: {"guard": Guard()})

@dataclass
class Player:
    current_location: str = "forest"
    weapon: Weapon = Fists()

@dataclass
class World:
    player: Player
    locations: dict[str, Location]

# Location helpers

def describe_loc(name: str, descr: str):
    return f"You're in a {name}. It's {descr}"

def you_can_go_to(destinations: str) -> str:
    return f"You can go to {destinations} from here."

# Object helpers

def look_around(found: str) -> str:
    return f"You see {found} here."

def look_at_object(name: str, descr: str) -> str:
    return f"You see a {name}. It's {descr}."

# World mutator messages

def you_went_to(loc_name: str) -> str:
    return f"You went to the {loc_name}."

# Error helpers

# go to errors

def already_there(loc_name: str) -> str:
    return f"You can't go to the {loc_name}, you're already there!"

def no_such_loc(loc_name):
    return f"There's no such place as a {loc_name}."

def can_only_go_to_stuff() -> str:
    return "Sorry, I only know how to go 'to' places."

def go_to_where() -> str:
    return "Go to... where?"

def cant_go_there_from_here(loc_name: str, current_loc_name: str) -> str:
    return f"""You can't go to {loc_name} from {current_loc_name}, they're not connected.\n
               To see where you can go from here, try 'where can i go'."""

# where errors

def where_what() -> str:
    return "Where what?\nMaybe try 'where am i' or 'where can i go'?"

def where_is_what() -> str:
    return "Where is what?"

def where_error() -> str:
    return "I can only answer questions like 'where is <something>' or 'where am i' or 'where can i go'."

def dont_know_where_is(what: str) -> str:
    return f"Sorry, no idea where {what} is ¯\_(ツ)_/¯"

# look errors

def empty_look() -> str:
    return "Look... how? And where?\nTry 'look around' or 'look at <object>.\n'"

def look_at_what() -> str:
    return "Look at... what?"

def can_only_look_like_this() -> str:
    return "You told me to look, but I don't know what you meant.\nMaybe try 'look around' or 'look at <object>'?"

def nothing_to_look_at() -> str:
    return "You don't see anything of importance around here."

def no_object_around(object_name: str) -> str:
    return f"You don't see any {object_name}s around."