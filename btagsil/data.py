from dataclasses import dataclass
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
class Location(ABC):
    name: str
    description: str

@dataclass
class Forest(Location):
    name: str = "forest"
    description: str = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a beautiful and peaceful feeling inside."

@dataclass
class Square(Location):
    name: str = "square"
    description: str = "a gigantic square full of people.\nYou suddenly long for some adventure!"

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

# Error helpers

# where errors

def where_what() -> None:
    return "Where what?\nMaybe try 'where am i' or 'where can i go'?"

def where_is_what() -> None:
    return "Where is what?"

def where_error() -> None:
    return "I can only answer questions like 'where is <something>' or 'where am i' or 'where can i go'."

def dont_know_where_is(what: str) -> None:
    return f"Sorry, no idea where {what} is ¯\_(ツ)_/¯"