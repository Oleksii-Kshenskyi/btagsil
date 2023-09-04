from dataclasses import dataclass
import sys

import game.data as d
import game.world as w
import game.logic as l

@dataclass
class Empty:
    pass
@dataclass
class Unknown:
    command: str
@dataclass
class Exit:
    message: str
@dataclass
class Echo:
    string: str
@dataclass
class SystemEcho:
    string: str


Command = Empty | Unknown | Exit | Echo | SystemEcho

def parse_create(create_args: list[str], TheWorld: d.World) -> str:
    match create_args:
        case []: return "What would You like to create?"
        case ["place"]: return w.create_new_place(TheWorld, None)
        case ["place", "called"]: return "What should the new place be called?"
        case ["new", "place"]: return w.create_new_place(TheWorld, None)
        case ["new", "place", "called"]: return "What should the new place be called?"
        case ["place", "called", *newname]: return w.create_new_place(TheWorld, ' '.join(newname))
        case ["new", "place", "called", *newname]: return w.create_new_place(TheWorld, ' '.join(newname))
        case _: return f"You ponder, unsure how to create the concept of `{' '.join(create_args)}`."

def parse_where(where_args: list[str], TheWorld: d.World) -> str:
    match where_args:
        case []: return "Where what?"
        case ["am", "i"]: return w.name_current_place(TheWorld)
        case ["is"]: return "Where is what?"
        case ["is", *what]: return f"No clue where `{' '.join(what)}` is T_T"
        case [*what]: return f"Did you want to ask `where is {l.box(what).join()}`?"

def parse_what(what_args: list[str], TheWorld: d.World) -> str:
    match what_args:
        case []: return "What `what`?"
        case ["is", "this", "place"]: return w.describe_current_place(TheWorld)
        case ["is"]: return "What is what?"
        case ["is", *thing]: return f"No clue what `{' '.join(thing)}` is ¯\_(*_*)_/¯"
        case a: return f"What `{' '.join(a)}` what?"

def parse_command(user_input: str, TheWorld: d.World) -> Command:
    match user_input.lower().split(sep=None):
        case []: return Empty()
        case ["create", *what]: return SystemEcho(string=parse_create(what, TheWorld))
        case ["where", *what]: return SystemEcho(string=parse_where(what, TheWorld))
        case ["what", *what]: return SystemEcho(string=parse_what(what, TheWorld))
        case ["echo", _, *_]: return Echo(string=user_input.split(maxsplit=1, sep=None)[1])
        case ["exit"]: return Exit(message="Thanks for playing! See ya ^_^")
        case _: return Unknown(command=user_input.split(maxsplit=1, sep=None)[0])

def execute_command(command: Command) -> str:
    match command:
        case Empty(): return ""
        case Echo(s): return f"`{s}`\n\n"
        case SystemEcho(s): return f"{s}\n\n"
        case Exit(m): print(m + "\n"); sys.exit()
        case Unknown(c): return f"Sorry, no clue what the `{c}` command is supposed to do T_T\n\n"

# For external usage
def run_command(user_input: str, TheWorld: d.World) -> str:
    return execute_command(parse_command(user_input, TheWorld))