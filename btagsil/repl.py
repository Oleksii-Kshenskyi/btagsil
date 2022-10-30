from sys import exit

import btagsil.data as data
import btagsil.world as w
from btagsil.world import World

def repl_where(world: World, what: list[str]) -> None:
    match what:
        case []: return data.where_what()
        case ["is"]: return data.where_is_what()
        case ["is", *what]: return data.dont_know_where_is(' '.join(what))
        case ["am", "i"]: return w.current_loc_description(world)
        case _: return data.where_error()

def repl_unknown(what: list[str]) -> None:
    print(data.unknown_action(' '.join(what)))

def repl_exit(args: list[str]) -> None:
    match args:
        case []:
            print(data.exit_message())
            exit(0)
        case x: print(data.exit_with_args(' '.join(x)))

def repl_echo(what: list[str]) -> None:
    match what:
        case []: print(data.echo_what())
        case x: print(f"{' '.join(x)}")

def repl_empty() -> None:
    """This is an empty action that does nothing. Occurs when user inputs nothing (or spaces) and presses enter."""

def repl_once(world: World, input: str) -> None:
    tags = input.split()
    match tags:
        case []: repl_empty()
        case ["echo", *args]: repl_echo(args)
        case ["exit", *lol]: repl_exit(lol)
        case ["where", *what]: print(repl_where(world, what))
        case wtf: repl_unknown(wtf)
    print() if tags else print(end="", sep="")