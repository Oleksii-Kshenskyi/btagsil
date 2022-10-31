from sys import exit

import btagsil.data as data
import btagsil.world as w
from btagsil.world import World

# Immutable responses with info about the world

def repl_where(world: World, what: list[str]) -> str:
    match what:
        case []: return data.where_what()
        case ["is"]: return data.where_is_what()
        case ["is", *what]: return data.dont_know_where_is(' '.join(what))
        case ["am", "i"]: return w.current_loc_description(world)
        case ["can", "i", "go"]: return w.possible_destinations(world)
        case _: return data.where_error()

# Actions that mutate the world

def repl_go(world: World, where: list[str]) -> str:
    match where:
        case []: return data.can_only_go_to_stuff()
        case ["to"]: return data.go_to_where()
        case ["to", *where]: return w.move_to(world, where)
        case x: data.can_only_go_to_stuff()

def repl_look(world: World, what: list[str]) -> str:
    match what:
        case []: return data.empty_look()
        case ["at"]: return data.look_at_what()
        case ["at", *what]: return w.look_at_object(world, what)
        case ["around"]: return w.look_around(world)
        case _: return data.can_only_look_like_this()
        

# REPL actions

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

# Entry point for execution of a single action

def repl_once(world: World, input: str) -> None:
    tags = input.split()
    match tags:
        case []: repl_empty()
        case ["echo", *args]: repl_echo(args)
        case ["exit", *lol]: repl_exit(lol)
        case ["where", *what]: print(repl_where(world, what))
        case ["look", *what]: print(repl_look(world, what))
        case ["go", *where]: print(repl_go(world, where))
        case wtf: repl_unknown(wtf)
    print() if tags else print(end="", sep="")