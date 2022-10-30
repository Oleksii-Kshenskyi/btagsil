from sys import exit

import data

def repl_unknown(what: list[str]):
    print(data.unknown_action(' '.join(what)))

def repl_exit(args: list[str]) -> None:
    match args:
        case []:
            print(data.exit_message())
            exit(0)
        case x: print(data.exit_with_arg(' '.join(x)))

def repl_echo(what: list[str]) -> None:
    match what:
        case []: print(data.echo_what())
        case x: print(f"{' '.join(x)}")

def repl_empty() -> None:
    """This is an empty action that does nothing. Occurs when user inputs nothing (or spaces) and presses enter."""

def repl_once(input: str) -> None:
    tags = input.split()
    match tags:
        case []: repl_empty()
        case ["echo", *args]: repl_echo(args)
        case ["exit", *lol]: repl_exit(lol)
        case wtf: repl_unknown(wtf)
    print() if tags else print(end="", sep="")