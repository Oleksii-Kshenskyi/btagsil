from sys import exit

def repl_unknown(what: list[str]):
    print(f"Wat? What's a '{' '.join(what)}'?")

def repl_exit(args: list[str]) -> None:
    match args:
        case []:
            print("Thanks for playing! Hope to see you again soon!\n")
            exit(0)
        case x: print(f"If you want to exit the game, you don't need any {' '.join(x)}s. Just write 'exit'.")

def repl_echo(what: list[str]) -> None:
    match what:
        case []: print("What do you want to echo?")
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