from dataclasses import dataclass
import sys

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


Command = Empty | Unknown | Exit | Echo

def parse_command(user_input: str) -> Command:
    match user_input.lower().split(sep=None):
        case []: return Empty()
        case ["echo", _, *_]: return Echo(string=user_input.split(maxsplit=1, sep=None)[1])
        case ["exit"]: return Exit(message="Thanks for playing! See ya ^_^")
        case _: return Unknown(command=user_input.split(maxsplit=1, sep=None)[0])

def execute_command(command: Command) -> str:
    match command:
        case Empty(): return ""
        case Echo(s): return f"`{s}`\n\n"
        case Exit(m): print(m + "\n"); sys.exit()
        case Unknown(c): return f"Sorry, no clue what the `{c}` command is supposed to do T_T\n\n"

# For external usage
def run_command(user_input: str) -> str:
    return execute_command(parse_command(user_input))