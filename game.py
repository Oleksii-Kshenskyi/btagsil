from btagsil.repl import repl_once
from btagsil.world import init_world

def main() -> None:
    world = init_world()
    while True:
        print("==> ", sep="", end="")
        repl_once(world, input().lower())

if __name__ == "__main__":
    main()