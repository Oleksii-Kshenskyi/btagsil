from repl import repl_once

def main() -> None:
    while True:
        print("==> ", sep="", end="")
        repl_once(input())

if __name__ == "__main__":
    main()