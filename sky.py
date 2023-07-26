import game.commands as c

def sky():
    while(True):
        user_input = input("SKY>> ")
        print(f"{c.run_command(user_input)}", sep="", end="")

if __name__ == "__main__":
    sky()