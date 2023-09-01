import game.commands as c
import game.world as w

def sky():
    TheWorld = w.create_world()
    while(True):
        user_input = input("SKY>> ")
        print(f"{c.run_command(user_input, TheWorld)}", sep="", end="")

if __name__ == "__main__":
    sky()