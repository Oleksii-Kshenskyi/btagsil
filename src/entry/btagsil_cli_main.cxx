#include <string>
#include <iostream>

#include "engine/action.hxx"

int main() {
    std::string user_input;
    while(user_input != "exit") {
        std::cout << "=>> ";
        std::getline(std::cin, user_input);
        std::cout << "Echoed: '" << user_input << "'" << std::endl << std::endl;
    }

    engine::say_kekw();

    return 0;
}