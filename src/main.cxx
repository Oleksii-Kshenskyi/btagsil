#include <string>
#include <iostream>

int main() {
    std::string user_input;
    while(user_input != "exit") {
        std::cout << "=>> ";
        std::getline(std::cin, user_input);
        std::cout << "Echoed: '" << user_input << "'" << std::endl << std::endl;
    }

    return 0;
}