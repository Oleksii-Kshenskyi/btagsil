#include <string>
#include <iostream>

#include <filesystem>

#include "engine/action.hxx"
#include "thirdparty/test.hxx"

int main() {
    std::string user_input;
    while(user_input != "exit") {
        std::cout << "=>> ";
        std::getline(std::cin, user_input);
        std::cout << "Echoed: '" << user_input << "'" << std::endl << std::endl;
    }

    engine::say_kekw();
    thirdparty::test_thirdparty();

    return 0;
}