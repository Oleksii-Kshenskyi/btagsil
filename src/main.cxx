#include <iostream>
#include <vector>
#include <algorithm>

#include "repl.cxx"

int main() {
    // while(true) {
    //     std::string input;
    //     std::cout << "++>> "; std::getline(std::cin, input);
    //     std::cout << std::flush << repl::once(input) << std::endl << std::endl;
    // }
    std::vector<std::string> v {"one", "two", "three", "four"};
    std::cout << util::join(v, ".") << std::endl;
    std::cout << util::display_vec(v) << std::endl;
    std::cout << util::display_vec(util::split("\t   one    two    three   four five KEKW  ")) << std::endl;
    std::cout << "Equal? " << ((util::eq(v, std::vector<std::string>{"one", "two", "three"})) ? "yes" : "no") << std::endl;

    return 0;
}