#include <iomanip>
#include <iostream>
#include <string>
#include <sstream>
#include <type_traits>
#include <variant>
#include <vector>

struct Trash {
    // Trash(int x, int y): x(x), y(y) {}
    std::string x;
    int y;
};

struct KEKW {};

using var_t = std::variant<int, long, double, std::string, Trash, KEKW>;

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

int main() {
    std::vector<var_t> vec = {10, 15l, 1.5, "hello", Trash {"KEKW", 420}, KEKW {}};
    for (auto& v: vec) {
        std::string visited = std::visit(overloaded {
            [](const std::string& arg) -> std::string { return (std::stringstream() << std::quoted(arg)).str(); },
            [](int arg) -> std::string { return std::to_string(arg) + "i"; },
            [](long arg) -> std::string { return std::to_string(arg) + "l"; },
            [](double arg) -> std::string { return std::to_string(arg) + "f"; },
            [](const Trash& arg) -> std::string { return std::string("Trash.x: ") + arg.x + std::string(", Trash.y: ") + std::to_string(arg.y); },
            [](KEKW arg) -> std::string { (void) arg; return "AHHAHA KEKW!!"; }
        }, v);
        std::cout << visited << " ";
    }
    std::cout << std::endl;
    return 0;
}