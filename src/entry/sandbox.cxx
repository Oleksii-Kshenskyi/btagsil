#include <iomanip>
#include <iostream>
#include <string>
#include <sstream>
#include <type_traits>
#include <variant>
#include <vector>

// ===================== std::visit START ===========================

// struct Trash {
//     // Trash(int x, int y): x(x), y(y) {}
//     std::string x;
//     int y;
// };

// struct KEKW {};

// using var_t = std::variant<int, long, double, std::string, Trash, KEKW>;

// template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
// template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

// int main() {
//     std::vector<var_t> vec = {10, 15l, 1.5, "hello", Trash {"KEKW", 420}, KEKW {}};
//     for (auto& v: vec) {
//         std::string visited = std::visit(overloaded {
//             [](const std::string& arg) -> std::string { return (std::stringstream() << std::quoted(arg)).str(); },
//             [](int arg) -> std::string { return std::to_string(arg) + "i"; },
//             [](long arg) -> std::string { return std::to_string(arg) + "l"; },
//             [](double arg) -> std::string { return std::to_string(arg) + "f"; },
//             [](const Trash& arg) -> std::string { return std::string("Trash.x: ") + arg.x + std::string(", Trash.y: ") + std::to_string(arg.y); },
//             [](KEKW arg) -> std::string { (void) arg; return "AHHAHA KEKW!!"; }
//         }, v);
//         std::cout << visited << " ";
//     }
//     std::cout << std::endl;
//     return 0;
// }

// ===================== std::visit END ===========================

// ====== parameter passing during function chaining START ========

// std::string one(std::string&& subject) {
//     subject[1] = 'B';
//     return std::move(subject);
// }

// std::string two(std::string&& subject) {
//     subject[2] = 'E';
//     return std::move(subject);
// } 

// std::string test(std::string&& subject) {
//     return two(one(std::move(subject)));
// }

// int main() {
//     std::string kekw { "KEKWING TO SEE HERE" };
//     std::cout << "KEKW ADDRESS: " << (void*)kekw.c_str() << std::endl;
//     std::string result { test(std::move(kekw)) };
//     std::cout << "RESLUT: '" << result << "'" << std::endl;
//     std::cout << "RESLUT address: " << (void*)result.c_str() << std::endl;
//     return 0;
// }

// ====== parameter passing during function chaining END ========

// ====================== rvalue copies? START ==================

void funca(std::string&& s) {
    std::string cpy { s };
    cpy[0] = '2'; s[0] = '1';
    std::cout << "s: " << s << std::endl;
    std::cout << "cpy: " << cpy << std::endl;
    std::cout << "s addr: " << (void*) s.c_str() << std::endl;
    std::cout << "cpy addr: " << (void*) cpy.c_str() << std::endl;
}

int main() {
    std::string s1 { "KKEW" };
    funca(std::move(s1));
}

// ====================== rvalue copies? END ==================