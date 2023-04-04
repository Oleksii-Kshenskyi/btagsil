#include <iostream>
#include <iomanip>
#include <iostream>
#include <string>
#include <sstream>
#include <type_traits>
#include <variant>

#include "engine/action.hxx"
#include "engine/data.hxx"
#include "thirdparty/string_utils.hxx"

template<class... Ts> struct ActionHandlers : Ts... { using Ts::operator()...; };
template<class... Ts> ActionHandlers(Ts...) -> ActionHandlers<Ts...>;

struct Empty {};
struct Exit {};
struct Unknown {
    std::string thing;
};
struct Echo {
    std::string echoed;
};
using Action = std::variant<Empty, Unknown, Echo, Exit>;

static Action action_from_input(std::string&& user_input) {
    std::string first_word { user_input }; first_word = lowercase(first_word_of(std::move(first_word)));
    if(first_word.empty()) {
        return Action { Empty {} };
    } else if(first_word == "echo") {
        return Action { Echo { skip_first_word(std::move(user_input)) } };
    } else if(first_word == "exit") {
        return Action { Exit {} };
    } else return Action { Unknown { std::move(user_input) } };
}

std::string respond_to_action(std::string&& user_input) {
    Action actual_action = action_from_input(std::move(user_input));
    return std::visit(ActionHandlers {
        [](Empty act) { (void) act; return std::string(); },
        [](Exit act) {
            (void) act;
            std::cout << Data::EXIT_MESSAGE << std::endl << std::endl;
            std::exit(0);
            return std::string();
        },
        [](Unknown act) { return Data::unknown_action(std::move(act.thing)); },
        [](Echo act) { return Data::echoed_string(std::move(act.echoed)); }
    }, actual_action);
}

