#include <sstream>


/**
* A namespace that contains strings of text used in the game.
* NOTE: this should only contain namespaces, string constants and
*       pure functions that take simple datatypes like string/int and return strings.
*/
namespace Data {
    std::string echoed_string(std::string&& echoed) {
        return (std::stringstream() << "Echoing: '" << std::move(echoed) << "'").str();
    };
    std::string unknown_action(std::string&& thing) {
        return (std::stringstream() << "Whoopsie... No clue what '" << std::move(thing) << "' is T_T").str();
    };
    const std::string EXIT_MESSAGE { "Thanks for playing! Come back soon ^_^" };
};