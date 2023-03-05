    #include <stdlib.h>
    #include "ftxui/component/component.hpp"
    #include <ftxui/dom/elements.hpp>
    #include <ftxui/screen/screen.hpp>
    #include <vector>

    #include "ftxui/component/event.hpp"
    #include "ftxui/dom/node.hpp"
    #include "ftxui/component/screen_interactive.hpp"
    #include "ftxui/screen/color.hpp"

int main() {
    using namespace ftxui;
    Element document = graph([](int x, int y) {
        std::vector<int> result(x, 0);
        for (int i{0}; i < x; ++i) {
        result[i] = ((3 * i) / 2) % y;
        }
        return result;
    });

    document |= color(Color::Red);
    document |= bgcolor(Color::DarkBlue);
    document |= border;
    std::vector<Event> ve;
    auto screen = ScreenInteractive::Fullscreen();
    auto main_component = Renderer([&] {
        return vbox({
            text("KEKW!!!") | size(HEIGHT, GREATER_THAN, 2) | hcenter,
            document,
        });
    });

    main_component |= CatchEvent([&](Event e) {
        if(e.is_character() && e.character() == "q") exit(0);
        return true;
    });
    screen.Loop(main_component);
}
