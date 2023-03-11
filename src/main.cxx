// #include <memory>  // for allocator, shared_ptr, __shared_ptr_access
// #include <string>  // for operator+, char_traits, to_string, string
// #include <vector>  // for vector

// #include "ftxui/component/captured_mouse.hpp"  // for ftxui
// #include "ftxui/component/component.hpp"       // for Slider, Renderer, Vertical
// #include "ftxui/component/component_base.hpp"  // for ComponentBase
// #include "ftxui/component/screen_interactive.hpp"  // for ScreenInteractive
// #include "ftxui/dom/elements.hpp"  // for Elements, Element, operator|, separator, text, focusPositionRelative, size, border, flex, frame, bgcolor, gridbox, vbox, EQUAL, center, HEIGHT, WIDTH
// #include "ftxui/screen/color.hpp"  // for Color

// using namespace ftxui;

// Element make_box(int x, int y) {
//   std::string title = "(" + std::to_string(x) + ", " + std::to_string(y) + ")";
//   return text(title) | center | size(WIDTH, EQUAL, 18) |
//          size(HEIGHT, EQUAL, 9) | border |
//          bgcolor(Color::HSV(x * 255 / 15, 255, y * 255 / 15));
// };

// Element make_grid() {
//   std::vector<Elements> rows;
//   for (int i = 0; i < 15; i++) {
//     std::vector<Element> cols;
//     for (int j = 0; j < 15; j++) {
//       cols.push_back(make_box(i, j));
//     }
//     rows.push_back(cols);
//   }

//   return gridbox(rows);
// };

// int main(int argc, const char* argv[]) {
//   float focus_x = 0.5f;
//   float focus_y = 0.5f;

//   auto slider_x = Slider("x", &focus_x, 0.f, 1.f, 0.01f);
//   auto slider_y = Slider("y", &focus_y, 0.f, 1.f, 0.01f);

//   auto renderer = Renderer(
//       Container::Vertical({
//           slider_x,
//           slider_y,
//       }),
//       [&] {
//         auto title = "focusPositionRelative(" +        //
//                      std::to_string(focus_x) + ", " +  //
//                      std::to_string(focus_y) + ")";    //
//         return vbox({
//                    text(title),
//                    separator(),
//                    slider_x->Render(),
//                    slider_y->Render(),
//                    separator(),
//                    make_grid() | focusPositionRelative(focus_x, focus_y) |
//                        frame | flex,
//                }) |
//                border;
//       });

//   auto screen = ScreenInteractive::Fullscreen();
//   screen.Loop(renderer);

//   return 0;
// }

// Copyright 2020 Arthur Sonzogni. All rights reserved.
// Use of this source code is governed by the MIT license that can be found in
// the LICENSE file.

#include <stdlib.h>
#include "ftxui/component/component.hpp"
#include <ftxui/dom/elements.hpp>
#include <ftxui/screen/screen.hpp>
#include <vector>

#include "ftxui/component/event.hpp"
#include "ftxui/dom/node.hpp"
#include "ftxui/component/screen_interactive.hpp"
#include "ftxui/screen/color.hpp"

// TODO: Probably create a service that will provide the functionality of writing
// a single line into the vbox of lines below.

// NOTE: probably use the input box from the homescreen example?
// That one looks like the closest thing to what I want.

int main() {
    using namespace ftxui;

    std::string user_input;
    // FIXME: the input widget is invisible (no border and can't see the font)
    auto input_component = Input(&user_input, "=>> ") | border | size(HEIGHT, EQUAL, 1) | color(Color::White);
    auto main_component = Renderer(Container::Vertical({
          input_component
        }), [&] {
        return vbox({
            // TODO: figure out how to properly change the size of this VBox so it takes up
            // all space but the single line of input, no matter how you resize it
            vbox({
                text("This is a KEKW vbox, colored RED.") | color(Color::Red)
            }) | size(HEIGHT, GREATER_THAN, 9) | border,
            input_component->Render() | color(Color::White)
        }) | size(HEIGHT, GREATER_THAN, 10);
    });
    auto screen = ScreenInteractive::Fullscreen();
    screen.Loop(main_component);
}
