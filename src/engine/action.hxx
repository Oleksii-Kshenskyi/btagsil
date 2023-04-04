#pragma once

/**
 *  Core response function. Chooses how to react to user input.
 *  @param user_input the input provided by the user in the REPL.
 *  @return the game's response to this specific user input.
*/
std::string respond_to_action(std::string&& user_input);
