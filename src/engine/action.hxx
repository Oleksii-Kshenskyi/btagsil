#pragma once

// A set of types wrapped by a variant
// The types describe the set of actions possible in BTAGSIL
// By actions we mean the distinct sequences of words
// The type of action is based on the first word in the action

/** Core response function. Chooses how to react to user input.
 *
 *  @param user_input the input provided by the user in the REPL.
 *  @return the game's response to this specific user input.
*/
std::string respond_to_action(const std::string& user_input);
