#pragma once

#include <string>

/**
* Trims everything until the first non-whitespace character.
* @param s the string to trim.
* @return the moved and trimmed result string.
*/
std::string trim_left(std::string&& s);

/**
* Trims everything until the last non-whitespace character, starting from the end of the string.
* @param s the string to trim.
* @return the moved and trimmed result string.
*/
std::string trim_right(std::string&& s);

/**
* trim_left + trim_right.
* @param s the string to trim.
* @return the moved and trimmed result string.
*/
std::string trim(std::string&& s);

/**
* Strips everything but the first word in the string.
* @param s the string to find the first word of.
* @return the muated string containing the first word or empty string if there are no words in s.
*/
std::string first_word_of(std::string&& s);

/**
* Gives back the same string but mutated so that all uppercase characters are changed to lowercase.
* @param s the string to lowercase.
* @return the mutated string containing the lowercase version of s.
*/
std::string lowercase(std::string&& s);

/**
* Mutates s so that it only contains the part of s starting from the second word.
* @param s the string to skip the first word of.
* @return the mutated string without the first word or empty string if there are less than two words in the original.
*/
std::string skip_first_word(std::string&& s);
