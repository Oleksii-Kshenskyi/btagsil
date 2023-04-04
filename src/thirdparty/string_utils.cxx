#include "boost/algorithm/string.hpp"

#include <iostream>

std::string trim_left(std::string&& s) {
    boost::trim_left(s);
    return s;
}

std::string trim_right(std::string&& s) {
    boost::trim_right(s);
    return s;
}

std::string trim(std::string&& s) {
    boost::trim(s);
    return s;
}

std::string lowercase(std::string&& s) {
    boost::to_lower(s);
    return s;
}

std::string first_word_of(std::string&& s) {
    std::string fw_start { trim_left(std::move(s)) };
    if(fw_start.empty())
        return fw_start;
    auto fw_ends = fw_start.find_first_of(" \t\n\r");
    if(fw_ends == std::string::npos)
        return fw_start;
    
    fw_start.erase(std::next(fw_start.begin(), fw_ends), fw_start.end());
    return fw_start;
}

std::string skip_first_word(std::string&& s) {
    std::string start_at_second_word { trim_left(std::move(s)) };
    if(start_at_second_word.empty()) {
        return start_at_second_word;
    }
    auto fw_ends = start_at_second_word.find_first_of(" \t\n\r");
    if(fw_ends == std::string::npos) {
        return std::string();
    }
    auto sw_starts = start_at_second_word.find_first_not_of(" \t\n\r", fw_ends);
    if(sw_starts == std::string::npos) {
        return std::string();
    }

    start_at_second_word.erase(start_at_second_word.begin(), std::next(start_at_second_word.begin(), sw_starts));
    return start_at_second_word;
}