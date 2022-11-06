#include <algorithm>
#include <numeric>
#include <iterator>
#include <string>
#include <sstream>
#include <iostream>


namespace util {
    template<typename T>
    bool eq(std::vector<T> first, std::vector<T> second) {
        return std::equal(first.begin(), first.end(), second.begin());
    }

    std::vector<std::string> split(const std::string &input) { 
        std::istringstream buffer(input);
        std::vector<std::string> ret((std::istream_iterator<std::string>(buffer)), 
                                    std::istream_iterator<std::string>());
        return ret;
    }

    std::string join(const std::vector<std::string>& vec, const std::string& sep) {
        std::ostringstream joined;
        for(uint32_t i = 0; i < vec.size(); i++) {
            joined << vec[i] << ((i == vec.size() - 1) ? "" : sep);
        }
        return joined.str();
    }

    std::string display_vec(const std::vector<std::string>& vec) {
        return std::string("[") + join(vec, ", ") + std::string("]");
    }
}