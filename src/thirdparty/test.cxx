#include "boost/filesystem.hpp"
#include "boost/hof.hpp"
#include "boost/json.hpp"
#include "thirdparty/test.hxx"

#include <iostream>


namespace thirdparty {
    void test_thirdparty() {
        std::cout << boost::filesystem::current_path() << std::endl;

        std::string p = "{}";
        auto something = boost::json::parse(p);
    }
}