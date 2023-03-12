#include <boost/hof.hpp>

#include <iostream>

using namespace boost::hof;

// A sum function object
struct sum_f
{
    template<class T, class U>
    auto operator()(T x, U y) const
    {
        return x + y;
    }
};

BOOST_HOF_STATIC_FUNCTION(sum) = pipable(sum_f());

int main() {
    auto thirteen = 3 | sum(6) | sum(4);
    std::cout << "Boost.HOF pipable: should be thirteen: " << thirteen << std::endl;

    return 0;
}