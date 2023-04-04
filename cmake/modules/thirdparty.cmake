# Thirdparty dependencies
set(BOOST_INCLUDE_LIBRARIES algorithm)
set(BOOST_ENABLE_CMAKE ON)
# FetchContent: retrieving thirdparty dependencies
include(FetchContent)
set(FETCHCONTENT_QUIET FALSE)
# Thirdparty dependency: Boost
FetchContent_Declare(
  Boost
  GIT_REPOSITORY https://github.com/boostorg/boost.git
  GIT_TAG boost-1.81.0
  GIT_PROGRESS TRUE
)
FetchContent_MakeAvailable(Boost)