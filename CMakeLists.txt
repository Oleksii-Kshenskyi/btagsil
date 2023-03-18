cmake_minimum_required(VERSION 3.20)
project(btagsil)


# Global properties for the entire project
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(BT_SRC_DIR ${CMAKE_SOURCE_DIR}/src)
set(BT_ENTRY_DIR ${BT_SRC_DIR}/entry)
set(BT_CLI ${BT_ENTRY_DIR}/btagsil_cli_main.cxx)
set(GAME_INCLUDES ${BT_SRC_DIR})


# All the targets BTAGSIL's entry points link to
set(THIRDPARTY_TARGETS Boost::hof)
set(INTERNAL_TARGETS engine)


# Thirdparty dependencies
set(BOOST_INCLUDE_LIBRARIES hof)
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


# Sub-targets just include headers starting from the src directory, same as the entry points
include_directories(${GAME_INCLUDES})
# All the subtargets are in the src directory
add_subdirectory(${BT_SRC_DIR})


# BTAGSIL's entry points
add_executable(btagsil-cli ${BT_CLI})
target_link_libraries(btagsil-cli PUBLIC ${THIRDPARTY_TARGETS} ${INTERNAL_TARGETS})
