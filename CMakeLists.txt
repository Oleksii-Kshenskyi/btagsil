cmake_minimum_required(VERSION 3.20)
project(btagsil)

set(SRC_DIR ${CMAKE_SOURCE_DIR}/src)
set(GAME_SRC ${SRC_DIR}/main.cxx)
set(GAME_INCLUDES ${SRC_DIR})

set(BOOST_INCLUDE_LIBRARIES thread filesystem system program_options)
set(BOOST_ENABLE_CMAKE ON)

# FetchContent: retrieving thirdparty dependencies
include(FetchContent)

# Thirdparty dependency: Boost
FetchContent_Declare(
  Boost
  GIT_REPOSITORY https://github.com/boostorg/boost.git
  GIT_TAG boost-1.81.0
)
FetchContent_MakeAvailable(Boost)

# Thirdpartydependency: FTXUI
FetchContent_Declare(ftxui
  GIT_REPOSITORY https://github.com/ArthurSonzogni/ftxui
  GIT_TAG v4.0.0
)
FetchContent_GetProperties(ftxui)
if(NOT ftxui_POPULATED)
  FetchContent_Populate(ftxui)
  add_subdirectory(${ftxui_SOURCE_DIR} ${ftxui_BINARY_DIR} EXCLUDE_FROM_ALL)
endif()
FetchContent_MakeAvailable(ftxui)

include_directories(${GAME_INCLUDES})

add_executable(btagsil ${GAME_SRC})
target_link_libraries(btagsil PRIVATE Boost::filesystem
                                      Boost::program_options
                                      ftxui::dom
                                      ftxui::screen
                                      ftxui::component)
