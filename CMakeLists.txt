cmake_minimum_required(VERSION 3.20)
project(btagsil)


# Global properties for the entire project
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(BT_PROJECT_DIR ${CMAKE_SOURCE_DIR})
set(BT_SRC_DIR ${BT_PROJECT_DIR}/src)
set(BT_ENTRY_DIR ${BT_SRC_DIR}/entry)
set(BT_CLI_ENTRY ${BT_ENTRY_DIR}/btagsil_cli_main.cxx)
set(BT_SANDBOX_ENTRY ${BT_ENTRY_DIR}/sandbox.cxx)
set(GAME_INCLUDES ${BT_SRC_DIR})

# Specify the directory for CMake modules used and enable compiler warnings
set(CMAKE_MODULES ${BT_PROJECT_DIR}/cmake/modules)
include(${CMAKE_MODULES}/warnings.cmake)

# All the targets BTAGSIL's entry points link to
set(INTERNAL_TARGETS engine thirdparty)

# Sub-targets just include headers starting from the src directory, same as the entry points
include_directories(${GAME_INCLUDES})
# All the subtargets are in the src directory
add_subdirectory(${BT_SRC_DIR})


# BTAGSIL's entry points
add_executable(btagsil-cli ${BT_CLI_ENTRY})
target_link_libraries(btagsil-cli PRIVATE ${INTERNAL_TARGETS})
target_compile_options(btagsil-cli PRIVATE ${BT_WARNINGS_NORMAL})

# Sandbox executable for experimentation and figuring out C++
add_executable(sandbox ${BT_SANDBOX_ENTRY})
target_link_libraries(sandbox PRIVATE ${INTERNAL_TARGETS})
target_compile_options(sandbox PRIVATE ${BT_WARNINGS_NORMAL})
