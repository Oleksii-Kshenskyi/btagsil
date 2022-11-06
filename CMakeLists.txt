cmake_minimum_required(VERSION 3.24.0)

project(BTAGSIL)

set(SRC "./src")

include_directories(${SRC})

add_executable(btagsil ${SRC}/main.cxx)
set_target_properties(btagsil 
                        PROPERTIES
                            CXX_STANDARD 20
                            CXX_STANDARD_REQUIRED On)