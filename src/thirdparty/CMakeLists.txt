# All thirdparty libs that BTAGSIL uses are defined here
include(${CMAKE_MODULES}/thirdparty.cmake)

set(BT_THIRDPARTY_DIR ${BT_SRC_DIR}/thirdparty)
set(BT_THIRDPARTY_SRC ${BT_THIRDPARTY_DIR}/string_utils.cxx)

list(APPEND THIRDPARTY_TARGETS Boost::algorithm)

add_library(thirdparty STATIC ${BT_THIRDPARTY_SRC})
target_link_libraries(thirdparty PRIVATE ${THIRDPARTY_TARGETS})
target_compile_options(thirdparty PRIVATE ${BT_WARNINGS_NONE})
