set(BT_ENGINE_DIR ${BT_SRC_DIR}/engine)
set(BT_ENGINE_SRC ${BT_ENGINE_DIR}/action.cxx)

add_library(engine STATIC ${BT_ENGINE_SRC})
target_compile_options(engine PRIVATE ${BT_WARNINGS_STRICT})