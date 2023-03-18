message("warnings.cmake: triggered!")
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options(-Wall
                        -Wextra
                        -Wpedantic
                        -Wconversion
                        -Wsign-conversion
                        -Werror
                        -Werror=conversion
                        -Werror=sign-conversion)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    add_compile_options(-Wall
                        -Wextra
                        -Wpedantic
                        -Wconversion
                        -Wsign-conversion
                        -Werror)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    add_compile_options(/W4
                        /WX)
endif()