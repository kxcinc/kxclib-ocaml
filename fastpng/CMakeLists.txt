cmake_minimum_required(VERSION 3.17)
project(kxclib_fastpng_libpng)

execute_process(COMMAND ocamlfind printconf path OUTPUT_VARIABLE OCAMLFINDPATH OUTPUT_STRIP_TRAILING_WHITESPACE)
set(OCAMLLIB "${OCAMLFINDPATH}/ocaml")

find_package(PNG REQUIRED)
include_directories(${PNG_INCLUDE_DIR} ${OCAMLLIB})

set(CMAKE_CXX_STANDARD 14)

add_library(fastpng_libpng fastpng_libpng.cpp ${PNG_LIBRARY})
add_executable(tryout fastpng_libpng_tryout.cpp fastpng_libpng ${PNG_LIBRARY})

target_link_libraries(tryout ${PNG_LIBRARY})
