cmake_minimum_required(VERSION 3.0)

enable_language(C)
project(erime VERSION 0.1.0 DESCRIPTION "librime bindings for emacs")

set(CMAKE_POSITION_INDEPENDENT_CODE TRUE CACHE BOOL "pic" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "shared" FORCE)
set(BUILD_CLAR OFF CACHE BOOL "clar" FORCE)

find_library(rime librime.so REQURIED)

add_library(erime SHARED
  src/interface.c
  src/liberime-bindings.c
  src/liberime.c)
set_target_properties(erime PROPERTIES C_STANDARD 99)

target_link_libraries(erime rime)

# Emacs looks for .so on linux and OSX.
# By default CMake builds .dylib on OSX, so override that.
if(APPLE)
  set_target_properties(erime PROPERTIES SUFFIX .so)
endif(APPLE)


# sanitizer and debug info,
# FIXME: remove when stable
if ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fuse-ld=gold -fsanitize=address,leak,undefined -fno-omit-frame-pointer -pg -g")
endif()

if(WIN32)
  set_target_properties(erime PROPERTIES PREFIX lib)
endif(WIN32)
