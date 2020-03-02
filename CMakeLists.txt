cmake_minimum_required(VERSION 3.0)

enable_language(C)
project(erime VERSION 0.1.0 DESCRIPTION "librime bindings for emacs")

set(CMAKE_POSITION_INDEPENDENT_CODE TRUE CACHE BOOL "pic" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "shared" FORCE)
set(BUILD_CLAR OFF CACHE BOOL "clar" FORCE)


add_library(erime SHARED "")


### Properties

set_target_properties(erime PROPERTIES C_STANDARD 99)

# Emacs looks for .so on linux and OSX.
# By default CMake builds .dylib on OSX, so override that.
if(APPLE)
  set_target_properties(erime PROPERTIES SUFFIX .so)
endif(APPLE)

if(WIN32)
  set_target_properties(erime PROPERTIES PREFIX lib)
endif(WIN32)


### Libraries and Includes

if (APPLE)
  if (NOT DEFINED ENV{RIME_PATH})
    message(FATAL_ERROR "Please set enviroment variable RIME_PATH to librime project dir.")
  endif()
  target_link_directories(erime PRIVATE "$ENV{RIME_PATH}/build/lib/Release/")
  target_include_directories(erime PRIVATE "$ENV{RIME_PATH}/src")
elseif (UNIX)
  find_library(rime librime.so REQUIRED)
  target_link_libraries(erime PRIVATE rime)
elseif (MINGW)
  find_package(Rime REQUIRED)
  if (${Rime_FOUND})
    target_include_directories(erime PRIVATE ${Rime_INCLUDE_DIR})
    target_link_libraries(erime PRIVATE ${Rime_LIBRARIES})
  else()
    message(FATAL_ERROR "Can not found rime")
  endif()
endif()


### Compile options


if ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  set(CMAKE_C_FLAGS_DEBUG "-fuse-ld=gold -fsanitize=address,leak,undefined -fno-omit-frame-pointer -pg -g")
endif()

### Sources

target_sources(erime PRIVATE
  src/interface.c
  src/liberime.c
  src/init.c)

