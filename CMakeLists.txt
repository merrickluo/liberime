cmake_minimum_required(VERSION 3.0)

enable_language(C)
project(erime VERSION 0.1.0 DESCRIPTION "librime bindings for emacs")

set(CMAKE_POSITION_INDEPENDENT_CODE TRUE CACHE BOOL "pic" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "shared" FORCE)
set(BUILD_CLAR OFF CACHE BOOL "clar" FORCE)

find_library(rime librime.so)

add_library(erime SHARED
  src/emacs-module-helpers.c
  src/emacs-rime-bindings.c
  src/emacs-rime.c)
set_target_properties(erime PROPERTIES C_STANDARD 99)

target_link_libraries(erime rime)

# Emacs looks for .so on linux and OSX.
# By default CMake builds .dylib on OSX, so override that.
if(APPLE)
  set_target_properties(erime PROPERTIES SUFFIX .so)
endif(APPLE)

if(WIN32)
  set_target_properties(erime PROPERTIES PREFIX lib)
endif(WIN32)
