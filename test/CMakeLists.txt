cmake_minimum_required(VERSION 3.14)

project(aslp-cppTests LANGUAGES CXX)

include(../cmake/project-is-top-level.cmake)
include(../cmake/folders.cmake)

# ---- Dependencies ----

if(PROJECT_IS_TOP_LEVEL)
  find_package(aslp-cpp REQUIRED)
  enable_testing()
endif()

# ---- Tests ----

add_executable(aslp-cpp_test source/aslp-cpp_test.cpp)
target_link_libraries(aslp-cpp_test PRIVATE aslp-cpp::aslp-cpp)
target_compile_features(aslp-cpp_test PRIVATE cxx_std_20)

add_test(NAME aslp-cpp_test COMMAND aslp-cpp_test)

# ---- End-of-file commands ----

add_folders(Test)
