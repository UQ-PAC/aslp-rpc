cmake_minimum_required(VERSION 3.14)

include(cmake/prelude.cmake)

project(
    aslp-cpp
    VERSION 0.1.0
    DESCRIPTION "Short description"
    HOMEPAGE_URL "https://github.com/UQ-PAC/aslp"
    LANGUAGES CXX
)

include(cmake/project-is-top-level.cmake)
include(cmake/variables.cmake)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# ---- Declare library ----

add_library(
    aslp-cpp_aslp-cpp
    source/aslp-cpp.cpp
)
add_library(aslp-cpp::aslp-cpp ALIAS aslp-cpp_aslp-cpp)


include(cmake/CPM.cmake)
CPMAddPackage("gh:fmtlib/fmt#10.2.1")
CPMAddPackage("gh:nlohmann/json@3.11.3")

include(GenerateExportHeader)
generate_export_header(
    aslp-cpp_aslp-cpp
    BASE_NAME aslp-cpp
    EXPORT_FILE_NAME export/aslp-cpp/aslp-cpp_export.hpp
    CUSTOM_CONTENT_FROM_VARIABLE pragma_suppress_c4251
)

if(NOT BUILD_SHARED_LIBS)
  target_compile_definitions(aslp-cpp_aslp-cpp PUBLIC ASLP_CPP_STATIC_DEFINE)
endif()

set_target_properties(
    aslp-cpp_aslp-cpp PROPERTIES
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN YES
    VERSION "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}"
    EXPORT_NAME aslp-cpp
    OUTPUT_NAME aslp-cpp
)

target_include_directories(
    aslp-cpp_aslp-cpp ${warning_guard}
    PUBLIC
    "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>"
)

target_include_directories(
    aslp-cpp_aslp-cpp SYSTEM
    PUBLIC
    "$<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/export>"
)


target_include_directories(
    aslp-cpp_aslp-cpp PRIVATE lib/
    )

target_compile_features(aslp-cpp_aslp-cpp PUBLIC cxx_std_20)

# ---- Install rules ----

if(NOT CMAKE_SKIP_INSTALL_RULES)
  include(cmake/install-rules.cmake)
endif()

# ---- Examples ----

add_subdirectory(test)

# ---- Developer mode ----

if(NOT aslp-cpp_DEVELOPER_MODE)
  return()
elseif(NOT PROJECT_IS_TOP_LEVEL)
  message(
      AUTHOR_WARNING
      "Developer mode is intended for developers of aslp-cpp"
  )
endif()

include(cmake/dev-mode.cmake)
