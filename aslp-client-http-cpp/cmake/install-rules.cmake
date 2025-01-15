if(PROJECT_IS_TOP_LEVEL)
  set(
      CMAKE_INSTALL_INCLUDEDIR "include/aslp-cpp-${PROJECT_VERSION}"
      CACHE STRING ""
  )
  set_property(CACHE CMAKE_INSTALL_INCLUDEDIR PROPERTY TYPE PATH)
endif()

include(CMakePackageConfigHelpers)
include(GNUInstallDirs)

# find_package(<package>) call for consumers to find this project
set(package aslp-cpp)

install(
    DIRECTORY
    include/
    "${PROJECT_BINARY_DIR}/export/"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
    COMPONENT aslp-cpp_Development
)

install(
    TARGETS aslp-cpp_aslp-cpp
    EXPORT aslp-cppTargets
    RUNTIME #
    COMPONENT aslp-cpp_Runtime
    LIBRARY #
    COMPONENT aslp-cpp_Runtime
    NAMELINK_COMPONENT aslp-cpp_Development
    ARCHIVE #
    COMPONENT aslp-cpp_Development
    INCLUDES #
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)

write_basic_package_version_file(
    "${package}ConfigVersion.cmake"
    COMPATIBILITY SameMajorVersion
)

# Allow package maintainers to freely override the path for the configs
set(
    aslp-cpp_INSTALL_CMAKEDIR "${CMAKE_INSTALL_LIBDIR}/cmake/${package}"
    CACHE STRING "CMake package config location relative to the install prefix"
)
set_property(CACHE aslp-cpp_INSTALL_CMAKEDIR PROPERTY TYPE PATH)
mark_as_advanced(aslp-cpp_INSTALL_CMAKEDIR)

install(
    FILES cmake/install-config.cmake
    DESTINATION "${aslp-cpp_INSTALL_CMAKEDIR}"
    RENAME "${package}Config.cmake"
    COMPONENT aslp-cpp_Development
)

install(
    FILES "${PROJECT_BINARY_DIR}/${package}ConfigVersion.cmake"
    DESTINATION "${aslp-cpp_INSTALL_CMAKEDIR}"
    COMPONENT aslp-cpp_Development
)

install(
    EXPORT aslp-cppTargets
    NAMESPACE aslp-cpp::
    DESTINATION "${aslp-cpp_INSTALL_CMAKEDIR}"
    COMPONENT aslp-cpp_Development
)

if(PROJECT_IS_TOP_LEVEL)
  include(CPack)
endif()
