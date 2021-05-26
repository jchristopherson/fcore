# Define the FCORE source files
set(fcore_sources
    strings.f90
    strings_string_builder.f90
    strings_ops.f90
    regular_expressions.f90
    regular_expressions.cpp
    fcore_constants.f90
    collections.f90
    collections_list.f90
    file_io.f90
    file_io_fm.f90
    file_io_text.f90
    file_io_reader.f90
    file_io_endian.f90
    file_io_binary.f90
    system_file_io.cpp
    file_io_path.f90
    ui_dialogs.cpp
    ui_dialogs.f90
    collections_dictionary.f90
    collections_hash.f90
    collections_linked_list.f90
)

# Build the library
add_library(fcore ${fcore_sources})
target_link_libraries(fcore 
    ${ferror_LIBRARIES}
)

# ------------------------------------------------------------------------------
# INSTALLATION INSTRUCTIONS
# ------------------------------------------------------------------------------
# Define target information
set_property(TARGET fcore PROPERTY VERSION ${FCORE_VERSION})
set_property(TARGET fcore PROPERTY SOVERSION ${FCORE_MAJOR_VERSION})
set_property(TARGET fcore PROPERTY INTERFACE_fcore_MAJOR_VERSION ${FCORE_MAJOR_VERSION})
set_property(TARGET fcore APPEND PROPERTY COMPATIBLE_INTERFACE_STRING fcore_MAJOR_VERSION)

# Locate the "include" directories
set(fcore_INCLUDE_DIRS ${PROJECT_SOURCE_DIR}/include)

# Define the installation instructions
install(TARGETS fcore EXPORT fcoreTargets
    RUNTIME DESTINATION fcore/bin
    LIBRARY DESTINATION fcore/lib
    ARCHIVE DESTINATION fcore/lib
    CONFIGURATIONS Release
    INCLUDES DESTINATION fcore/include
)
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/include
    DESTINATION ${CMAKE_INSTALL_PREFIX}/fcore
    COMPONENT Devel
)

# Include the documentation
install(DIRECTORY ${PROJECT_SOURCE_DIR}/doc/html DESTINATION fcore/doc)

# Define the version file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_BINARY_DIR}/fcoreConfigVersion.cmake"
    VERSION ${FCORE_VERSION}
    COMPATIBILITY AnyNewerVersion
)

export(EXPORT fcoreTargets
    FILE "${CMAKE_BINARY_DIR}/fcoreTargets.cmake"
)

# Define the configuration file
configure_file(
    "${PROJECT_SOURCE_DIR}/fcoreConfig.cmake.in"
    "${CMAKE_BINARY_DIR}/fcoreConfig.cmake"
    COPYONLY
)

set(ConfigPackageLocation fcore/lib/cmake/fcore)
install(
    EXPORT fcoreTargets
    FILE fcoreTargets.cmake
    DESTINATION ${ConfigPackageLocation}
)
install(
    FILES
        "${CMAKE_BINARY_DIR}/fcoreConfig.cmake"
        "${CMAKE_BINARY_DIR}/fcoreConfigVersion.cmake"
    DESTINATION
        ${ConfigPackageLocation}
    COMPONENT
        Devel
)
