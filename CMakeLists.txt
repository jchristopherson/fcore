# Master CMAKE Build Script
cmake_minimum_required(VERSION 3.7)
project(fcore C CXX Fortran)

# Define version information
set(FCORE_MAJOR_VERSION 1)
set(FCORE_MINOR_VERSION 1)
set(FCORE_PATCH_VERSION 0)
set(FCORE_VERSION ${FCORE_MAJOR_VERSION}.${FCORE_MINOR_VERSION}.${FCORE_PATCH_VERSION})

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Debug' as none was specified.")
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()

# By default, shared library
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)

# Get compiler info
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

# Export all symbols on Windows when building shared libraries
SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

# Locate the module files
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/include)

# Define output directories, if undefined
if (NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
    message(STATUS "MEASUREMENTS library output directories undefined.  Using default directories.")
    if (CMAKE_BUILD_TYPE MATCHES Debug)
        # Debug Build
        if (BUILD_SHARED_LIBS)
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
        else()
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
        endif()
    elseif (CMAKE_BUILD_TYPE MATCHES Release)
        # Release Build
        if (BUILD_SHARED_LIBS)
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
        else()
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
        endif()
    else()
        # Default Condition
        if (BUILD_SHARED_LIBS)
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
        else()
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
        endif()
    endif()
endif()

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set(CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O0 -Wl,--allow-multiple-definition")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -Wall -Wno-c-binding-type -Wl,--allow-multiple-definition")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O0")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O0 -Wl,--allow-multiple-definition")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -Wall -Wl,--allow-multiple-definition")
endif ()
message(STATUS "CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
message(STATUS "Fortran compiler: " ${Fortran_COMPILER_NAME})

# C/C++ Compiler Directions - Windows Specific
# Necessary for COM support => REF: https://gist.github.com/armornick/106e1124995194af5c02
set(CMAKE_CXX_FLAGS_DEBUG "-DUNICODE -D_UNICODE -D_WIN32_WINNT=_WIN32_WINNT_VISTA -DNTDDI_VERSION=NTDDI_VISTA -Wl,--allow-multiple-definition")
set(CMAKE_CXX_FLAGS_RELEASE "-DUNICODE -D_UNICODE -D_WIN32_WINNT=_WIN32_WINNT_VISTA -DNTDDI_VERSION=NTDDI_VISTA -Wl,--allow-multiple-definition")


# Locate Dependencies
find_package(ferror 1.3.0)

if (ferror_FOUND)
  message(STATUS "FERROR library found.")
  set(ferror_LIBRARIES ferror)
endif()

# Locate the include directory
include_directories(${PROJECT_SOURCE_DIR}/include)

# Locate the source directory
add_subdirectory(src)

# ------------------------------------------------------------------------------
# EXAMPLES
# ------------------------------------------------------------------------------
option(BUILD_FCORE_EXAMPLES "Build FCORE examples?" OFF)
if (BUILD_FCORE_EXAMPLES)
  # Inform the user we're building the examples
  message(STATUS "Building FCORE examples.")

  # Build the examples
  add_subdirectory(examples)
endif()

# ------------------------------------------------------------------------------
# TESTS
# ------------------------------------------------------------------------------
option(BUILD_FCORE_TESTS "Build FCORE tests?" OFF)
if (BUILD_FCORE_TESTS)
    # Inform the user we're building the tests
    message(STATUS "Building FCORE tests.")

    # Build the tests
    add_subdirectory(tests)
endif()
