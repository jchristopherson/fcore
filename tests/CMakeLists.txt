set(test_sources
    test_fcore.f90
    test_fcore_strings.f90
    test_fcore_regex.f90
    test_fcore_list.f90
)

# Build the executable
add_executable(fcore_tests ${test_sources})
target_link_libraries(fcore_tests fcore)

# Run the tests
add_custom_command(
    OUTPUT fcore_tests_output
    DEPENDS ${test_sources}
    COMMAND fcore_tests
)
add_custom_target(
    run_fcore_tests ALL
    DEPENDS fcore_tests_output
)
