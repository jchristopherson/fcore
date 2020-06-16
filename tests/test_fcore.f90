! test_fcore.f90

program main
    use iso_fortran_env
    use test_fcore_strings
    use test_fcore_regex
    implicit none

    ! Local Variables
    logical :: local, overall

    ! Process
    overall = .true.

    ! Testing
    local = test_index_of_all_1()
    if (.not.local) overall = .false.

    local = test_split_string_1()
    if (.not.local) overall = .false.

    local = test_replace_string_1()
    if (.not.local) overall = .false.

    local = test_remove_string_1()
    if (.not.local) overall = .false.

    local = test_remove_at_string_1()
    if (.not.local) overall = .false.

    local = test_to_upper_1()
    if (.not.local) overall = .false.

    local = test_to_lower_1()
    if (.not.local) overall = .false.

    local = test_regex_match()
    if (.not.local) overall = .false.

    ! End
    if (overall) then
        print '(A)', "FCORE: ALL TESTS PASSED"
    else
        print '(A)', "FCORE: TESTS FAILED."
    end if
end program
