! test_fcore_regex.f90

module test_fcore_regex
    use iso_fortran_env
    use strings
    use regular_expressions
    implicit none
contains
! ------------------------------------------------------------------------------
    function test_regex_match() result(rst)
        ! Variables
        logical :: rst
        character(len = *), parameter :: test_string = &
            "subject"
        character(len = *), parameter :: pattern = "(sub)(.*)"

        type(string), allocatable, dimension(:) :: matches

        ! Initialization
        rst = .true.

        ! Test 1
        matches = regex_match(test_string, pattern)

        if (size(matches) /= 3) then
            rst = .false.
            print '(AI0A)', &
                "TEST_REGEX_MATCH (Test 1a): Expected: 3, but found: ", &
                size(matches), "."
            return
        end if
        
        if (matches(1) /= "subject") then
            rst = .false.
            print '(A)', &
                "TEST_REGEX_MATCH (Test 1b): Expected: subject, but found: " &
                // matches(1)%str // "."
        end if

        if (matches(2) /= "sub") then
            rst = .false.
            print '(A)', &
                "TEST_REGEX_MATCH (Test 1c): Expected: sub, but found: " &
                // matches(1)%str // "."
        end if

        if (matches(3) /= "ject") then
            rst = .false.
            print '(A)', &
                "TEST_REGEX_MATCH (Test 1d): Expected: ject, but found: " &
                // matches(1)%str // "."
        end if
    end function

! ------------------------------------------------------------------------------
end module
