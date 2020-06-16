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
    function test_regex_search() result(rst)
        ! Variables
        logical :: rst
        character(len = *), parameter :: test_string = &
            "This is a string with some numbers 0.1234"
        
        type(string), allocatable, dimension(:) :: matches

        ! Initialization
        rst = .true.

        ! Look for the numeric values
        matches = regex_search(test_string, "\d.+")

        if (size(matches) /= 1) then
            rst = .false.
            print '(AI0A)', &
                "TEST_REGEX_SEARCH (Test 1a): Expected: 1, but found: ", &
                size(matches), "."
            return
        end if
        
        if (matches(1) /= "0.1234") then
            rst = .false.
            print '(A)', &
                "TEST_REGEX_SEARCH (Test 1b): Expected: 0.1234, but found: " &
                // matches(1)%str // "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_regex_replace() result(rst)
        ! Variables
        logical :: rst
        character(len = *), parameter :: test_string = &
            "This is a string with some numbers 0.1234"
        
        character(len = :), allocatable :: test1

        ! Initialization
        rst = .true.

        ! Replace the non-numeric characters with empty characters such that
        ! only numbers are left
        test1 = regex_replace(test_string, "[^0-9]+", "")
        
        if (test1 /= "01234") then
            rst = .false.
            print '(A)', &
                "TEST_REGEX_REPLACE (Test 1): Expected: 01234, but found: " &
                // test1 // "."
        end if
    end function

! ------------------------------------------------------------------------------
end module
