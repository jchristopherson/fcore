! test_fcore_strings.f90

module test_fcore_strings
    use iso_fortran_env
    use strings
    implicit none

contains
! ------------------------------------------------------------------------------
    function test_index_of_all_1() result(rst)
        ! Local Variables
        logical :: rst
        character(len = *), parameter :: test_string = "A fine and sunny day."
        integer(int32), allocatable, dimension(:) :: indices

        ! Initialization
        rst = .true.

        ! Find all lower case letters "a"
        ! indices: 8, 19
        indices = index_of_all(test_string, "a")
        if (size(indices) /= 2) then
            rst = .false.
            print '(AI0A)', "TEST_INDEX_OF_ALL_1 (Test #1): Expected " // &
                "an index array of length 2, but found an array of length ", &
                size(indices), "."
        end if
        if (indices(1) /= 8) then
            rst = .false.
            print '(AI0A)', "TEST_INDEX_OF_ALL_1 (Test #1): Expected to " // &
                "find an index of 8, but found ", indices(1), "."
        end if
        if (indices(2) /= 19) then
            rst = .false.
            print '(AI0A)', "TEST_INDEX_OF_ALL_1 (Test #1): Expected to " // &
                "find an index of 19, but found ", indices(1), "."
        end if

        ! Find the word "and"
        ! index: 8
        indices = index_of_all(test_string, "and")
        if (size(indices) /= 1) then
            rst = .false.
            print '(AI0A)', "TEST_INDEX_OF_ALL_1 (Test #2): Expected " // &
                "an index array of length 1, but found an array of length ", &
                size(indices), "."
        end if
        if (indices(1) /= 8) then
            rst = .false.
            print '(AI0A)', "TEST_INDEX_OF_ALL_1 (Test #2): Expected to " // &
                "find an index of 8, but found ", indices(1), "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_split_string_1() result(rst)
        ! Variables
        logical :: rst
        character(len = *), parameter :: test_string = &
            "A,test string,,with spaces,and empty,,delimiters"
        type(string), allocatable, dimension(:) :: items

        ! Initialization
        rst = .true.

        ! Process
        items = split_string(test_string, ",")

        if (size(items) /= 7) then
            rst = .false.
            print '(AI0A)', &
                "TEST_SPLIT_STRING_1 (Test #1): Expected 7 items, but found ", &
                size(items), "."
        end if
        if (items(1)%str /= "A") then
            rst = .false.
            print '(A)', "TEST_SPLIT_STRING_1 (Test #1): Found " // items(1)%str
        end if

        if (items(2)%str /= "test string") then
            rst = .false.
            print '(A)', "TEST_SPLIT_STRING_1 (Test #2): Found " // items(2)%str
        end if

        if (items(3)%str /= "") then
            rst = .false.
            print '(A)', "TEST_SPLIT_STRING_1 (Test #3): Found " // items(3)%str
        end if

        if (items(4)%str /= "with spaces") then
            rst = .false.
            print '(A)', "TEST_SPLIT_STRING_1 (Test #4): Found " // items(4)%str
        end if

        if (items(5)%str /= "and empty") then
            rst = .false.
            print '(A)', "TEST_SPLIT_STRING_1 (Test #5): Found " // items(5)%str
        end if

        if (items(6)%str /= "") then
            rst = .false.
            print '(A)', "TEST_SPLIT_STRING_1 (Test #6): Found " // items(6)%str
        end if

        if (items(7)%str /= "delimiters") then
            rst = .false.
            print '(A)', "TEST_SPLIT_STRING_1 (Test #7): Found " // items(7)%str
        end if
    end function

! ------------------------------------------------------------------------------
end module
