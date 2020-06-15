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
    function test_replace_string_1() result(rst)
        ! Variables
        logical :: rst
        character(len = *), parameter :: test_string = &
            "This is just a test string."
        character(len = *), parameter :: answer1 = &
            "This_is_just_a_test_string."
        character(len = *), parameter :: answer2 = &
            "Thiss iss jusst a tesst sstring."
        character(len = *), parameter :: answer3 = &
            "This is just a test string!"
        character(len = *), parameter :: answer4 = &
            "tis is just a test string."
        
        character(len = :), allocatable :: test1, test2, test3, test4

        ! Initialization
        rst = .true.

        ! Replace all spaces with underscores
        test1 = replace(test_string, " ", "_")
        if (test1 /= answer1) then
            rst = .false.
            print '(A)', "TEST_REPLACE_STRING_1 (Test #1): Expected: " // &
                answer1 // ", but found: " // test1 // "."
        end if

        ! Replace all 's' with 'ss'
        test2 = replace(test_string, "s", "ss")
        if (test2 /= answer2) then
            rst = .false.
            print '(A)', "TEST_REPLACE_STRING_1 (Test #2): Expected: " // &
                answer2 // ", but found: " // test2 // "."
        end if

        ! Replace the '.' with a '!'
        test3 = replace(test_string, ".", "!")
        if (test3 /= answer3) then
            rst = .false.
            print '(A)', "TEST_REPLACE_STRING_1 (Test #3): Expected: " // &
                answer3 // ", but found: " // test3 // "."
        end if

        ! Replace the 'Th' with a 'tH'
        test4 = replace(test_string, "Th", "t")
        if (test4 /= answer4) then
            rst = .false.
            print '(A)', "TEST_REPLACE_STRING_1 (Test #4): Expected: " // &
                answer4 // ", but found: " // test4 // "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_remove_string_1() result(rst)
        ! Variables
        logical :: rst
        character(len = *), parameter :: test_string = &
            "This is just a test string."
        character(len = *), parameter :: answer1 = &
            "Thi i jut a tet tring."
        character(len = *), parameter :: answer2 = &
            "his is just a test string."
        character(len = *), parameter :: answer3 = &
            "This is just a test string"
        
        character(len = :), allocatable :: test1, test2, test3

        ! Initialization
        rst = .true.

        ! Remove all 's' characters
        test1 = remove(test_string, "s")
        if (test1 /= answer1) then
            rst = .false.
            print '(A)', "TEST_REMOVE_STRING_1 (Test #1): Expected: " // &
                answer1 // ", but found: " // test1 // "."
        end if

        ! Remove the first character
        test2 = remove(test_string, "T")
        if (test2 /= answer2) then
            rst = .false.
            print '(A)', "TEST_REMOVE_STRING_1 (Test #2): Expected: " // &
                answer2 // ", but found: " // test2 // "."
        end if

        ! Remove the last character
        test3 = remove(test_string, ".")
        if (test3 /= answer3) then
            rst = .false.
            print '(A)', "TEST_REMOVE_STRING_1 (Test #3): Expected: " // &
                answer3 // ", but found: " // test3 // "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_remove_at_string_1() result(rst)
        ! Variables
        logical :: rst
        character(len = *), parameter :: test_string = &
            "This is just a test string."
        character(len = *), parameter :: answer1 = &
            "This is just a test."
        character(len = *), parameter :: answer2 = &
            "is just a test string."
        character(len = *), parameter :: answer3 = &
            "This is just a test"

        character(len = :), allocatable :: test1, test2, test3

        ! Initialization
        rst = .true.

        ! Test 1
        test1 = remove_at(test_string, 20, 7)
        if (test1 /= answer1) then
            rst = .false.
            print '(A)', "TEST_REMOVE_AT_STRING_1 (Test #1): Expected: " // &
                answer1 // ", but found: " // test1 // "."
        end if

        ! Test 2
        test2 = remove_at(test_string, 1, 5)
        if (test2 /= answer2) then
            rst = .false.
            print '(A)', "TEST_REMOVE_AT_STRING_1 (Test #2): Expected: " // &
                answer2 // ", but found: " // test2 // "."
        end if

        ! Test 3
        test3 = remove_at(test_string, 20, 8)
        if (test3 /= answer3) then
            rst = .false.
            print '(A)', "TEST_REMOVE_AT_STRING_1 (Test #3): Expected: " // &
                answer3 // ", but found: " // test3 // "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_to_upper_1() result(rst)
        ! Variables
        logical :: rst
        character(len = *), parameter :: test_string = &
            "This is just a test string."
        character(len = *), parameter :: answer1 = &
            "THIS IS JUST A TEST STRING."
        
        character(len = :), allocatable :: test1

        ! Initialization
        rst = .true.

        ! Test
        test1 = to_upper(test_string)
        if (test1 /= answer1) then
            rst = .false.
            print '(A)', "TEST_TO_UPPER_1 (Test #1): Expected: " // &
                answer1 // ", but found: " // test1 // "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_to_lower_1() result(rst)
        ! Variables
        logical :: rst
        character(len = *), parameter :: test_string = &
            "This is just a test string."
        character(len = *), parameter :: answer1 = &
            "this is just a test string."
        
        character(len = :), allocatable :: test1

        ! Initialization
        rst = .true.

        ! Test
        test1 = to_lower(test_string)
        if (test1 /= answer1) then
            rst = .false.
            print '(A)', "TEST_TO_LOWER_1 (Test #1): Expected: " // &
                answer1 // ", but found: " // test1 // "."
        end if
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
