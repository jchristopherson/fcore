! test_fcore_dictionary.f90

module test_fcore_dictionary
    use iso_fortran_env
    use collections
    implicit none
contains
! ------------------------------------------------------------------------------
    function test_dictionary_1() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: str1 = "Test String 1"
        character(len = *), parameter :: str2 = "Test String 2"
        character(len = *), parameter :: str3 = "Test String 3"

        ! Local Variables
        integer(int64) :: key1, key2, key3
        type(hash_code) :: hash
        type(dictionary) :: x
        class(*), pointer :: item
        logical :: temp

        ! Initialization
        rst = .true.

        ! Generate hash values
        key1 = hash%get(str1)
        key2 = hash%get(str2)
        key3 = hash%get(str3)

        ! Add the items to the dictionary
        call x%add(key1, str1)
        call x%add(key2, str2)
        call x%add(key3, str3)

        ! Check the size
        if (x%get_count() /= 3) then
            rst = .false.
            print '(AI0A)', "TEST_DICTIONARY_1 (Test 1): Expected 3 items " // &
                "in the dictionary, but found ", x%get_count(), "."
            return
        end if

        ! Check to ensure each key exists in the collection
        if (.not.x%contains_key(key1)) then
            rst = .false.
            print '(A)', "TEST_DICTIONARY_1 (Test 2): " // &
                "Expected to find key 1, but did not."
            return
        end if

        if (.not.x%contains_key(key2)) then
            rst = .false.
            print '(A)', "TEST_DICTIONARY_1 (Test 3): " // &
                "Expected to find key 2, but did not."
            return
        end if

        if (.not.x%contains_key(key3)) then
            rst = .false.
            print '(A)', "TEST_DICTIONARY_1 (Test 4): " // &
                "Expected to find key 3, but did not."
            return
        end if

        ! Check with an invalid key
        if (x%contains_key(key2 - key1) .and. key2 - key1 /= key3) then
            rst = .false.
            print '(A)', "TEST_DICTIONARY_1 (Test 5): " // &
                "Did not expect to find an invalid key, but did."
            return
        end if

        ! Retrieve an item and check
        item => x%get(key2)
        select type (item)
            type is (character(len = *))
                if (item /= str2) then
                    rst = .false.
                    print '(A)', "TEST_DICTIONARY_1 (Test 6): " // &
                        "Expected to find " // str2 // ", but found " // &
                        item // " instead."
                end if
        end select

        item => x%get(key3)
        select type (item)
            type is (character(len = *))
                if (item /= str3) then
                    rst = .false.
                    print '(A)', "TEST_DICTIONARY_1 (Test 7): " // &
                        "Expected to find " // str3 // ", but found " // &
                        item // " instead."
                end if
        end select

        ! Remove the second item (key2)
        temp = x%remove(key2)
        if (.not.temp) then
            rst = .false.
            print '(A)', "TEST_DICTIONARY_1 (Test 8): Failed to remove " // &
                "an existing item."
        end if
        if (x%contains_key(key2)) then
            rst = .false.
            print '(A)', "TEST_DICTIONARY_1 (Test 9): Failed to remove " // &
                "an existing item."
        end if
    end function

! ------------------------------------------------------------------------------
end module
