! test_fcore_list.f90

module test_fcore_list
    use iso_fortran_env
    use collections
    implicit none

contains
! ------------------------------------------------------------------------------
    function test_list_1() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        integer(int32), parameter :: list_size = 100
        integer(int32), parameter :: insert_index = 20
        integer(int32), parameter :: insert_value = 500

        ! Local Variables
        type(list) :: x
        integer(int32) :: i, ans
        class(*), pointer :: ptr

        ! Initialization
        rst = .true.

        ! Store a series of integers
        do i = 1, list_size
            call x%push(i)
        end do

        ! Check the count
        if (x%get_count() /= list_size) then
            rst = .false.
            print '(AI0AI0A)', "TEST_LIST_1 (Test 1): Expected: ", list_size, &
                ", but found: ", x%get_count(), "."
        end if

        ! Check the content
        do i = 1, x%get_count()
            ptr => x%get(i)
            select type (ptr)
            type is (integer(int32))
                if (ptr /= i) then
                    rst = .false.
                    print '(AI0AI0A)', "TEST_LIST_1 (Test 2): Expected: ", &
                        i, ", but found: ", ptr, "."
                end if
            end select
        end do

        ! Insert an item
        call x%insert(insert_index, insert_value)

        ! Check the size and contents
        if (x%get_count() /= list_size + 1) then
            rst = .false.
            print '(AI0AI0A)', "TEST_LIST_1 (Test 3): Expected: ", &
                list_size + 1, ", but found: ", x%get_count(), "."
        end if
        do i = 1, x%get_count()
            ptr => x%get(i)
            select type (ptr)
            type is (integer(int32))
                if (i < insert_index) then
                    ans = i
                else if (i == insert_index) then
                    ans = insert_value
                else
                    ans = i - 1
                end if
                if (ptr /= ans) then
                    rst = .false.
                    print '(AI0AI0A)', "TEST_LIST_1 (Test 4): Expected: ", &
                        ans, ", but found: ", ptr, "."
                end if
            end select
        end do

        ! Remove an item
        call x%remove(insert_index)

        ! Check the size & contents
        if (x%get_count() /= list_size) then
            rst = .false.
            print '(AI0AI0A)', "TEST_LIST_1 (Test 5): Expected: ", list_size, &
                ", but found: ", x%get_count(), "."
        end if

        do i = 1, x%get_count()
            ptr => x%get(i)
            select type (ptr)
            type is (integer(int32))
                if (ptr /= i) then
                    rst = .false.
                    print '(AI0AI0A)', "TEST_LIST_1 (Test 6): Expected: ", &
                        i, ", but found: ", ptr, "."
                end if
            end select
        end do

        ! Clear the list
        call x%clear()

        ! Check the size
        if (x%get_count() /= 0) then
            rst = .false.
            print '(AI0AI0A)', "TEST_LIST_1 (Test 5): Expected: ", 0, &
                ", but found: ", x%get_count(), "."
        end if
    end function

! ------------------------------------------------------------------------------
end module
