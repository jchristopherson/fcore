! test_fcore_list.f90

module test_fcore_list
    use iso_fortran_env
    use collections
    implicit none

contains
! ------------------------------------------------------------------------------
    function compare_ints(item1, item2) result(rst)
        class(*), intent(in) :: item1, item2
        logical :: rst

        integer(int32) :: i1, i2

        i1 = 0
        i2 = 1

        select type (item1)
        type is (integer(int32))
            i1 = item1
        end select

        select type (item2)
        type is (integer(int32))
            i2 = item2
        end select

        rst = i1 == i2
    end function

! --------------------
    function test_ints(item1, item2) result(rst)
        class(*), intent(in) :: item1, item2
        integer(int32) :: rst

        integer(int32) :: i1, i2

        i1 = 0
        i2 = 1

        select type (item1)
        type is (integer(int32))
            i1 = item1
        end select

        select type (item2)
        type is (integer(int32))
            i2 = item2
        end select

        if (i1 > i2) then
            rst = 1
        else if (i1 < i2) then
            rst = -1
        else
            rst = 0
        end if
    end function

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
        procedure(items_equal), pointer :: fcn
        procedure(compare_items), pointer :: cfcn

        ! Initialization
        rst = .true.
        fcn => compare_ints
        cfcn => test_ints

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

        ! See if the collection contains the item
        if (.not.x%contains(insert_value, fcn)) then
            rst = .false.
            print '(A)', "TEST_LIST_1 (Test 5): Could not find a value " // &
                "known to exist in the collection."
        end if

        ! Remove an item
        call x%remove(insert_index)

        ! Check the size & contents
        if (x%get_count() /= list_size) then
            rst = .false.
            print '(AI0AI0A)', "TEST_LIST_1 (Test 6): Expected: ", list_size, &
                ", but found: ", x%get_count(), "."
        end if

        do i = 1, x%get_count()
            ptr => x%get(i)
            select type (ptr)
            type is (integer(int32))
                if (ptr /= i) then
                    rst = .false.
                    print '(AI0AI0A)', "TEST_LIST_1 (Test 7): Expected: ", &
                        i, ", but found: ", ptr, "."
                end if
            end select
        end do

        ! Reverse the contents
        call x%reverse()

        do i = 1, x%get_count()
            ptr => x%get(i)
            select type (ptr)
            type is (integer(int32))
                if (ptr /= x%get_count() - i + 1) then
                    rst = .false.
                    print '(AI0AI0A)', "TEST_LIST_1 (Test 8): Expected: ", &
                        x%get_count() - i + 1, ", but found: ", ptr, "."
                end if
            end select
        end do

        ! Sort into ascending order
        call x%sort(cfcn)
        do i = 1, x%get_count()
            ptr => x%get(i)
            select type (ptr)
            type is (integer(int32))
                if (ptr /= i) then
                    rst = .false.
                    print '(AI0AI0A)', "TEST_LIST_1 (Test 9): Expected: ", &
                        i, ", but found: ", ptr, "."
                end if
            end select
        end do

        ! Clear the list
        call x%clear()

        ! Check the size
        if (x%get_count() /= 0) then
            rst = .false.
            print '(AI0AI0A)', "TEST_LIST_1 (Test 10): Expected: ", 0, &
                ", but found: ", x%get_count(), "."
        end if
    end function

! ------------------------------------------------------------------------------
    function test_linked_list_1() result(rst)
        ! Arbuments
        logical :: rst

        ! Parameters
        integer(int32), parameter :: list_size = 100

        ! Local Variables
        type(linked_list) :: x
        integer(int32) :: i
        logical :: check
        class(*), pointer :: ptr
        procedure(items_equal), pointer :: fcn

        ! Initialization
        rst = .true.
        fcn => compare_ints

        ! Store a series of integers
        do i = 1, list_size
            call x%push(i)
        end do

        ! Check the count
        if (x%get_count() /= list_size) then
            rst = .false.
            print '(AI0AI0A)', "TEST_LINKED_LIST_1 (Test 1); Expected: ", &
                list_size, ", but found: ", x%get_count(), "."
        end if

        ! Check the content - ascending
        check = x%move_to_first()
        i = 0
        do while (check)
            i = i + 1
            ptr => x%get()
            select type (ptr)
            type is (integer(int32))
                if (ptr /= i) then
                    rst = .false.
                    print '(AI0AI0A)', &
                        "TEST_LINKED_LIST_1 (Test 2); Expected: ", i, &
                        ", but found: ", ptr, "."
                end if
            end select

            check = x%move_to_next()
        end do

        ! Check the content - descending
        check = x%move_to_last()
        i = list_size + 1
        do while (check)
            i = i - 1
            ptr => x%get()
            select type (ptr)
            type is (integer(int32))
                if (ptr /= i) then
                    rst = .false.
                    print '(AI0AI0A)', &
                        "TEST_LINKED_LIST_1 (Test 3); Expected: ", i, &
                        ", but found: ", ptr, "."
                end if
            end select

            check = x%move_to_previous()
        end do

        ! Check the contains routine
        if (.not.x%contains(list_size / 2, fcn)) then
            rst = .false.
            print '(A)', "TEST_LINKED_LIST_1 (Test 4): Could not find a " // &
                "value known to exist in the collection."
        end if
    end function

! ------------------------------------------------------------------------------
end module
