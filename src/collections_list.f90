! collections_list.f90

submodule (collections) collections_list
    use fcore_constants

! ******************************************************************************
! CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief The default buffer size.
    integer(int32), parameter :: DEFAULT_BUFFER_SIZE = 10

contains
! ------------------------------------------------------------------------------
    !> @brief Gets the capacity of the list.
    !!
    !! @param[in] this The list object.
    !! @return The capacity of the list.
    pure module function list_get_capacity(this) result(rst)
        class(list), intent(in) :: this
        integer(int32) :: rst
        if (allocated(this%m_list)) then
            rst = size(this%m_list)
        else
            rst = 0
        end if
    end function

! --------------------
    !> @brief Sets the capacity of the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] n The desired capacity of the list.  This value must not be
    !!  less than the number of items already stored in the list.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
    !!  - FCORE_INVALID_INPUT_ERROR: Occurs if the user asks to reduce capacity
    !!      via this routine.
    module subroutine list_set_capacity(this, n, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag, m
        type(container), allocatable, dimension(:) :: copy
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        m = this%get_count()

        ! Input Check
        if (n < m) then
            ! ERROR: We're not into reducing capacity
            call errmgr%report_error("list_set_capacity", &
                "This routine cannot be used to reduce collection capacity.", &
                FCORE_INVALID_INPUT_ERROR)
            return
        end if

        ! Quick Return
        if (m == n) return

        ! Process
        if (allocated(this%m_list)) then
            allocate(copy(m), stat = flag)
            if (flag == 0) then
                copy = this%m_list
                deallocate(this%m_list)
                allocate(this%m_list(n), stat = flag)
                if (flag == 0) this%m_list(1:m) = copy
            end if
        else
            allocate(this%m_list(n), stat = flag)
        end if
        if (flag /= 0) then
            ! ERROR: Memory issues
            call errmgr%report_error("list_set_capacity", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the number of items in the list.
    !!
    !! @param[in] this The list object.
    !! @return The number of items stored in the list.
    pure module function list_get_count(this) result(rst)
        class(list), intent(in) :: this
        integer(int32) :: rst
        rst = this%m_count
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets an item from the list.
    !!
    !! @param[in] this The list object.
    !! @param[in] i The index of the item.
    !! @return A pointer to the requested item.
    module function list_get(this, i) result(x)
        class(list), intent(in) :: this
        integer(int32), intent(in) :: i
        class(*), pointer :: x
        type(container) :: cntr
        if (i < 1 .or. i > this%get_count()) then
            nullify(x)
        else
            cntr = this%m_list(i)
            x => cntr%item
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets an item into the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index of the item.
    !! @param[in] x The item to place into the list.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
    !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p i is out of range.
    module subroutine list_set(this, i, x, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure i is within the bounds of the array
        if (i < 1 .or. i > this%get_count()) then
            ! ERROR:
            write (errmsg, '(AI0AI0A)') "The supplied index of ", i, &
                " is outside the bounds of this collection as this " // &
                "collection has ", this%get_count(), " elements."
            call errmgr%report_error("list_set", trim(errmsg), &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Store the copy
        call this%store(i, x, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Stores an item in the collection.  If the collection isn't large
    !! enough to accomodate, it is automatically resized to accomodate.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index of the item.
    !! @param[in] x The object to store.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
    module subroutine list_store(this, i, x, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag
        class(*), pointer :: cpy, old
        type(container) :: obj
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there's capacity.  If not adjust accordingly
        if (i > this%get_capacity()) then
            call this%set_capacity(max(i, DEFAULT_BUFFER_SIZE), errmgr)
            if (errmgr%has_error_occurred()) return
        end if

        ! Clean out any existing object
        old => this%get(i)
        if (associated(old)) then
            deallocate(old)
            nullify(old)
        end if

        ! Create a copy of the item
        allocate(cpy, source = x, stat = flag)
        if (flag /= 0) then
            ! ERROR:
            call errmgr%report_error("list_store", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Store the copy
        obj%item => cpy
        this%m_list(i) = obj
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an item onto the end of the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] x The object to add to the list.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
    module subroutine list_push(this, x, err)
        ! Arguments
        class(list), intent(inout) :: this
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Determine the index of the new item
        integer(int32) :: ind
        ind = this%get_count() + 1

        ! Store the item
        call this%store(ind, x, err)
        this%m_count = this%m_count + 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pops the last item from the end of the list.
    !!
    !! @param[in,out] this The list object.
    module subroutine list_pop(this)
        ! Arguments
        class(list), intent(inout) :: this

        ! Local Variables
        class(*), pointer :: ptr
        integer(int32) :: n

        ! Process
        n = this%get_count()
        if (n > 0) then
            ptr => this%m_list(n)%item
            if (associated(ptr)) then
                deallocate(ptr)
                nullify(ptr)
            end if
            this%m_count = this%m_count - 1
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Clears the contents of the list.
    !!
    !! @param[in,out] this The list object.
    module subroutine list_clear(this)
        ! Arguments
        class(list), intent(inout) :: this

        ! Local Variables
        class(*), pointer :: ptr
        integer(int32) :: i, n

        ! Process
        n = this%get_count()
        do i = 1, n
            ptr => this%m_list(i)%item
            if (associated(ptr)) then
                deallocate(ptr)
                nullify(ptr)
            end if
        end do
        this%m_count = 0
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Creates a deep copy of the list.
    !!
    !! @param[in] this The list object.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
    module function list_copy(this, err) result(rst)
        ! Arguments
        class(list), intent(in) :: this
        class(errors), intent(inout), optional, target :: err
        type(list) :: rst

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: i, n
        class(*), pointer :: x
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Set the capacity of the new list
        n = this%get_count()
        call rst%set_capacity(n, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Populate the list
        do i = 1, n
            x => this%get(i)
            call rst%push(x, errmgr)
            if (errmgr%has_error_occurred()) return
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Inserts an item into the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index at which to insert the item.  Items at, and beyond
    !!  this index are shifted back in the list.
    !! @param[in] x The item to insert.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
    !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p i is out of range.
    module subroutine list_insert(this, i, x, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: j, n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (i < 1 .or. i > this%get_count() + 1) then
            ! ERROR:
            write (errmsg, '(AI0AI0A)') "The supplied index of ", i, &
                " is outside the bounds of this collection as this " // &
                "collection has ", this%get_count(), " elements."
            call errmgr%report_error("list_insert", trim(errmsg), &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
        end if

        ! Ensure there's sufficient capacity
        n = this%get_count()
        if (this%get_capacity() <= n + 1) then
            call this%set_capacity(n + DEFAULT_BUFFER_SIZE, errmgr)
            if (errmgr%has_error_occurred()) return
        end if

        ! Shift everything back by one element, and insert the specified item
        this%m_count = this%m_count + 1
        do j = n, i, -1
            call this%store(j + 1, this%get(j))
        end do
        call this%store(i, x)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Removes an item from the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index at which to remove the item.  Any objects behind
    !!  this object in the list are shifted forward by one.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
    !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p i is out of range.
    module subroutine list_remove(this, i, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: n
        class(*), pointer :: item
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        n = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (i < 1 .or. i > this%get_count()) then
            ! ERROR:
            write (errmsg, '(AI0AI0A)') "The supplied index of ", i, &
                " is outside the bounds of this collection as this " // &
                "collection has ", this%get_count(), " elements."
            call errmgr%report_error("list_remove", trim(errmsg), &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
        end if

        ! Get the removed item, and dispose of properly
        item => this%get(i)
        if (associated(item)) then
            deallocate(item)
            nullify(item)
        end if

        ! Shift everything down by one element
        this%m_list(i:n-1) = this%m_list(i+1:n)
        this%m_count = this%m_count - 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Cleans up resources held by the list.
    !!
    !! @param[in,out] this The list object.
    module subroutine list_destroy(this)
        type(list), intent(inout) :: this
        call this%clear()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reverses the contents of the list.
    !!
    !! @param[in,out] this The list object.
    module subroutine list_reverse(this)
        ! Arguments
        class(list), intent(inout) :: this

        ! Reverse the contents of the array
        integer(int32) :: n
        n = this%get_count()
        this%m_list(1:n) = this%m_list(n:1:-1)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests to see if the list contains the specified item.
    !!
    !! @param[in] this The list object.
    !! @param[in] item The item to search for.
    !! @param[in] fcn The function to use to compare the contents of the list
    !!  against @p item.
    !!
    !! @return Returns true if @p item is found; else, returns false.
    module function list_contains(this, item, fcn) result(rst)
        ! Arguments
        class(list), intent(in) :: this
        class(*), intent(in) :: item
        procedure(items_equal), pointer, intent(in) :: fcn
        logical :: rst

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        rst = .false.
        n = this%get_count()
        do i = 1, n
            if (fcn(item, this%get(i))) then
                rst = .true.
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Finds the index of the first item in the list that matches
    !! the specified object.
    !!
    !! @param[in] this The list object.
    !! @param[in] item The item to search for.
    !! @param[in] fcn The function to use to compare the contents of the list
    !!  against @p item.
    !!
    !! @return Returns the index of the first occurrence of @p item in the
    !!  list.  If no matching item is found, a value of 0 is returned.
    module function list_index_of(this, item, fcn) result(rst)
        ! Arguments
        class(list), intent(in) :: this
        class(*), intent(in) :: item
        procedure(items_equal), pointer, intent(in) :: fcn
        integer(int32) :: rst

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        rst = 0
        n = this%get_count()
        do i = 1, n
            if (fcn(item, this%get(i))) then
                rst = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Finds the indices of all items in the list that match the 
    !! specified object.
    !!
    !! @param[in] this The list object.
    !! @param[in] item The item to search for.
    !! @param[in] fcn The function to use to compare the contents of the list
    !!  against @p item.
    !!
    !! @return Returns an array of indices of all items in the list that match
    !!  @p item.  If not matches are found, an empty array is returned.
    module function list_indices_of_all(this, item, fcn, err) result(rst)
        ! Arguments
        class(list), intent(in) :: this
        class(*), intent(in) :: item
        procedure(items_equal), pointer, intent(in) :: fcn
        class(errors), intent(inout), optional, target :: err
        integer(int32), allocatable, dimension(:) :: rst

        ! Local Variables
        integer(int32) :: i, j, n, flag
        integer(int32), allocatable, dimension(:) :: buffer
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        n = this%get_count()
        j = 0
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        allocate(buffer(n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("list_indices_of_all", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Process
        do i = 1, n
            if (fcn(item, this%get(i))) then
                j = j + 1
                buffer(j) = i
            end if
        end do

        ! Output
        if (j == 0) then
            allocate(rst(0))
        else
            rst = buffer(1:j)
        end if
    end function

! ------------------------------------------------------------------------------
    ! Swaps two items in the list
    module subroutine list_swap(this, i1, i2, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i1, i2
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: n
        type(container) :: temp
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        n = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Quick Return
        if (n == 0 .or. i1 == i2) return

        ! Input Check
        if (i1 < 0 .or. i1 > n) then
            write(errmsg, '(AI0AI0A)') "The I1 index of ", i1, &
                " is outside the bounds of the array: [0, ", n, "]."
            call errmgr%report_error("list_swap", trim(errmsg), &
                FCORE_ARRAY_SIZE_ERROR)
            return
        end if

        if (i2 < 0 .or. i2 > n) then
            write(errmsg, '(AI0AI0A)') "The I2 index of ", i2, &
                " is outside the bounds of the array: [0, ", n, "]."
            call errmgr%report_error("list_swap", trim(errmsg), &
                FCORE_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        temp = this%m_list(i1)
        this%m_list(i1) = this%m_list(i2)
        this%m_list(i2) = temp
    end subroutine

! ------------------------------------------------------------------------------
    ! http://www.personal.psu.edu/jhm/f90/examples/sort/sorthalf.f
    !> @brief Sorts an array into ascending order.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] fcn The function to use to make the comparison.
    module subroutine list_sort(this, fcn)
        ! Arguments
        class(list), intent(inout) :: this
        procedure(compare_items), pointer, intent(in) :: fcn

        ! Local Variables
        integer(int32) :: n

        ! Process
        n = this%get_count()
        if (n <= 1) return
        call list_sort_core(this%m_list(1:n), fcn)
    end subroutine

! --------------------
    subroutine list_sort_core(x, fcn)
        ! Arguments
        type(container), intent(inout), dimension(:) :: x
        procedure(compare_items), pointer, intent(in) :: fcn

        ! Local Variables
        real(real64) :: r
        type(container) :: t, tt
        integer(int32) :: i, ij, j, k, l, m, n, il(21), iu(21)
        
        ! Initialization
        n = size(x)

        ! Quick Return
        if (n <= 1) return

        ! Process
        m = 1
        i = 1
        j = n
        r = 0.375d0
    20  if (i == j) go to 60
        if (r <= 0.5898437d0) then
            r = r + 3.90625d-2
        else
            r = r - 0.21875d0
        end if

    30  k = i
        ! Select a central element of the array, and save it in location T
        ij = i + int((j - i) * r)
        t = x(ij)

        ! Compare: If the first element of the array is greater than T, 
        ! interchange with T
        if (fcn(x(i)%item, t%item) == 1) then
            x(ij) = x(i)
            x(i) = t
            t = x(ij)
        end if
        l = j

        ! If the last element of the array is less than T, interchange 
        ! with T
        if (fcn(x(j)%item, t%item) == -1) then
            x(ij) = x(j)
            x(j) = t
            t = x(ij)

            ! If the first element of the array is greater than T, 
            ! interchange with T
            if (fcn(x(i)%item, t%item) == 1) then
                x(ij) = x(i)
                x(i) = t
                t = x(ij)
            end if
        end if

        ! Find an element in the second half of the array which is smaller
        ! than T
    40  l = l - 1
        if (fcn(x(l)%item, t%item) == 1) go to 40

        ! Find an element in the first half of the array which is greater
        ! than T
    50  k = k + 1
        if (fcn(x(k)%item, t%item) == -1) go to 50

        ! Interchange these elements
        if (k <= l) then
            tt = x(l)
            x(l) = x(k)
            x(k) = tt
            go to 40
        end if

        ! Save upper and lower subscripts of the array yet to be sorted
        if (l - i > j - k) then
            il(m) = i
            iu(m) = l
            i = k
            m = m + 1
        else
            il(m) = k
            iu(m) = j
            j = l
            m = m + 1
        end if
        go to 70

        ! Begin again on another portion of the unsorted array
    60  m = m - 1
        if (m == 0) go to 100
        i = il(m)
        j = iu(m)
    
    70  if (j - i >= 1) go to 30
        if (i == 1) go to 20
        i = i - 1

    80  i = i + 1
        if (i == j) go to 60
        t = x(i + 1)
        if (fcn(x(i)%item, t%item) == -1) go to 80
        k = i

    90  x(k + 1) = x(k)
        k = k - 1
        if (fcn(t%item, x(k)%item) == -1) go to 90
        x(k + 1) = t
        go to 80

    ! End
    100 continue
        return
    end subroutine

! ------------------------------------------------------------------------------
end submodule
