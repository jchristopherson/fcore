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
end submodule
