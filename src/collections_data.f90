! collections_data.f90

submodule (collections) collections_data
    use fcore_constants
contains
! ------------------------------------------------------------------------------
    module subroutine dt_clear(this)
        ! Arguments
        class(data_table), intent(inout) :: this

        ! Local Variables
        integer(int32) :: i, j

        ! Quick Return
        if (.not.associated(this%m_table)) return

        ! Process
        do j = 1, size(this%m_table, 2)
            do i = 1, size(this%m_table, 1)
                if (associated(this%m_table(i,j)%item)) then
                    deallocate(this%m_table(i,j)%item)
                end if
            end do
        end do
        deallocate(this%m_table)
        nullify(this%m_table)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine dt_final(this)
        type(data_table), intent(inout) :: this
        call this%clear()
    end subroutine

! ------------------------------------------------------------------------------
    pure module function dt_get_row_count(this) result(rst)
        class(data_table), intent(in) :: this
        integer(int32) :: rst
        if (associated(this%m_table)) then
            rst = size(this%m_table, 1)
        else
            rst = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function dt_get_column_count(this) result(rst)
        class(data_table), intent(in) :: this
        integer(int32) :: rst
        if (associated(this%m_table)) then
            rst = size(this%m_table, 2)
        else
            rst = 0
        end if
    end function

! ------------------------------------------------------------------------------
    module subroutine dt_initialize(this, m, n, err)
        ! Arguments
        class(data_table), intent(inout) :: this
        integer(int32), intent(in) :: m, n
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Check the inputs
        if (m <= 0) then
            call errmgr%report_error("dt_initialize", "The number of " // &
                "rows must be a positive integer.", FCORE_INVALID_INPUT_ERROR)
            return
        end if
        if (n <= 0) then
            call errmgr%report_error("dt_initialize", "The number of " // &
                "columns must be a positive integer.", &
                FCORE_INVALID_INPUT_ERROR)
            return
        end if

        ! Clear the contents
        call this%clear()

        ! Process
        allocate(this%m_table(m, n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("dt_initialize", "There is " // &
                "insufficient memory available for this operation.", &
                FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function dt_get(this, i, j, err) result(rst)
        ! Arguments
        class(data_table), intent(in) :: this
        integer(int32) :: i, j
        class(errors), intent(inout), optional, target :: err
        class(*), pointer :: rst

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Quick Return
        if (.not.associated(this%m_table)) then
            nullify(rst)
            return
        end if

        ! Bounds Checking
        if (i <= 0 .or. i > size(this%m_table, 1)) then
            write(errmsg, '(AI0AI0A)') "Row index outside the bounds " // &
                "of the array.  Found: ", i, ", but must lie between 1 and ", &
                size(this%m_table, 1), "."
            call errmgr%report_error("dt_get", trim(errmsg), &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if
        if (j <= 0 .or. j > size(this%m_table, 2)) then
            write(errmsg, '(AI0AI0A)') "Column index outside the bounds " // &
                "of the array.  Found: ", j, ", but must lie between 1 and ", &
                size(this%m_table, 2), "."
            call errmgr%report_error("dt_get", trim(errmsg), &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        rst => this%m_table(i,j)%item
    end function

! --------------------
    module subroutine dt_set(this, i, j, x, err)
        ! Arguments
        class(data_table), intent(inout) :: this
        integer(int32), intent(in) :: i, j
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure we've got an array to work with
        if (.not.associated(this%m_table)) then
            call errmgr%report_error("dt_set", "The data table has not " // &
                "yet been initialized.", FCORE_NULL_REFERENCE_ERROR)
            return
        end if

        ! Bounds Checking
        if (i <= 0 .or. i > size(this%m_table, 1)) then
            write(errmsg, '(AI0AI0A)') "Row index outside the bounds " // &
                "of the array.  Found: ", i, ", but must lie between 1 and ", &
                size(this%m_table, 1), "."
            call errmgr%report_error("dt_set", trim(errmsg), &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if
        if (j <= 0 .or. j > size(this%m_table, 2)) then
            write(errmsg, '(AI0AI0A)') "Column index outside the bounds " // &
                "of the array.  Found: ", j, ", but must lie between 1 and ", &
                size(this%m_table, 2), "."
            call errmgr%report_error("dt_set", trim(errmsg), &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Clear the existing item, and store the new item
        if (associated(this%m_table(i,j)%item)) then
            deallocate(this%m_table(i,j)%item)
        end if
        allocate(this%m_table(i,j)%item, source = x, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("dt_set", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine dt_insert_rows(this, rstart, x, err)
        ! Arguments
        class(data_table), intent(inout) :: this
        integer(int32), intent(in) :: rstart
        class(*), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, k, mnew, m, n, flag
        type(container), allocatable, dimension(:,:) :: copy
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Initialization
        m = this%get_row_count()
        n = this%get_column_count()
        mnew = m + size(x, 1)

        ! Input Check
        if (rstart < 1) then
            call errmgr%report_error("dt_insert_rows", &
                "The insertion index must be at least 1.", &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if
        if (rstart > 1 + m) then
            write(errmsg, '(AI0AI0A)') "The insertion index must not be " // &
                "greater than ", m + 1, ", but was found to be ", rstart, "."
            call errmgr%report_error("dt_insert_rows", trim(errmsg), &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if
        if (associated(this%m_table) .and. size(x, 2) /= n) then
            write(errmsg, '(AI0AI0A)') "The input data set was expected " // &
                "to have ", n, " columns, but was found to have ", &
                size(x, 2), "."
            call errmgr%report_error("dt_insert_rows", trim(errmsg), &
                FCORE_ARRAY_SIZE_ERROR)
            return
        end if

        ! If the array is not allocated, allocate and store as the input array
        ! will define the table structure
        if (.not.associated(this%m_table)) then
            call this%initialize(size(x, 1), size(x, 2), err = errmgr)
            if (errmgr%has_error_occurred()) return
            do j = 1, size(x, 2)
                do i = 1, size(x, 1)
                    call this%set(i, j, x(i,j), err = errmgr)
                    if (errmgr%has_error_occurred()) return
                end do
            end do
            return
        end if

        ! If we're here, there was already a properly allocated matrix, and the
        ! size of X is OK.  Start by copying m_table, and then reallocate 
        ! m_table to allow fitting of the new data
        allocate(copy(m, n), stat = flag)
        if (flag /= 0) go to 100
        copy = this%m_table

        deallocate(this%m_table)
        allocate(this%m_table(mnew, n), stat = flag)
        if (flag /= 0) then
            ! Put copy back to m_table, and then handle the error
            this%m_table = copy
            go to 100
        end if
        
        ! Copy back the contents into m_table
        do j = 1, n
            do i = 1, rstart - 1
                this%m_table(i, j) = copy(i, j)
            end do

            k = rstart
            do i = 1, size(x, 1)
                call this%set(k, j, x(i, j), err = errmgr)
                if (errmgr%has_error_occurred()) return
                k = k + 1
            end do

            do i = rstart, m
                this%m_table(k, j) = copy(i, j)
                k = k + 1
            end do
        end do
        
        return
    100 continue
        ! Deal with memory errors
        call errmgr%report_error("dt_insert_rows", &
            "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
        return
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine dt_insert_row(this, i, x, err)
        ! Arguments
        class(data_table), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Check the length of x
        if (associated(this%m_table) .and. &
            size(x) /= this%get_column_count()) &
        then
            write(errmsg, '(AI0AI0A)') "The number of items in the array " // &
                "to insert ,", size(x), ", must match the number of " // &
                "columns in the table ", this%get_column_count(), "."
            call errmgr%report_error("dt_insert_row", trim(errmsg), &
                FCORE_ARRAY_SIZE_ERROR)
            return
        end if

        ! Insert the array
        call this%insert_rows(i, reshape(x, [1, size(x)]), err = errmgr)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine dt_insert_columns(this, cstart, x, err)
        ! Arguments
        class(data_table), intent(inout) :: this
        integer(int32), intent(in) :: cstart
        class(*), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, k, m, n, nnew, flag
        type(container), allocatable, dimension(:,:) :: copy
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Initialization
        m = this%get_row_count()
        n = this%get_column_count()
        nnew = n + size(x, 2)

        ! Input Check
        if (cstart < 1) then
            call errmgr%report_error("dt_insert_columnss", &
                "The insertion index must be at least 1.", &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if
        if (cstart > 1 + n) then
            write(errmsg, '(AI0AI0A)') "The insertion index must not be " // &
                "greater than ", n + 1, ", but was found to be ", cstart, "."
            call errmgr%report_error("dt_insert_columns", trim(errmsg), &
                FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if
        if (associated(this%m_table) .and. size(x, 1) /= m) then
            write(errmsg, '(AI0AI0A)') "The input data set was expected " // &
                "to have ", m, " rows, but was found to have ", &
                size(x, 1), "."
            call errmgr%report_error("dt_insert_columns", trim(errmsg), &
                FCORE_ARRAY_SIZE_ERROR)
            return
        end if

        ! If the array is not allocated, allocate and store as the input array
        ! will define the table structure
        if (.not.associated(this%m_table)) then
            call this%initialize(size(x, 1), size(x, 2), err = errmgr)
            if (errmgr%has_error_occurred()) return
            do j = 1, size(x, 2)
                do i = 1, size(x, 1)
                    call this%set(i, j, x(i,j), err = errmgr)
                    if (errmgr%has_error_occurred()) return
                end do
            end do
            return
        end if

        ! If we're here, there was already a properly allocated matrix, and the
        ! size of X is OK.  Start by copying m_table, and then reallocate 
        ! m_table to allow fitting of the new data
        allocate(copy(m, n), stat = flag)
        if (flag /= 0) go to 100
        copy = this%m_table

        deallocate(this%m_table)
        allocate(this%m_table(mnew, n), stat = flag)
        if (flag /= 0) then
            ! Put copy back to m_table, and then handle the error
            this%m_table = copy
            go to 100
        end if
        
        ! Copy back the contents into m_table
        do j = 1, cstart - 1
            do i = 1, m
                this%m_table(i, j) = copy(i, j)
            end do
        end do

        k = cstart
        do j = 1, size(x, 2)
            do i = 1, m
                call this%set(i, k, x(i, j), err = errmgr)
                if (errmgr%has_error_occurred()) return
            end do
            k = k + 1
        end do

        do j = cstart, n
            do i = 1, m
                this%m_table(i, k) = copy(i, j)
            end do
            k = k + 1
        end do

        return
    100 continue
        ! Deal with memory errors
        call errmgr%report_error("dt_insert_columns", &
            "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
        return
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
