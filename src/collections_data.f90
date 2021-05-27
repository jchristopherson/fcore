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

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
