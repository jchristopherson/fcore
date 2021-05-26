! csv_column.f90

submodule (csv) csv_column_

    interface get_data_type_name
        module procedure :: get_data_type_name_real
        module procedure :: get_data_type_name_logical
        module procedure :: get_data_type_name_string
        module procedure :: get_data_type_name_real_array
        module procedure :: get_data_type_name_logical_array
        module procedure :: get_data_type_name_string_array
    end interface

contains
! ------------------------------------------------------------------------------
    pure module function cc_get_header(this) result(rst)
        class(csv_column), intent(in) :: this
        character(len = :), allocatable :: rst
        if (.not.allocated(this%m_header)) then
            rst = ""
        else
            rst = this%m_header
        end if
    end function

! ------------------------------------------------------------------------------
    module subroutine cc_set_header(this, x)
        class(csv_column), intent(inout) :: this
        character(len = *), intent(in) :: x
        this%m_header = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function cc_get_data_type(this) result(rst)
        class(csv_column), intent(in) :: this
        integer(int32) :: rst
        rst = this%m_dataType
    end function

! ------------------------------------------------------------------------------
    module subroutine cc_set_data_type(this, x)
        class(csv_column), intent(inout) :: this
        integer(int32), intent(in) :: x
        if (x == CSV_STRING_DATA .or. &
            x == CSV_NUMERIC_DATA .or. &
            x == CSV_LOGICAL_DATA) &
        then
            this%m_dataType = x
        else
            this%m_dataType = CSV_STRING_DATA
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function cc_get_data_count(this) result(rst)
        class(csv_column), intent(in) :: this
        integer(int32) :: rst
        rst = this%m_data%get_count()
    end function

! ------------------------------------------------------------------------------
    module function cc_get_string_item(this, index, id, err) result(rst)
        ! Arguments
        class(csv_column), intent(in) :: this
        integer(int32), intent(in) :: index
        character(len = *), intent(in) :: id
        class(errors), intent(inout), optional, target :: err
        character(len = :), allocatable :: rst

        ! Local Variables
        class(*), pointer :: ptr
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (this%get_data_type() /= CSV_STRING_DATA) then
            write (errmsg, '(A)') &
                "This column holds string data, but a request for " // &
                get_data_type_name(id) // " was made."
            call errmgr%report_error("cc_get_string_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_get_string_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        ptr => this%m_data%get(index)
        select type (ptr)
        class is (string)
            rst = ptr%str
        type is (character(len = *))
            rst = ptr
        end select
    end function

! ------------------------------------------------------------------------------
    module function cc_get_numeric_item(this, index, id, err) result(rst)
        ! Arguments
        class(csv_column), intent(in) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: id
        class(errors), intent(inout), optional, target :: err
        real(real64) :: rst

        ! Local Variables
        class(*), pointer :: ptr
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (this%get_data_type() /= CSV_NUMERIC_DATA) then
            write (errmsg, '(A)') &
                "This column holds numeric data, but a request for " // &
                get_data_type_name(id) // " was made."
            call errmgr%report_error("cc_get_numeric_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_get_numeric_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        ptr => this%m_data%get(index)
        select type (ptr)
        type is (real(real64))
            rst = ptr
        end select
    end function

! ------------------------------------------------------------------------------
    module function cc_get_logical_item(this, index, id, err) result(rst)
        ! Arguments
        class(csv_column), intent(in) :: this
        integer(int32), intent(in) :: index
        logical, intent(in) :: id
        class(errors), intent(inout), optional, target :: err
        logical :: rst

        ! Local Variables
        class(*), pointer :: ptr
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (this%get_data_type() /= CSV_LOGICAL_DATA) then
            write (errmsg, '(A)') &
                "This column holds logical data, but a request for " // &
                get_data_type_name(id) // " was made."
            call errmgr%report_error("cc_get_logical_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_get_logical_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        ptr => this%m_data%get(index)
        select type (ptr)
        type is (logical)
            rst = ptr
        end select
    end function

! ------------------------------------------------------------------------------
    module subroutine cc_set_string_item(this, index, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        integer(int32), intent(in) :: index
        character(len = *), intent(in) :: x
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

        ! Input Checking
        if (this%get_data_type() /= CSV_STRING_DATA) then
            write (errmsg, '(A)') &
                "This column holds string data, but " // &
                get_data_type_name(x) // " was supplied."
            call errmgr%report_error("cc_set_string_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_set_string_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Store the data
        call this%m_data%set(index, x)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_set_numeric_item(this, index, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
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

        ! Input Checking
        if (this%get_data_type() /= CSV_NUMERIC_DATA) then
            write (errmsg, '(A)') &
                "This column holds numeric data, but " // &
                get_data_type_name(x) // " was supplied."
            call errmgr%report_error("cc_set_numeric_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_set_numeric_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Store the data
        call this%m_data%set(index, x)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_set_logical_item(this, index, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        integer(int32), intent(in) :: index
        logical, intent(in) :: x
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

        ! Input Checking
        if (this%get_data_type() /= CSV_LOGICAL_DATA) then
            write (errmsg, '(A)') &
                "This column holds logical data, but " // &
                get_data_type_name(x) // " was supplied."
            call errmgr%report_error("cc_set_logical_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_set_logical_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Store the data
        call this%m_data%set(index, x)
    end subroutine

! ------------------------------------------------------------------------------
    module function cc_get_string_array(this, id, err) result(rst)
        ! Arguments
        class(csv_column), intent(in) :: this
        character(len = *), intent(in) :: id
        class(errors), intent(inout), optional, target :: err
        type(string), allocatable, dimension(:) :: rst

        ! Local Variables
        integer(int32) :: i, n, flag
        class(*), pointer :: ptr
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (this%get_data_type() /= CSV_STRING_DATA) then
            write (errmsg, '(A)') &
                "This column holds string data, but a request for " // &
                get_data_type_name(id) // " was made."
            call errmgr%report_error("cc_get_string_array", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if

        ! Process
        n = this%get_count()
        allocate(rst(n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("cc_get_string_array", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if
        do i = 1, n
            ptr => this%m_data%get(i)
            select type (ptr)
            class is (string)
                rst(i)%str = ptr%str
            type is (character(len = *))
                rst(i)%str = ptr
            end select
        end do
    end function

! ------------------------------------------------------------------------------
    module function cc_get_numeric_array(this, id, err) result(rst)
        ! Arguments
        class(csv_column), intent(in) :: this
        real(real64), intent(in) :: id
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:) :: rst

        ! Local Variables
        integer(int32) :: i, n, flag
        class(*), pointer :: ptr
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (this%get_data_type() /= CSV_NUMERIC_DATA) then
            write (errmsg, '(A)') &
                "This column holds numeric data, but a request for " // &
                get_data_type_name(id) // " was made."
            call errmgr%report_error("cc_get_numeric_array", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if

        ! Process
        n = this%get_count()
        allocate(rst(n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("cc_get_numeric_array", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if
        do i = 1, n
            ptr => this%m_data%get(i)
            select type (ptr)
            type is (real(real64))
                rst(i) = ptr
            end select
        end do
    end function

! ------------------------------------------------------------------------------
    module function cc_get_logical_array(this, id, err) result(rst)
        ! Arguments
        class(csv_column), intent(in) :: this
        logical, intent(in) :: id
        class(errors), intent(inout), optional, target :: err
        logical, allocatable, dimension(:) :: rst

        ! Local Variables
        integer(int32) :: i, n, flag
        class(*), pointer :: ptr
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (this%get_data_type() /= CSV_LOGICAL_DATA) then
            write (errmsg, '(A)') &
                "This column holds logical data, but a request for " // &
                get_data_type_name(id) // " was made."
            call errmgr%report_error("cc_get_logical_array", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if

        ! Process
        n = this%get_count()
        allocate(rst(n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("cc_get_logical_array", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if
        do i = 1, n
            ptr => this%m_data%get(i)
            select type (ptr)
            type is (logical)
                rst(i) = ptr
            end select
        end do
    end function

! ------------------------------------------------------------------------------
    module subroutine cc_set_string_array(this, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        class(string), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (this%get_data_type() /= CSV_STRING_DATA) then
            write (errmsg, '(A)') &
                "This column holds string data, but a request for " // &
                get_data_type_name(x) // " was made."
            call errmgr%report_error("cc_set_string_array", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if

        ! Process
        n = size(x)
        call this%m_data%clear()
        if (this%m_data%get_capacity() < n) then
            call this%m_data%set_capacity(n, errmgr)
            if (errmgr%has_error_occurred()) return
        end if
        do i = 1, n
            call this%m_data%push(x(i))
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_set_numeric_array(this, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (this%get_data_type() /= CSV_NUMERIC_DATA) then
            write (errmsg, '(A)') &
                "This column holds numeric data, but a request for " // &
                get_data_type_name(x) // " was made."
            call errmgr%report_error("cc_set_numeric_array", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if

        ! Process
        n = size(x)
        call this%m_data%clear()
        if (this%m_data%get_capacity() < n) then
            call this%m_data%set_capacity(n, errmgr)
            if (errmgr%has_error_occurred()) return
        end if
        do i = 1, n
            call this%m_data%push(x(i))
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_set_logical_array(this, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        logical, intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (this%get_data_type() /= CSV_LOGICAL_DATA) then
            write (errmsg, '(A)') &
                "This column holds logical data, but a request for " // &
                get_data_type_name(x) // " was made."
            call errmgr%report_error("cc_set_logical_array", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if

        ! Process
        n = size(x)
        call this%m_data%clear()
        if (this%m_data%get_capacity() < n) then
            call this%m_data%set_capacity(n, errmgr)
            if (errmgr%has_error_occurred()) return
        end if
        do i = 1, n
            call this%m_data%push(x(i))
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_insert_string_item(this, index, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        integer(int32), intent(in) :: index
        character(len = *), intent(in) :: x
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

        ! Input Checking
        if (this%get_data_type() /= CSV_STRING_DATA) then
            write (errmsg, '(A)') &
                "This column holds string data, but a request for " // &
                get_data_type_name(x) // " was made."
            call errmgr%report_error("cc_insert_string_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_insert_string_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        call this%m_data%insert(index, x, errmgr)
        if (errmgr%has_error_occurred()) return
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_insert_numeric_item(this, index, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
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

        ! Input Checking
        if (this%get_data_type() /= CSV_NUMERIC_DATA) then
            write (errmsg, '(A)') &
                "This column holds numeric data, but a request for " // &
                get_data_type_name(x) // " was made."
            call errmgr%report_error("cc_insert_numeric_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_insert_numeric_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        call this%m_data%insert(index, x, errmgr)
        if (errmgr%has_error_occurred()) return
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_insert_logical_item(this, index, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        integer(int32), intent(in) :: index
        logical, intent(in) :: x
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

        ! Input Checking
        if (this%get_data_type() /= CSV_LOGICAL_DATA) then
            write (errmsg, '(A)') &
                "This column holds logical data, but a request for " // &
                get_data_type_name(x) // " was made."
            call errmgr%report_error("cc_insert_logical_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_insert_logical_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        call this%m_data%insert(index, x, errmgr)
        if (errmgr%has_error_occurred()) return
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_append_string_item(this, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        character(len = *), intent(in) :: x
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

        ! Input Checking
        if (this%get_data_type() /= CSV_STRING_DATA) then
            write (errmsg, '(A)') &
                "This column holds string data, but a request for " // &
                get_data_type_name(x) // " was made."
            call errmgr%report_error("cc_append_string_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if

        ! Process
        call this%m_data%push(x, errmgr)
        if (errmgr%has_error_occurred()) return
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_append_numeric_item(this, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        real(real64), intent(in) :: x
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

        ! Input Checking
        if (this%get_data_type() /= CSV_NUMERIC_DATA) then
            write (errmsg, '(A)') &
                "This column holds numeric data, but a request for " // &
                get_data_type_name(x) // " was made."
            call errmgr%report_error("cc_append_numeric_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if

        ! Process
        call this%m_data%push(x, errmgr)
        if (errmgr%has_error_occurred()) return
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_append_logical_item(this, x, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        logical, intent(in) :: x
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

        ! Input Checking
        if (this%get_data_type() /= CSV_LOGICAL_DATA) then
            write (errmsg, '(A)') &
                "This column holds logical data, but a request for " // &
                get_data_type_name(x) // " was made."
            call errmgr%report_error("cc_append_logical_item", &
                trim(errmsg), FCORE_DATA_TYPE_ERROR)
            return
        end if

        ! Process
        call this%m_data%push(x, errmgr)
        if (errmgr%has_error_occurred()) return
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine cc_remove_item(this, index, err)
        ! Arguments
        class(csv_column), intent(inout) :: this
        integer(int32), intent(in) :: index
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (index < 1 .or. index > this%get_count()) then
            call errmgr%report_error("cc_remove_item", &
                "The supplied array index is outside the bounds of this " // &
                "collection.", FCORE_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        call this%m_data%remove(index)
    end subroutine

! ******************************************************************************
! PRIVATE ROUTINES
! ------------------------------------------------------------------------------
    pure function get_data_type_name_real(flag) result(rst)
        real(real64), intent(in) :: flag
        character(len = :), allocatable :: rst
        rst = "numeric"
    end function

    pure function get_data_type_name_logical(flag) result(rst)
        logical, intent(in) :: flag
        character(len = :), allocatable :: rst
        rst = "logical"
    end function

    pure function get_data_type_name_string(flag) result(rst)
        character(len = *), intent(in) :: flag
        character(len = :), allocatable :: rst
        rst = "string"
    end function

    pure function get_data_type_name_real_array(flag) result(rst)
        real(real64), intent(in), dimension(:) :: flag
        character(len = :), allocatable :: rst
        rst = "numeric"
    end function

    pure function get_data_type_name_logical_array(flag) result(rst)
        logical, intent(in), dimension(:) :: flag
        character(len = :), allocatable :: rst
        rst = "logical"
    end function

    pure function get_data_type_name_string_array(flag) result(rst)
        class(string), intent(in), dimension(:) :: flag
        character(len = :), allocatable :: rst
        rst = "string"
    end function

! ------------------------------------------------------------------------------
end submodule