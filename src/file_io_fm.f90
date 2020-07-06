! file_io_fm.f90

submodule (file_io) file_io_tfm
contains
! ------------------------------------------------------------------------------
    !> @brief Returns the unit value for the file object.
    !!
    !! @param[in,out] this The file_manager object.
    !!
    !! @return The Fortran unit value for this stream.
    module function fm_get_unit(this) result(rst)
        class(file_manager), intent(in) :: this
        integer(int32) :: rst
        rst = this%m_unit
    end function

! ------------------------------------------------------------------------------
    !> @brief Creates a new, unused unit value to identify the file.
    !!
    !! @param[in,out] this The file_manager object.
    module subroutine fm_new_unit(this)
        ! Arguments
        class(file_manager), intent(inout) :: this

        ! Parameters
        integer(int32), parameter :: min_unit = 10
        integer(int32), parameter :: max_unit = 1000

        ! Local Variables
        logical :: opened
        integer(int32) :: i

        ! Process
        if (this%m_unit == -1) then
            do i = min_unit, max_unit
                inquire(unit = i, opened = opened)
                if (.not.opened) then
                    this%m_unit = i
                    exit
                end if
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Determines if the file is already opened.
    !!
    !! @param[in] this The file_manager object.
    !!
    !! @return Returns true if the file is opened; else, false.
    module function fm_get_is_opened(this) result(rst)
        ! Arguments
        class(file_manager), intent(in) :: this
        logical :: rst

        ! Process
        if (this%m_unit == -1) then
            rst = .false.
        else
            inquire(unit = this%m_unit, opened = rst)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Closes the file.
    !!
    !! @param[in,out] this The file_manager object.
    !! @param[in] del An optional input, that if set, determines if the file
    !!  should be deleted once closed.  The default is false such that the
    !!  file remains.
    module subroutine fm_close(this, del)
        ! Arguments
        class(file_manager), intent(inout) :: this
        logical, intent(in), optional :: del

        ! Local Variables
        logical :: check

        ! Process
        check = .false.
        if (present(del)) check = del
        if (this%is_open()) then
            if (check) then
                close(this%get_unit(), status = "delete")
            else
                close(this%get_unit())
            end if
            this%m_unit = -1
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Forces closure of the file, if open, whenever the object goes out
    !! of scope.
    !!
    !! @param[in,out] this THe file_manager object.
    module subroutine fm_clean_up(this)
        type(file_manager), intent(inout) :: this
        call this%close(.false.)
    end subroutine

! ------------------------------------------------------------------------------
end submodule
