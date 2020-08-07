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

    !> @brief Sets the Fortran unit value to associate with this file.
    module subroutine fm_set_unit(this, x)
        ! Arguments
        class(file_manager), intent(inout) :: this
        integer(int32), intent(in) :: x
        this%m_unit = x
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
        rst = .true.
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
    !! @param[in,out] this The file_manager object.
    module subroutine fm_clean_up(this)
        type(file_manager), intent(inout) :: this
        call this%close(.false.)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the filename.
    !!
    !! @param[in] this The file_manager object.
    !! 
    !! @return The filename.
    pure module function fm_get_fname(this) result(rst)
        class(file_manager), intent(in) :: this
        character(len = :), allocatable :: rst
        rst = this%m_fname
    end function

! --------------------
    !> @brief Sets the filename.
    !!
    !! @param[in,out] this The file_manager object.
    !! @param[in] x The filename.
    module subroutine fm_set_fname(this, x)
        class(file_manager), intent(inout) :: this
        character(len = *), intent(in) :: x
        this%m_fname = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the size of the currently open file.
    !!
    !! @param[in] this The file_manager object.
    !!
    !! @return The file size, in bytes.
    module function fm_get_size(this) result(rst)
        ! Arguments
        class(file_manager), intent(in) :: this
        integer(int32) :: rst

        ! Process
        rst = 0
        if (.not.this%is_open()) return
        inquire(file = this%get_filename(), size = rst)
    end function

! ------------------------------------------------------------------------------
end submodule
