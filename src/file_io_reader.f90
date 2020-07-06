! file_io_reader.f90

submodule (file_io) file_io_reader
contains
! ------------------------------------------------------------------------------
    !> @brief Gets the current position within the file.
    !!
    !! @param[in] this The file_reader object.
    !! @return The current position.
    pure module function fr_get_position(this) result(rst)
        ! Arguments
        class(file_reader), intent(in) :: this
        integer(int32) :: rst

        ! Process
        rst = this%m_position
    end function

! --------------------
    !> @brief Sets the position within the file.
    !!
    !! @param[in,out] this The file_reader object.
    !! @param[in] x The file position.
    module subroutine fr_set_position(this, x)
        ! Arguments
        class(file_reader), intent(inout) :: this
        integer(int32), intent(in) :: x

        ! Local Variables
        integer(int32) :: p

        ! Quick Return
        if (.not. this%is_open()) return

        ! Initialization
        if (x < 1) then
            p = 1
        else
            p = x
        end if

        ! Process
        this%m_position = p
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Moves the current position to the start of the file.
    !!
    !! @param[in,out] this The file_reader object.
    module subroutine fr_move_to_start(this)
        ! Arguments
        class(file_reader), intent(inout) :: this

        ! Set the position
        call this%set_position(1)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests to see if the current position denotes the end-of-file.
    !!
    !! @param[in] this The file_reader object.
    !!
    !! @return Returns true if the current position is the end-of-file; else,
    !!  false.
    module function fr_eof(this) result(rst)
        ! Arguments
        class(file_reader), intent(in) :: this
        logical :: rst

        ! Local Variables
        integer(int8) :: temp
        integer(int32) :: flag

        ! Process - do not increment the position
        read(this%get_unit(), pos = this%m_position, iostat = flag) temp
        rst = flag < 0
    end function

! ------------------------------------------------------------------------------
end submodule
