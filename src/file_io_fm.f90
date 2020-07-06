! file_io_fm.f90

submodule (file_io) file_io_tfm
contains
! ------------------------------------------------------------------------------
    !> @brief Returns the unit value.
    !!
    !! @param[in,out] this The file_manager object.
    !!
    !! @return The Fortran unit value for this stream.
    module function fm_get_unit(this) result(rst)
        ! Arguments
        class(file_manager), intent(inout) :: this
        integer(int32) :: rst

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
        rst = this%m_unit
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
