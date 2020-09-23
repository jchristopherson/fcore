! collections_hash.f90

submodule (collections) collections_hash
contains
! ******************************************************************************
! HASH_GENERATOR MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Initializes the hash code generator object.
    !!
    !! @param[in,out] this The hash_code object.
    module subroutine hc_init(this)
        ! Arguments
        class(hash_code), intent(inout) :: this

        ! Local Variables
        integer(int64) :: rem
        integer(int32) :: i, j

        ! Process
        if (.not.this%have_table) then
            do i = 1, size(this%table)
                rem = int(i - 1, int64)
                do j = 1, 8
                    if (iand(rem, 1_int64) /= 0_int64) then
                        rem = ieor(ishft(rem, -abs(1_int64)), this%crcpoly)
                    else
                        rem = ishft(rem, -abs(1_int64))
                    end if
                end do
                this%table(i) = rem
            end do
            this%have_table = .true.
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a unique hash code for the supplied string.
    !!
    !! @param[in,out] this The hash_code object.
    !! @param[in] str The string to hash.
    !!
    !! @return The hash code for @p str.
    module function hc_get(this, str) result(rst)
        ! Arguments
        class(hash_code), intent(inout) :: this
        character(len = *), intent(in) :: str
        integer(int64) :: rst

        ! Local Variables
        integer(int32) :: i, j, n
        integer(int64) :: crc

        ! Parameters
        integer(int64), parameter :: ff = int(z'ff', int64)

        ! Ensure initialization
        if (.not.this%have_table) call this%initialize()

        ! Process
        crc = this%initxor
        n = len(str)
        do i = 1, n
            j = int(iand(ieor(crc, int(iachar(str(i:i)), int64)), ff))
            crc = ieor(ishft(crc, -abs(8_int64)), this%table(j + 1))
        end do
        rst = ieor(crc, this%finalxor)
    end function

! ------------------------------------------------------------------------------
end submodule
