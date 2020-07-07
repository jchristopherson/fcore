! file_io_endian.f90

submodule (file_io) file_io_endian
contains
! ------------------------------------------------------------------------------
    !> @brief Determines if the current machine is little-endian or big-endian.
    !!
    !! @return Returns true if the current machine is little-endian; else, false
    !! if big-endian.
    pure module function is_little_endian() result(rst)
        ! Arguments
        logical :: rst

        ! Process
        integer(int8) :: j(2)
        integer(int16) :: i
        equivalence(i, j)
        i = 1
        rst = j(1) == 1
    end function

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 64-bit floating-point value.
    !!
    !! @param[in,out] x The value whose byte order is to be swapped.
    !!
    !! @return The resulting byte-swapped value.
    elemental module function swap_bytes_r64(x) result(rst)
        ! Arguments
        real(real64), intent(in) :: x
        real(real64) :: rst

        ! Local Variables
        integer(int8) :: ii(8), jj(8)
        real(real64) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(8)
        jj(2) = ii(7)
        jj(3) = ii(6)
        jj(4) = ii(5)
        jj(5) = ii(4)
        jj(6) = ii(3)
        jj(7) = ii(2)
        jj(8) = ii(1)
        rst = t
    end function

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 32-bit floating-point value.
    !!
    !! @param[in,out] x The value whose byte order is to be swapped.
    !!
    !! @return The resulting byte-swapped value.
    elemental module function swap_bytes_r32(x) result(rst)
        ! Arguments
        real(real32), intent(in) :: x
        real(real32) :: rst

        ! Local Variables
        integer(int8) :: ii(4), jj(4)
        real(real32) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(4)
        jj(2) = ii(3)
        jj(3) = ii(2)
        jj(4) = ii(1)
        rst = t
    end function

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 16-bit integer value.
    !!
    !! @param[in,out] x The value whose byte order is to be swapped.
    !!
    !! @return The resulting byte-swapped value.
    elemental module function swap_bytes_i16(x) result(rst)
        ! Arguments
        integer(int16), intent(in) :: x
        integer(int16) :: rst

        ! Local Variables
        integer(int8) :: ii(2), jj(2)
        integer(int16) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(2)
        jj(2) = ii(1)
        rst = t
    end function

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 32-bit integer value.
    !!
    !! @param[in,out] x The value whose byte order is to be swapped.
    !!
    !! @return The resulting byte-swapped value.
    elemental module function swap_bytes_i32(x) result(rst)
        ! Arguments
        integer(int32), intent(in) :: x
        integer(int32) :: rst

        ! Local Variables
        integer(int8) :: ii(4), jj(4)
        integer(int32) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(4)
        jj(2) = ii(3)
        jj(3) = ii(2)
        jj(4) = ii(1)
        rst = t
    end function

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 64-bit integer value.
    !!
    !! @param[in,out] x The value whose byte order is to be swapped.
    !!
    !! @return The resulting byte-swapped value.
    elemental module function swap_bytes_i64(x) result(rst)
        ! Arguments
        integer(int64), intent(in) :: x
        integer(int64) :: rst

        ! Local Variables
        integer(int8) :: ii(8), jj(8)
        integer(int64) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(8)
        jj(2) = ii(7)
        jj(3) = ii(6)
        jj(4) = ii(5)
        jj(5) = ii(4)
        jj(6) = ii(3)
        jj(7) = ii(2)
        jj(8) = ii(1)
        rst = t
    end function

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 64-bit complex-value.
    !!
    !! @param[in,out] x The value whose byte order is to be swapped.
    !!
    !! @return The resulting byte-swapped value.
    elemental module function swap_bytes_c64(x) result(rst)
        ! Arguments
        complex(real64), intent(in) :: x
        complex(real64) :: rst

        ! Local Variables
        real(real64) :: re, im

        ! Process
        re = real(x)
        im = aimag(x)
        re = swap_bytes_r64(re)
        im = swap_bytes_r64(im)
        rst = cmplx(re, im, real64)
    end function

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 32-bit complex-value.
    !!
    !! @param[in,out] x The value whose byte order is to be swapped.
    !!
    !! @return The resulting byte-swapped value.
    elemental module function swap_bytes_c32(x) result(rst)
        ! Arguments
        complex(real32), intent(in) :: x
        complex(real32) :: rst

        ! Local Variables
        real(real32) :: re, im

        ! Process
        re = real(x)
        im = aimag(x)
        re = swap_bytes_r32(re)
        im = swap_bytes_r32(im)
        rst = cmplx(re, im, real32)
    end function

! ------------------------------------------------------------------------------
end submodule