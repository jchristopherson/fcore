! strings_ops.f90

submodule (strings) strings_ops
contains
! ------------------------------------------------------------------------------
    !> @brief Splits a string into substrings marked by the specified delimiter
    !! string.
    !!
    !! @param[in] txt The string to split.
    !! @param[in] delim The delimiter string.
    !!
    !! @return The resulting array of strings.
    pure module function split_string_char(txt, delim) result(rst)
        ! Arguments
        character(len = *), intent(in) :: txt, delim
        type(string), allocatable, dimension(:) :: rst

        ! Local Variables
        integer(int32), allocatable, dimension(:) :: indices
        integer(int32) :: i, ndelim, nsubs, start, last

        ! Determine the indices of each delimiter in the string
        indices = find_all(txt, delim)
        ndelim = size(indices)
        if (ndelim == 0) then
            allocate(rst(0))
            return
        else
            allocate(rst(ndelim + 1))
        end if

        ! Process
        nsubs = len(delim)
        start = 1

        do i = 1, size(rst)
            ! Determine starting and ending indices of the string
            if (i == 1) then
                start = i
            else
                start = last + nsubs
            end if
            if (i == size(rst)) then
                last = len(txt)
            else
                last = indices(i)
            end if
            rst(i)%str = txt(start:last)
        end do
    end function

! --------------------
    !> @brief Splits a string into substrings marked by the specified delimiter
    !! string.
    !!
    !! @param[in] txt The string to split.
    !! @param[in] delim The delimiter string.
    !!
    !! @return The resulting array of strings.
    pure module function split_string_str(txt, delim) result(rst)
        ! Arguments
        type(string), intent(in) :: txt, delim
        type(string), allocatable, dimension(:) :: rst

        ! Process
        rst = split_string_char(txt%str, delim%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Finds all occurrences of a substring within a string.
    !!
    !! @param[in] str The string to search.
    !! @param[in] sub The substring to search for.
    !!
    !! @return An array of indices where each substring starts.
    pure module function find_all_chars(str, sub) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str, sub
        integer(int32), allocatable, dimension(:) :: rst

        ! Local Variables
        integer(int32) :: i, j, ind, n, nsub
        integer(int32), allocatable, dimension(:) :: buffer

        ! Initialization
        n = len(str)
        nsub = len(sub)
        allocate(buffer(n))

        ! Process
        j = 0
        i = 1
        do
            ! Find the index
            ind = index(str(i:n), sub)
            if (ind == 0) then
                ! Nothing found - exit the loop
                exit
            end if

            ! Store the index of the substring
            j = j + 1
            buffer(j) = ind + i - 1

            ! Update the position to the new starting location
            i = buffer(j) + nsub
            if (i >= n) exit
        end do

        ! End
        if (j == 0) then
            allocate(rst(0))
        else
            rst = buffer(1:j)
        end if
    end function

! --------------------
    !> @brief Finds all occurrences of a substring within a string.
    !!
    !! @param[in] str The string to search.
    !! @param[in] sub The substring to search for.
    !!
    !! @return An array of indices where each substring starts.
    pure module function find_all_str(str, sub) result(rst)
        ! Arguments
        type(string), intent(in) :: str, sub
        integer(int32), allocatable, dimension(:) :: rst

        ! Process
        rst = find_all_chars(str%str, sub%str)
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
