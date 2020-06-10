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
        indices = index_of_all(txt, delim)
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
                start = last + nsubs + 1
            end if
            if (i == size(rst)) then
                last = len(txt)
            else
                last = indices(i) - 1
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
    pure module function index_of_all_chars(str, sub) result(rst)
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
    pure module function index_of_all_str(str, sub) result(rst)
        ! Arguments
        type(string), intent(in) :: str, sub
        integer(int32), allocatable, dimension(:) :: rst

        ! Process
        rst = index_of_all_chars(str%str, sub%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Replaces all occurrences of a substring within the original 
    !! string.
    !!
    !! @param[in] str The string on which to operate.
    !! @param[in] substr The substring to replace.
    !! @param[in] newstr The substring with which @p substr will be replaced.
    !!
    !! @return The modified string.
    pure module function replace_char(str, substr, newstr) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str, substr, newstr
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32), allocatable, dimension(:) :: indices
        integer(int32) :: i, nsub, nnew, nrst, nind, nstr, startNew, startOld, &
            lastNew, lastOld, start, last

        ! Find all starting indices of SUBSTR in STR
        indices = index_of_all(str, substr)
        nind = size(indices)
        
        ! Quick return
        if (nind == 0) then
            rst = str
            return
        end if

        ! Process
        nsub = len(substr)
        nnew = len(newstr)
        nstr = len(str)
        if (nsub == nnew) then
            ! This is a simple swap as both strings are the same size
            rst = str
            do i = 1, nind
                start = indices(i)
                last = start + nsub - 1
                rst(start:last) = newstr
            end do
        else
            ! This swap becomes more complex as the two strings are different
            ! size
            nrst = nstr + nind * (nnew - nsub)
            allocate(character(len = nrst) :: rst)
            do i = 1, nind + 1
                ! Determine the starting and ending indices
                if (i == 1) then
                    startOld = 1
                    startNew = 1
                else
                    startOld = lastOld + nsub
                    startNew = lastNew + nnew
                end if

                if (i == nind + 1) then
                    lastOld = nstr
                    lastNew = nrst
                else
                    lastOld = indices(i) - 1
                    lastNew = startNew + (lastOld - startOld)
                end if

                if (startOld > nstr .or. startNew > nrst) exit

                if (lastOld == 0) then
                    rst(1:nnew) = newstr
                    lastOld = 1
                    lastNew = 1
                else if (i == nind + 1) then
                    rst(startNew:lastNew) = str(startOld:lastOld)
                else
                    rst(startNew:lastNew) = str(startOld:lastOld)
                    start = min(lastNew + 1, nrst)
                    last = min(lastNew + nnew, nrst)
                    rst(start:last) = newstr(1:last-start+1)
                end if
            end do
        end if
    end function

! --------------------
    !> @brief Replaces all occurrences of a substring within the original 
    !! string.
    !!
    !! @param[in] str The string on which to operate.
    !! @param[in] substr The substring to replace.
    !! @param[in] newstr The substring with which @p substr will be replaced.
    !!
    !! @return The modified string.
    pure module function replace_str(str, substr, newstr) result(rst)
        ! Arguments
        type(string), intent(in) :: str, substr, newstr
        type(string) :: rst

        ! Process
        rst%str = replace_char(str%str, substr%str, newstr%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Removes all occurrences of a substring within the original string.
    !!
    !! @param[in] str The string on which to operate.
    !! @param[in] substr The substring to remove.
    !!
    !! @return The modified string.
    pure module function remove_char(str, substr) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str, substr
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32), allocatable, dimension(:) :: indices
        integer(int32) :: i, nind, nrmv, nrst, nstr, nsub, startOld, startNew, &
            lastOld, lastNew

        ! Locate all indices of the substrings to remove
        indices = index_of_all(str, substr)
        nind = size(indices)

        ! Quick Return
        if (nind == 0) then
            rst = str
            return
        end if

        ! Process
        nstr = len(str)
        nsub = len(substr)
        nrst = nstr - nind * nsub
        allocate(character(len = nrst) :: rst)
        do i = 1, nind + 1
            ! Determine the starting and ending indices
            if (i == 1) then
                startOld = 1
                startNew = 1
            else
                startOld = lastOld + nsub + 1
                startNew = lastNew + 1
            end if

            if (i == nind + 1) then
                lastOld = nstr
                lastNew = nrst
            else
                lastOld = indices(i) - 1
                lastNew = startNew + (lastOld - startOld)
            end if

            if (startOld > nstr .or. startNew > nrst) exit

            ! Copy the appropriate substrings back
            if (lastOld == 0) cycle
            rst(startNew:lastNew) = str(startOld:lastOld)
        end do
    end function

! --------------------
    !> @brief Removes all occurrences of a substring within the original string.
    !!
    !! @param[in] str The string on which to operate.
    !! @param[in] substr The substring to remove.
    !!
    !! @return The modified string.
    pure module function remove_str(str, substr) result(rst)
        ! Arguments
        type(string), intent(in) :: str, substr
        type(string) :: rst

        ! Process
        rst%str = remove_char(str%str, substr%str)
    end function

! ------------------------------------------------------------------------------
! REPLACE AT

! --------------------

! ------------------------------------------------------------------------------
! REMOVE AT

! --------------------

! ------------------------------------------------------------------------------
! To upper case

! --------------------

! ------------------------------------------------------------------------------
! To lower case

! --------------------

! ------------------------------------------------------------------------------
! Contains substring

! --------------------

! ------------------------------------------------------------------------------
! Number to string routines

! --------------------

! ------------------------------------------------------------------------------
! Parse strings to numbers

! --------------------

! ------------------------------------------------------------------------------
end submodule
