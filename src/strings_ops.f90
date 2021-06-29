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
            rst = txt
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
        class(string), intent(in) :: txt, delim
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
        class(string), intent(in) :: str, sub
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
        class(string), intent(in) :: str, substr, newstr
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
        integer(int32) :: i, nind, nrst, nstr, nsub, startOld, startNew, &
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
        class(string), intent(in) :: str, substr
        type(string) :: rst

        ! Process
        rst%str = remove_char(str%str, substr%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Removes the specified number of characters from the string at the
    !! specified location.
    !! 
    !! @param[in] str The string on which to operate.
    !! @param[in] ind The index at which to start character removal.
    !! @param[in] nchar The number of characters to remove.
    !!
    !! @return The modified string.
    pure module function remove_at_char(str, ind, nchar) result(rst)
        ! Parameters
        character(len = *), intent(in) :: str
        integer(int32), intent(in) :: ind, nchar
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32) :: nstr, nrst

        ! Initialization
        nstr = len(str)
        nrst = nstr - nchar

        ! Quick Return
        if (nrst == 0) then
            allocate(character(len = nrst) :: rst)
            return
        end if

        ! Input Check
        if (nrst < 0 .or. ind < 1 .or. ind > nstr) then
            rst = str
            return
        end if

        ! Process
        if (ind == 1) then
            ! The characters to remove are at the beginning of the string
            rst = str(nchar + 1:nstr)
        else if (ind + nchar - 1 == nstr) then
            ! The characters to remove are at the end of the string
            rst = str(1:ind - 1)
        else
            allocate(character(len = nrst) :: rst)
            rst(1:ind - 1) = str(1:ind - 1)
            rst(ind:nrst) = str(ind + nchar:nstr)
        end if
    end function

! --------------------
    !> @brief Removes the specified number of characters from the string at the
    !! specified location.
    !! 
    !! @param[in] str The string on which to operate.
    !! @param[in] ind The index at which to start character removal.
    !! @param[in] nchar The number of characters to remove.
    !!
    !! @return The modified string.
    pure module function remove_at_str(str, ind, nchar) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        integer(int32), intent(in) :: ind, nchar
        type(string) :: rst

        ! Process
        rst%str = remove_at_char(str%str, ind, nchar)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts an ASCII character string to all upper case.
    !!
    !! @param[in] str The string on which to operate.
    !! @return The modified string.
    pure module function to_upper_char(str) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32) :: i, j, n

        ! Process
        n = len(str)
        allocate(character(len = n) :: rst)
        do i = 1, n
            j = iachar(str(i:i))
            if (j >= iachar("a") .and. j <= iachar("z")) then
                rst(i:i) = achar(iachar(str(i:i)) - 32)
            else
                rst(i:i) = str(i:i)
            end if
        end do
    end function

! --------------------
    !> @brief Converts an ASCII character string to all upper case.
    !!
    !! @param[in] str The string on which to operate.
    !! @return The modified string.
    pure module function to_upper_str(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        type(string) :: rst

        ! Process
        rst%str = to_upper_char(str%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts an ASCII character string to all lower case.
    !!
    !! @param[in] str The string on which to operate.
    !! @return The modified string.
    pure module function to_lower_char(str) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32) :: i, j, n

        ! Process
        n = len(str)
        allocate(character(len = n) :: rst)
        do i = 1, n
            j = iachar(str(i:i))
            if (j >= iachar("A") .and. j <= iachar("Z")) then
                rst(i:i) = achar(iachar(str(i:i)) + 32)
            else
                rst(i:i) = str(i:i)
            end if
        end do
    end function

! --------------------
    !> @brief Converts an ASCII character string to all lower case.
    !!
    !! @param[in] str The string on which to operate.
    !! @return The modified string.
    pure module function to_lower_str(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        type(string) :: rst

        ! Process
        rst%str = to_lower_char(str%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if a substring exists within a parent string.
    !!
    !! @param[in] str The string on which to operate.
    !! @param[in] substr The substring of interest.
    !!
    !! @return Returns true if @p substr exists within @p str; else, returns
    !! false.
    pure module function contains_char(str, substr) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str, substr
        logical :: rst

        ! Process
        integer(int32) :: ind
        ind = index(str, substr)
        rst = ind /= 0
    end function

! --------------------
    !> @brief Tests to see if a substring exists within a parent string.
    !!
    !! @param[in] str The string on which to operate.
    !! @param[in] substr The substring of interest.
    !!
    !! @return Returns true if @p substr exists within @p str; else, returns
    !! false.
    pure module function contains_str(str, substr) result(rst)
        ! Arguments
        class(string), intent(in) :: str, substr
        logical :: rst

        ! Process
        rst = contains_char(str%str, substr%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Attempts to parse a string to a 64-bit floating-point value.
    !!
    !! @param[in] str The string to convert.
    !! 
    !! @return The resulting numeric value.
    pure module function parse_real64_char(str) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        real(real64) :: rst

        ! Process
        read(str, *) rst
    end function

! ----------
    !> @brief Attempts to parse a string to a 64-bit floating-point value.
    !!
    !! @param[in] str The string to convert.
    !! 
    !! @return The resulting numeric value.
    pure module function parse_real64_str(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        real(real64) :: rst

        ! Process
        rst = parse_real64_char(str%str)
    end function

! --------------------
    !> @brief Attempts to parse a string to a 32-bit floating-point value.
    !!
    !! @param[in] str The string to convert.
    !! 
    !! @return The resulting numeric value.
    pure module function parse_real32_char(str) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        real(real32) :: rst

        ! Process
        read(str, *) rst
    end function

! ----------
    !> @brief Attempts to parse a string to a 32-bit floating-point value.
    !!
    !! @param[in] str The string to convert.
    !! 
    !! @return The resulting numeric value.
    pure module function parse_real32_str(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        real(real32) :: rst

        ! Process
        rst = parse_real32_char(str%str)
    end function

! --------------------
    !> @brief Attempts to parse a string to an 8-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !!
    !! @return The resulting numeric value.
    pure module function parse_int8_char(str) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        integer(int8) :: rst

        ! Process
        read(str, *) rst
    end function

! ----------
    !> @brief Attempts to parse a string to an 8-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !!
    !! @return The resulting numeric value.
    pure module function parse_int8_str(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        integer(int8) :: rst

        ! Process
        rst = parse_int8_char(str%str)
    end function

! --------------------
    !> @brief Attempts to parse a string to a 16-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !!
    !! @return The resulting numeric value.
    pure module function parse_int16_char(str) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        integer(int16) :: rst

        ! Process
        read(str, *) rst
    end function

! ----------
    !> @brief Attempts to parse a string to a 16-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !!
    !! @return The resulting numeric value.
    pure module function parse_int16_str(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        integer(int16) :: rst

        ! Process
        rst = parse_int16_char(str%str)
    end function

! --------------------  
    !> @brief Attempts to parse a string to a 32-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !!
    !! @return The resulting numeric value.
    pure module function parse_int32_char(str) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        integer(int32) :: rst

        ! Process
        read(str, *) rst
    end function

! ----------
    !> @brief Attempts to parse a string to a 32-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !!
    !! @return The resulting numeric value.
    pure module function parse_int32_str(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        integer(int32) :: rst

        ! Process
        rst = parse_int32_char(str%str)
    end function

! --------------------
    !> @brief Attempts to parse a string to a 64-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !!
    !! @return The resulting numeric value.
    pure module function parse_int64_char(str) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        integer(int64) :: rst

        ! Process
        read(str, *) rst
    end function

! ----------
    !> @brief Attempts to parse a string to a 64-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !!
    !! @return The resulting numeric value.
    pure module function parse_int64_str(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        integer(int64) :: rst

        ! Process
        rst = parse_int64_char(str%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a number to a string.
    !!
    !! @param[in] num The number to convert.
    !! @param[in] fmt An optional formatting string.
    !!
    !! @return The resulting string.
    pure module function to_string_r64(num, fmt) result(rst)
        ! Arguments
        real(real64), intent(in) :: num
        character(len = *), intent(in), optional :: fmt
        character(len = :), allocatable :: rst

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        if (present(fmt)) then
            write(buffer, fmt) num
        else
            write(buffer, *) num
        end if
        rst = trim(adjustl(buffer))
    end function

! --------------------
    !> @brief Converts a number to a string.
    !!
    !! @param[in] num The number to convert.
    !! @param[in] fmt An optional formatting string.
    !!
    !! @return The resulting string.
    pure module function to_string_r32(num, fmt) result(rst)
        ! Arguments
        real(real32), intent(in) :: num
        character(len = *), intent(in), optional :: fmt
        character(len = :), allocatable :: rst

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        if (present(fmt)) then
            write(buffer, fmt) num
        else
            write(buffer, *) num
        end if
        rst = trim(adjustl(buffer))
    end function

! --------------------
    !> @brief Converts a number to a string.
    !!
    !! @param[in] num The number to convert.
    !!
    !! @return The resulting string.
    pure module function to_string_i8(num) result(rst)
        ! Arguments
        integer(int8), intent(in) :: num
        character(len = :), allocatable :: rst

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(I0)') num
        rst = trim(adjustl(buffer))
    end function

! --------------------
    !> @brief Converts a number to a string.
    !!
    !! @param[in] num The number to convert.
    !!
    !! @return The resulting string.
    pure module function to_string_i16(num) result(rst)
        ! Arguments
        integer(int16), intent(in) :: num
        character(len = :), allocatable :: rst

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(I0)') num
        rst = trim(adjustl(buffer))
    end function

! --------------------
    !> @brief Converts a number to a string.
    !!
    !! @param[in] num The number to convert.
    !!
    !! @return The resulting string.
    pure module function to_string_i32(num) result(rst)
        ! Arguments
        integer(int32), intent(in) :: num
        character(len = :), allocatable :: rst

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(I0)') num
        rst = trim(adjustl(buffer))
    end function

! --------------------
    !> @brief Converts a number to a string.
    !!
    !! @param[in] num The number to convert.
    !!
    !! @return The resulting string.
    pure module function to_string_i64(num) result(rst)
        ! Arguments
        integer(int64), intent(in) :: num
        character(len = :), allocatable :: rst

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(I0)') num
        rst = trim(adjustl(buffer))
    end function

! --------------------
    !> @brief Converts a number to a string.
    !!
    !! @param[in] num The number to convert.
    !! @param[in] fmt An optional formatting string.
    !!
    !! @return The resulting string.
    pure module function to_string_c64(num, fmt) result(rst)
        ! Arguments
        complex(real64), intent(in) :: num
        character(len = *), intent(in), optional :: fmt
        character(len = :), allocatable :: rst

        ! Process
        rst = "(" // to_string(real(num, real64), fmt) // "," // &
            to_string(aimag(num), fmt) // ")"
    end function

! --------------------
    !> @brief Converts a number to a string.
    !!
    !! @param[in] num The number to convert.
    !! @param[in] fmt An optional formatting string.
    !!
    !! @return The resulting string.
    pure module function to_string_c32(num, fmt) result(rst)
        ! Arguments
        complex(real32), intent(in) :: num
        character(len = *), intent(in), optional :: fmt
        character(len = :), allocatable :: rst

        ! Process
        rst = "(" // to_string(real(num, real32), fmt) // "," // &
            to_string(aimag(num), fmt) // ")"
    end function

! ------------------------------------------------------------------------------
    !> @brief Combines two strings.
    !!
    !! @param[in] str1 The first string.
    !! @param[in] str2 The second string.
    !!
    !! @return The resulting string.
    pure module function add_str(str1, str2) result(rst)
        ! Arguments
        class(string), intent(in) :: str1, str2
        type(string) :: rst
        rst%str = str1%str // str2%str
    end function

! --------------------
    !> @brief Combines two strings.
    !!
    !! @param[in] str1 The first string.
    !! @param[in] str2 The second string.
    !!
    !! @return The resulting string.
    pure module function add_char1(str1, str2) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str1
        class(string), intent(in) :: str2
        type(string) :: rst
        rst%str = str1 // str2%str
    end function

! --------------------
    !> @brief Combines two strings.
    !!
    !! @param[in] str1 The first string.
    !! @param[in] str2 The second string.
    !!
    !! @return The resulting string.
    pure module function add_char2(str1, str2) result(rst)
        ! Arguments
        class(string), intent(in) :: str1
        character(len = *), intent(in) :: str2
        type(string) :: rst
        rst%str = str1%str // str2
    end function

! ------------------------------------------------------------------------------
    !> @brief Assigns the contents of one string to another.
    !!
    !! @param[in,out] x The assignee.
    !! @param[in] y The string to copy.
    module subroutine str_equals(x, y)
        ! Arguments
        class(string), intent(inout) :: x
        class(string), intent(in) :: y

        ! Process
        if (allocated(x%str)) deallocate(x%str)
        x%str = y%str
    end subroutine

! --------------------
    !> @brief Assigns the contents of one string to another.
    !!
    !! @param[in,out] x The assignee.
    !! @param[in] y The string to copy.
    module subroutine str_equals_char(x, y)
        ! Arguments
        class(string), intent(inout) :: x
        character(len = *), intent(in) :: y

        ! Process
        if (allocated(x%str)) deallocate(x%str)
        x%str = y
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests for equality between two strings.
    !!
    !! @param[in] str1 The first string.
    !! @param[in] str2 The second string.
    !!
    !! @return Returns true if @p str1 is equal to @p str2; else, returns false.
    pure module function str_compare_char1(str1, str2) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str1
        class(string), intent(in) :: str2
        logical :: rst

        ! Process
        rst = str1 == str2%str
    end function

! --------------------
    !> @brief Tests for equality between two strings.
    !!
    !! @param[in] str1 The first string.
    !! @param[in] str2 The second string.
    !!
    !! @return Returns true if @p str1 is equal to @p str2; else, returns false.
    pure module function str_compare_char2(str1, str2) result(rst)
        ! Arguments
        class(string), intent(in) :: str1
        character(len = *), intent(in) :: str2
        logical :: rst

        ! Process
        rst = str1%str == str2
    end function

! --------------------
    !> @brief Tests for equality between two strings.
    !!
    !! @param[in] str1 The first string.
    !! @param[in] str2 The second string.
    !!
    !! @return Returns true if @p str1 is equal to @p str2; else, returns false.
    pure module function str_compare_str(str1, str2) result(rst)
        ! Arguments
        class(string), intent(in) :: str1, str2
        logical :: rst

        ! Process
        rst = str1%str == str2%str
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests for inequality between two strings.
    !!
    !! @param[in] str1 The first string.
    !! @param[in] str2 The second string.
    !!
    !! @return Returns true if @p str1 is not equal to @p str2; else, returns 
    !! false.
    pure module function str_ne_compare_char1(str1, str2) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str1
        class(string), intent(in) :: str2
        logical :: rst

        ! Process
        rst = str1 /= str2%str
    end function

! --------------------
    !> @brief Tests for inequality between two strings.
    !!
    !! @param[in] str1 The first string.
    !! @param[in] str2 The second string.
    !!
    !! @return Returns true if @p str1 is not equal to @p str2; else, returns 
    !! false.
    pure module function str_ne_compare_char2(str1, str2) result(rst)
        ! Arguments
        class(string), intent(in) :: str1
        character(len = *), intent(in) :: str2
        logical :: rst

        ! Process
        rst = str1%str /= str2
    end function

! --------------------
    !> @brief Tests for inequality between two strings.
    !!
    !! @param[in] str1 The first string.
    !! @param[in] str2 The second string.
    !!
    !! @return Returns true if @p str1 is not equal to @p str2; else, returns 
    !! false.
    pure module function str_ne_compare_str(str1, str2) result(rst)
        ! Arguments
        class(string), intent(in) :: str1, str2
        logical :: rst

        ! Process
        rst = str1%str /= str2%str
    end function

! ------------------------------------------------------------------------------
    !> @brief Trims any trailing whitespace from the string.
    !!
    !! @param[in] str The string on which to operate.
    !!
    !! @return The resulting string.
    pure module function str_trim(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        type(string) :: rst

        ! Process
        rst%str = trim(str%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Adjusts the contents of the string to the left by trimming leading
    !! whitespaces.
    !!
    !! @param[in] str The string on which to operate.
    !!
    !! @return The resulting string.
    pure module function str_trim_leading(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        type(string) :: rst

        ! Process
        rst%str = trim(adjustl(str%str))
    end function

! ------------------------------------------------------------------------------
    !> @brief Returns the length of the string.
    !!
    !! @param[in] str The string to test.
    !!
    !! @return The length of the string.
    pure module function str_length(str) result(rst)
        ! Arguments
        class(string), intent(in) :: str
        integer(int32) :: rst

        ! Process
        rst = len(str%str)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a Fortran string to a C-style string.
    !!
    !! @param[in] str The Fortran string to convert.
    !! @param[out] cstr The null-terminated C-string.
    !! @param[in,out] csize On input, the size of the character buffer @p cstr.
    !!  On output, the actual number of characters (not including the null
    !!  character) written to  @p cstr.
    module subroutine to_c_string_char(str, cstr, csize)
        ! Arguments
        character(len = *), intent(in) :: str
        character(kind = c_char), intent(out) :: cstr(*)
        integer(int32), intent(inout) :: csize

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = min(len(str), csize - 1)    ! -1 accounts for null character
        do i = 1, n
            cstr(i) = str(i:i)
        end do
        cstr(n + 1) = c_null_char
        csize = n
    end subroutine

! --------------------
    !> @brief Converts a Fortran string to a C-style string.
    !!
    !! @param[in] str The Fortran string to convert.
    !! @param[out] cstr The null-terminated C-string.
    !! @param[in,out] csize On input, the size of the character buffer @p cstr.
    !!  On output, the actual number of characters (not including the null
    !!  character) written to  @p cstr.
    module subroutine to_c_string_str(str, cstr, csize)
        ! Arguments
        class(string), intent(in) :: str
        character(kind = c_char), intent(out) :: cstr(*)
        integer(int32), intent(inout) :: csize

        ! Process
        call to_c_string_char(str%str, cstr, csize)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Converts a C-style string to a Fortran string.
    !!
    !! @param[in] str The C-style string to convert.
    !! @param[in] nchar The length of @p str, not including the null character.
    !!
    !! @return The resulting Fortran string.
    pure module function to_fortran_string(str, nchar) result(rst)
        ! Arguments
        character(kind = c_char), intent(in) :: str(*)
        integer(int32), intent(in) :: nchar
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32) :: i

        ! Process
        allocate(character(len = nchar) :: rst)
        do i = 1, nchar
            rst(i:i) = str(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Determines the length of a C-type null-terminated string.
    !!
    !! @param[in] str The C-style string.
    !!
    !! @return The length of @p str.
    pure module function c_string_length(cstring) result(rst)
        ! Arguments
        character(kind = c_char, len = *), intent(in) :: cstring
        integer(int32) :: rst

        ! Process
        rst = 0
        do while (rst < len_trim(cstring))
            rst = rst + 1
            if (cstring(rst:rst) == C_NULL_CHAR) then
                rst = rst - 1
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
end submodule
