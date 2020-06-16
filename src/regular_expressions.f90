! regular_expressions.f90

! https://docs.microsoft.com/en-us/cpp/standard-library/regex?view=vs-2019

!> @brief A collection of routines providing regular expression support.
module regular_expressions
    use iso_fortran_env
    use iso_c_binding
    use strings
    implicit none
    private
    public :: regex_match
    public :: regex_search
    public :: regex_replace

! ******************************************************************************
! C INTERFACE
! ------------------------------------------------------------------------------
    interface
        function c_regex_match(src, pattern, numbuff, buffsizes, buffer, &
                itemsizes) result(rst) bind(C, name = "c_regex_match")
            ! Arguments
            use iso_c_binding
            character(kind = c_char), intent(in) :: src(*), pattern(*)
            integer(c_int), intent(in), value :: numbuff, buffsizes
            type(c_ptr), intent(out) :: buffer(numbuff)
            integer(c_int), intent(out) :: itemsizes(numbuff)
            integer(c_int) :: rst
        end function

        function c_regex_search(src, pattern, numbuff, buffsizes, buffer, &
                itemsizes) result(rst) bind(C, name = "c_regex_search")
            ! Arguments
            use iso_c_binding
            character(kind = c_char), intent(in) :: src(*), pattern(*)
            integer(c_int), intent(in), value :: numbuff, buffsizes
            type(c_ptr), intent(out) :: buffer(numbuff)
            integer(c_int), intent(out) :: itemsizes(numbuff)
            integer(c_int) :: rst
        end function

        subroutine c_regex_replace(src, pattern, rplc, buffsize, buffer, &
                nbuff) bind(C, name = "c_regex_replace")
            ! Arguments
            use iso_c_binding
            character(kind = c_char), intent(in) :: src(*), pattern(*), rplc(*)
            integer(c_int), intent(in), value :: buffsize
            character(kind = c_char), intent(out) :: buffer(*)
            integer(c_int), intent(out) :: nbuff
        end subroutine
    end interface

! ******************************************************************************
! FORTRAN INTERFACES
! ------------------------------------------------------------------------------
    !> @brief Looks for sequences that match the requested pattern.  The entire
    !! target sequence must match the regular expression for this function to 
    !! succeed.  Utilize regex_search if only a partial match is required.
    interface regex_match
        module procedure :: regex_match_char
        module procedure :: regex_match_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Looks for sequences that match the requested pattern.
    interface regex_search
        module procedure :: regex_search_char
        module procedure :: regex_search_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief
    interface regex_replace
        module procedure :: regex_replace_char
        module procedure :: regex_replace_str
    end interface
contains
! ------------------------------------------------------------------------------
    !> @brief Looks for sequences that match the requested pattern.  The entire
    !! target sequence must match the regular expression for this function to 
    !! succeed.
    !!
    !! @param[in] src The string to search.
    !! @param[in] pattern The pattern to match.
    !! @param[in] buffsz An optional input that allows the user to control the
    !!  size of each internal storage buffer.  The default is 1024 characters.
    !! @param[in] nbuff An optional input that allows the user to control the
    !!  quantity of internal storage buffers available to the regular expression
    !!  parser.  The default is 1024.
    !!
    !! @return A list of all matching sequences in @p src.
    function regex_match_char(src, pattern, buffsz, nbuff) result(rst)
        ! Arguments
        character(len = *), intent(in) :: src, pattern
        integer(int32), intent(in), optional :: buffsz, nbuff
        type(string), allocatable, dimension(:) :: rst

        ! Parameters
        integer(int32), parameter :: defaultBufferSize = 1024
        integer(int32), parameter :: defaultNumBuffers = 1024

        ! Local Variables
        integer(int32) :: i, nsrc, nptrn, nitems, n, numBuffers, bufferSize
        type(c_ptr), allocatable, dimension(:) :: buffer
        character(kind = c_char, len = :), allocatable, target, &
            dimension(:) :: bufferStrings
        character(kind = c_char), allocatable, dimension(:) :: csrc, cpattern
        integer(int32), allocatable, dimension(:) :: sizeList

        ! Initialization
        if (present(buffsz)) then
            bufferSize = buffsz
            if (bufferSize < 1) bufferSize = defaultBufferSize
        else
            bufferSize = defaultBufferSize
        end if
        if (present(nbuff)) then
            numBuffers = nbuff
            if (numBuffers < 1) numBuffers = defaultNumBuffers
        else
            numBuffers = defaultNumBuffers
        end if

        ! Get the location of each buffer string
        allocate(buffer(numBuffers))
        allocate(character(kind = c_char, len = bufferSize) :: &
            bufferStrings(numBuffers))
        do i = 1, numBuffers
            buffer(i) = c_loc(bufferStrings(i))
        end do
        
        ! Convert the input strings into something useful for C
        nsrc = len(src) + 1
        nptrn = len(pattern) + 1    ! +1 allows for null character
        allocate(csrc(nsrc))
        allocate(cpattern(nptrn))
        call to_c_string(src, csrc, nsrc)
        call to_c_string(pattern, cpattern, nptrn)

        ! Perform the operation
        allocate(sizeList(numBuffers))
        nitems = c_regex_match(csrc, cpattern, numBuffers, bufferSize, &
            buffer, sizeList)

        ! Define output
        allocate(rst(nitems))
        do i = 1, nitems
            n = sizeList(i)
            rst(i)%str = bufferStrings(i)(1:n)
        end do
    end function

! --------------------
    !> @brief Looks for sequences that match the requested pattern.  The entire
    !! target sequence must match the regular expression for this function to 
    !! succeed.
    !!
    !! @param[in] src The string to search.
    !! @param[in] pattern The pattern to match.
    !! @param[in] buffsz An optional input that allows the user to control the
    !!  size of each internal storage buffer.  The default is 1024 characters.
    !! @param[in] nbuff An optional input that allows the user to control the
    !!  quantity of internal storage buffers available to the regular expression
    !!  parser.  The default is 1024.
    !!
    !! @return A list of all matching sequences in @p src.
    function regex_match_str(src, pattern, buffsz, nbuff) result(rst)
        ! Arguments
        class(string), intent(in) :: src, pattern
        integer(int32), intent(in), optional :: buffsz, nbuff
        type(string), allocatable, dimension(:) :: rst

        ! Process
        rst = regex_match_char(src%str, pattern%str, buffsz, nbuff)
    end function

! ------------------------------------------------------------------------------
    !> @brief Looks for sequences that match the requested pattern.
    !!
    !! @param[in] src The string to search.
    !! @param[in] pattern The pattern to match.
    !! @param[in] buffsz An optional input that allows the user to control the
    !!  size of each internal storage buffer.  The default is 1024 characters.
    !! @param[in] nbuff An optional input that allows the user to control the
    !!  quantity of internal storage buffers available to the regular expression
    !!  parser.  The default is 1024.
    !!
    !! @return A list of all matching sequences in @p src.
    function regex_search_char(src, pattern, buffsz, nbuff) result(rst)
        ! Arguments
        character(len = *), intent(in) :: src, pattern
        integer(int32), intent(in), optional :: buffsz, nbuff
        type(string), allocatable, dimension(:) :: rst

        ! Parameters
        integer(int32), parameter :: defaultBufferSize = 1024
        integer(int32), parameter :: defaultNumBuffers = 1024

        ! Local Variables
        integer(int32) :: i, nsrc, nptrn, nitems, n, numBuffers, bufferSize
        type(c_ptr), allocatable, dimension(:) :: buffer
        character(kind = c_char, len = :), allocatable, target, &
            dimension(:) :: bufferStrings
        character(kind = c_char), allocatable, dimension(:) :: csrc, cpattern
        integer(int32), allocatable, dimension(:) :: sizeList

        ! Initialization
        if (present(buffsz)) then
            bufferSize = buffsz
            if (bufferSize < 1) bufferSize = defaultBufferSize
        else
            bufferSize = defaultBufferSize
        end if
        if (present(nbuff)) then
            numBuffers = nbuff
            if (numBuffers < 1) numBuffers = defaultNumBuffers
        else
            numBuffers = defaultNumBuffers
        end if

        ! Get the location of each buffer string
        allocate(buffer(numBuffers))
        allocate(character(kind = c_char, len = bufferSize) :: &
            bufferStrings(numBuffers))
        do i = 1, numBuffers
            buffer(i) = c_loc(bufferStrings(i))
        end do
        
        ! Convert the input strings into something useful for C
        nsrc = len(src) + 1
        nptrn = len(pattern) + 1    ! +1 allows for null character
        allocate(csrc(nsrc))
        allocate(cpattern(nptrn))
        call to_c_string(src, csrc, nsrc)
        call to_c_string(pattern, cpattern, nptrn)

        ! Perform the operation
        allocate(sizeList(numBuffers))
        nitems = c_regex_search(csrc, cpattern, numBuffers, bufferSize, &
            buffer, sizeList)

        ! Define output
        allocate(rst(nitems))
        do i = 1, nitems
            n = sizeList(i)
            rst(i)%str = bufferStrings(i)(1:n)
        end do
    end function

! --------------------
    !> @brief Looks for sequences that match the requested pattern.
    !!
    !! @param[in] src The string to search.
    !! @param[in] pattern The pattern to match.
    !! @param[in] buffsz An optional input that allows the user to control the
    !!  size of each internal storage buffer.  The default is 1024 characters.
    !! @param[in] nbuff An optional input that allows the user to control the
    !!  quantity of internal storage buffers available to the regular expression
    !!  parser.  The default is 1024.
    !!
    !! @return A list of all matching sequences in @p src.
    function regex_search_str(src, pattern, buffsz, nbuff) result(rst)
        ! Arguments
        class(string), intent(in) :: src, pattern
        integer(int32), intent(in), optional :: buffsz, nbuff
        type(string), allocatable, dimension(:) :: rst

        ! Process
        rst = regex_search_char(src%str, pattern%str, buffsz, nbuff)
    end function

! ------------------------------------------------------------------------------
    !
    function regex_replace_char(src, pattern, rplc) result(rst)
        ! Arguments
        character(len = *), intent(in) :: src, pattern, rplc
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32) :: nsrc, nptrn, nrplc, nbuffer, nout
        character(kind = c_char), allocatable, dimension(:) :: csrc, &
            cpattern, crplc, buffer

        ! Initialization
        nsrc = len(src) + 1
        nptrn = len(pattern) + 1    ! +1 allows for null character
        nrplc = len(rplc) + 1
        nbuffer = nrplc * nsrc
        allocate(csrc(nsrc))
        allocate(cpattern(nptrn))
        allocate(crplc(nrplc))
        allocate(buffer(nbuffer))

        ! Convert input Fortran strings to C-friendly versions
        call to_c_string(src, csrc, nsrc)
        call to_c_string(pattern, cpattern, nptrn)
        call to_c_string(rplc, crplc, nrplc)

        ! Process
        call c_regex_replace(csrc, cpattern, crplc, nbuffer, buffer, nout)

        ! Return the C string output as a Fortran string
        rst = to_fortran_string(buffer, nout)
    end function

! --------------------
    !
    function regex_replace_str(src, pattern, rplc) result(rst)
        ! Arguments
        class(string), intent(in) :: src, pattern, rplc
        type(string) :: rst

        ! Process
        rst%str = regex_replace_char(src%str, pattern%str, rplc%str)
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
