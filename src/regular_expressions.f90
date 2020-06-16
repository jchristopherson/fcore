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
    end interface

! ******************************************************************************
! FORTRAN INTERFACES
! ------------------------------------------------------------------------------
    !> @brief Looks for sequences that match the requested pattern.
    interface regex_match
        module procedure :: regex_match_char
        module procedure :: regex_match_str
    end interface

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
    !> @brief Looks for sequences that match the requested pattern.
    !!
    !! @param[in] src The string to search.
    !! @param[in] pattern The pattern to match.
    !!
    !! @return A list of all matching sequences in @p src.
    function regex_match_char(src, pattern) result(rst)
        ! Arguments
        character(len = *), intent(in) :: src, pattern
        type(string), allocatable, dimension(:) :: rst

        ! Parameters
        integer(int32), parameter :: bufferSize = 1024
        integer(int32), parameter :: numBuffers = 1024

        ! Local Variables
        integer(int32) :: i, nsrc, nptrn, nitems, n
        type(c_ptr), allocatable, dimension(:) :: buffer
        character(kind = c_char, len = bufferSize), allocatable, target, &
            dimension(:) :: bufferStrings
        character(kind = c_char), allocatable, dimension(:) :: csrc, cpattern
        integer(int32), allocatable, dimension(:) :: sizeList

        ! Get the location of each buffer string
        allocate(buffer(numBuffers))
        allocate(bufferStrings(numBuffers))
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
    !> @brief Looks for sequences that match the requested pattern.
    !!
    !! @param[in] src The string to search.
    !! @param[in] pattern The pattern to match.
    !!
    !! @return A list of all matching sequences in @p src.
    function regex_match_str(src, pattern) result(rst)
        ! Arguments
        class(string), intent(in) :: src, pattern
        type(string), allocatable, dimension(:) :: rst

        ! Process
        rst = regex_match_char(src%str, pattern%str)
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
