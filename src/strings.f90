! strings.f90

!> @brief Defines string handling routines.
module strings
    use iso_fortran_env
    implicit none
    private
    public :: string
    public :: string_builder
    public :: index_of_all
    public :: split_string

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a string type.
    type string
        !> @brief The string.
        character(len = :), allocatable :: str
    end type

! ------------------------------------------------------------------------------
    !> @brief Provides a mechanism for building strings.
    type string_builder
    private
        !> @brief The actual length of the string.
        integer(int32) :: m_length = 0
        !> @brief The string buffer.
        character(len = :), allocatable :: m_buffer
    contains
        !> @brief Initializes the string_builder object.
        procedure, public :: initialize => sb_init
        !> @brief Appends to the string.
        procedure, public :: append => sb_append
        !> @brief Returns the contents as a single string.
        procedure, public :: to_string => sb_to_string
        !> @brief Gets the current length of the string being built.
        procedure, public :: get_length => sb_get_length
        !> @brief Clears the buffer.
        procedure, public :: clear => sb_clear
    end type

! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    interface
        module subroutine sb_init(this)
            class(string_builder), intent(inout) :: this
        end subroutine

        module subroutine sb_append(this, txt)
            class(string_builder), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        pure module function sb_to_string(this) result(txt)
            class(string_builder), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        pure module function sb_get_length(this) result(n)
            class(string_builder), intent(in) :: this
            integer(int32) :: n
        end function

        module subroutine sb_clear(this)
            class(string_builder), intent(inout) :: this
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    interface
        pure module function index_of_all_chars(str, sub) result(rst)
            character(len = *), intent(in) :: str, sub
            integer(int32), allocatable, dimension(:) :: rst
        end function

        pure module function index_of_all_str(str, sub) result(rst)
            type(string), intent(in) :: str, sub
            integer(int32), allocatable, dimension(:) :: rst
        end function

        pure module function split_string_char(txt, delim) result(rst)
            character(len = *), intent(in) :: txt, delim
            type(string), allocatable, dimension(:) :: rst
        end function

        pure module function split_string_str(txt, delim) result(rst)
            type(string), intent(in) :: txt, delim
            type(string), allocatable, dimension(:) :: rst
        end function
    end interface
! ------------------------------------------------------------------------------
    !> @brief Finds all occurrences of a substring within a string.
    interface index_of_all
        module procedure :: index_of_all_chars
        module procedure :: index_of_all_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Splits a string into substrings marked by the specified delimiter
    !! string.
    interface split_string
        module procedure :: split_string_char
        module procedure :: split_string_str
    end interface

! ------------------------------------------------------------------------------
end module
