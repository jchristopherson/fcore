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
    public :: replace
    public :: remove
    public :: remove_at
    public :: to_upper
    public :: to_lower
    public :: contains_string
    public :: parse_real64
    public :: parse_real32
    public :: parse_int8
    public :: parse_int16
    public :: parse_int32
    public :: parse_int64
    public :: to_string
    public :: operator(+)
    public :: assignment(=)
    public :: operator(==)
    public :: operator(/=)

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a string type.
    type string
        !> @brief The string.
        character(len = :), allocatable :: str
    contains
        !> @brief Splits a string into substrings marked by the specified 
        !! delimiter string.
        procedure, public :: split => split_string_str
        !> @brief Finds all occurrences of a substring within a string.
        procedure, public :: index_of => index_of_all_str
        !> @brief Replaces all occurrences of a substring within the original 
        !! string.
        procedure, public :: replace => replace_str
        !> @brief Removes all occurrences of a substring within the original 
        !! string.
        procedure, public :: remove => remove_str
        !> @brief Removes the specified number of characters from the string at 
        !! the specified location.
        procedure, public :: remove_at => remove_at_str
        !> @brief Converts an ASCII character string to all upper case.
        procedure, public :: to_upper => to_upper_str
        !> @brief Converts an ASCII character string to all lower case.
        procedure, public :: to_lower => to_lower_str
        !> @brief Tests to see if a substring exists within a parent string.
        procedure, public :: contains_string => contains_str
        !> @brief Trims any trailing whitespace from the string.
        procedure, public :: trim => str_trim
        !> @brief Adjusts the contents of the string to the left by trimming 
        !! leading whitespaces.
        procedure, public :: trim_leading => str_trim_leading
        !> @brief Returns the length of the string.
        procedure, public :: length => str_length
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
    !> @brief Combines two strings.
    interface operator(+)
        module procedure :: add_str
        module procedure :: add_char1
        module procedure :: add_char2
    end interface

! ------------------------------------------------------------------------------
    !> @brief Assigns the contents of one string to another.
    interface assignment(=)
        module procedure :: str_equals
        module procedure :: str_equals_char
    end interface

! ------------------------------------------------------------------------------
    !> @brief Tests for equality between two strings.
    interface operator(==)
        module procedure :: str_compare_char1
        module procedure :: str_compare_char2
        module procedure :: str_compare_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Tests for inequality between two strings.
    interface operator(/=)
        module procedure :: str_ne_compare_char1
        module procedure :: str_ne_compare_char2
        module procedure :: str_ne_compare_str
    end interface

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
            class(string), intent(in) :: str, sub
            integer(int32), allocatable, dimension(:) :: rst
        end function

        pure module function split_string_char(txt, delim) result(rst)
            character(len = *), intent(in) :: txt, delim
            type(string), allocatable, dimension(:) :: rst
        end function

        pure module function split_string_str(txt, delim) result(rst)
            class(string), intent(in) :: txt, delim
            type(string), allocatable, dimension(:) :: rst
        end function

        pure module function replace_char(str, substr, newstr) result(rst)
            character(len = *), intent(in) :: str, substr, newstr
            character(len = :), allocatable :: rst
        end function

        pure module function replace_str(str, substr, newstr) result(rst)
            class(string), intent(in) :: str, substr, newstr
            type(string) :: rst
        end function

        pure module function remove_char(str, substr) result(rst)
            character(len = *), intent(in) :: str, substr
            character(len = :), allocatable :: rst
        end function

        pure module function remove_str(str, substr) result(rst)
            class(string), intent(in) :: str, substr
            type(string) :: rst
        end function

        pure module function remove_at_char(str, ind, nchar) result(rst)
            character(len = *), intent(in) :: str
            integer(int32), intent(in) :: ind, nchar
            character(len = :), allocatable :: rst
        end function

        pure module function remove_at_str(str, ind, nchar) result(rst)
            class(string), intent(in) :: str
            integer(int32), intent(in) :: ind, nchar
            type(string) :: rst
        end function

        pure module function to_upper_char(str) result(rst)
            character(len = *), intent(in) :: str
            character(len = :), allocatable :: rst
        end function

        pure module function to_upper_str(str) result(rst)
            class(string), intent(in) :: str
            type(string) :: rst
        end function

        pure module function to_lower_char(str) result(rst)
            character(len = *), intent(in) :: str
            character(len = :), allocatable :: rst
        end function

        pure module function to_lower_str(str) result(rst)
            class(string), intent(in) :: str
            type(string) :: rst
        end function

        pure module function contains_char(str, substr) result(rst)
            character(len = *), intent(in) :: str, substr
            logical :: rst
        end function

        pure module function contains_str(str, substr) result(rst)
            class(string), intent(in) :: str, substr
            logical :: rst
        end function

        pure module function parse_real64_char(str) result(rst)
            character(len = *), intent(in) :: str
            real(real64) :: rst
        end function

        pure module function parse_real64_str(str) result(rst)
            class(string), intent(in) :: str
            real(real64) :: rst
        end function

        pure module function parse_real32_char(str) result(rst)
            character(len = *), intent(in) :: str
            real(real32) :: rst
        end function

        pure module function parse_real32_str(str) result(rst)
            class(string), intent(in) :: str
            real(real32) :: rst
        end function

        pure module function parse_int8_char(str) result(rst)
            character(len = *), intent(in) :: str
            integer(int8) :: rst
        end function

        pure module function parse_int8_str(str) result(rst)
            class(string), intent(in) :: str
            integer(int8) :: rst
        end function

        pure module function parse_int16_char(str) result(rst)
            character(len = *), intent(in) :: str
            integer(int16) :: rst
        end function

        pure module function parse_int16_str(str) result(rst)
            class(string), intent(in) :: str
            integer(int16) :: rst
        end function

        pure module function parse_int32_char(str) result(rst)
            character(len = *), intent(in) :: str
            integer(int32) :: rst
        end function

        pure module function parse_int32_str(str) result(rst)
            class(string), intent(in) :: str
            integer(int32) :: rst
        end function

        pure module function parse_int64_char(str) result(rst)
            character(len = *), intent(in) :: str
            integer(int64) :: rst
        end function

        pure module function parse_int64_str(str) result(rst)
            class(string), intent(in) :: str
            integer(int64) :: rst
        end function

        pure module function to_string_r64(num, fmt) result(rst)
            real(real64), intent(in) :: num
            character(len = *), intent(in), optional :: fmt
            character(len = :), allocatable :: rst
        end function

        pure module function to_string_r32(num, fmt) result(rst)
            real(real32), intent(in) :: num
            character(len = *), intent(in), optional :: fmt
            character(len = :), allocatable :: rst
        end function

        pure module function to_string_i8(num) result(rst)
            integer(int8), intent(in) :: num
            character(len = :), allocatable :: rst
        end function

        pure module function to_string_i16(num) result(rst)
            integer(int16), intent(in) :: num
            character(len = :), allocatable :: rst
        end function

        pure module function to_string_i32(num) result(rst)
            integer(int32), intent(in) :: num
            character(len = :), allocatable :: rst
        end function

        pure module function to_string_i64(num) result(rst)
            integer(int64), intent(in) :: num
            character(len = :), allocatable :: rst
        end function

        pure module function to_string_c64(num, fmt) result(rst)
            complex(real64), intent(in) :: num
            character(len = *), intent(in), optional :: fmt
            character(len = :), allocatable :: rst
        end function

        pure module function to_string_c32(num, fmt) result(rst)
            complex(real32), intent(in) :: num
            character(len = *), intent(in), optional :: fmt
            character(len = :), allocatable :: rst
        end function

        pure module function add_str(str1, str2) result(rst)
            class(string), intent(in) :: str1, str2
            type(string) :: rst
        end function

        pure module function add_char1(str1, str2) result(rst)
            character(len = *), intent(in) :: str1
            class(string), intent(in) :: str2
            type(string) :: rst
        end function

        pure module function add_char2(str1, str2) result(rst)
            class(string), intent(in) :: str1
            character(len = *), intent(in) :: str2
            type(string) :: rst
        end function

        module subroutine str_equals(x, y)
            class(string), intent(inout) :: x
            class(string), intent(in) :: y
        end subroutine

        module subroutine str_equals_char(x, y)
            class(string), intent(inout) :: x
            character(len = *), intent(in) :: y
        end subroutine

        pure module function str_compare_char1(str1, str2) result(rst)
            character(len = *), intent(in) :: str1
            class(string), intent(in) :: str2
            logical :: rst
        end function

        pure module function str_compare_char2(str1, str2) result(rst)
            class(string), intent(in) :: str1
            character(len = *), intent(in) :: str2
            logical :: rst
        end function

        pure module function str_compare_str(str1, str2) result(rst)
            class(string), intent(in) :: str1, str2
            logical :: rst
        end function

        pure module function str_ne_compare_char1(str1, str2) result(rst)
            character(len = *), intent(in) :: str1
            class(string), intent(in) :: str2
            logical :: rst
        end function

        pure module function str_ne_compare_char2(str1, str2) result(rst)
            class(string), intent(in) :: str1
            character(len = *), intent(in) :: str2
            logical :: rst
        end function

        pure module function str_ne_compare_str(str1, str2) result(rst)
            class(string), intent(in) :: str1, str2
            logical :: rst
        end function

        pure module function str_trim(str) result(rst)
            class(string), intent(in) :: str
            type(string) :: rst
        end function

        pure module function str_trim_leading(str) result(rst)
            class(string), intent(in) :: str
            type(string) :: rst
        end function

        pure module function str_length(str) result(rst)
            class(string), intent(in) :: str
            integer(int32) :: rst
        end function
    end interface

! ******************************************************************************
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
    !> @brief Replaces all occurrences of a substring within the original 
    !! string.
    interface replace
        module procedure :: replace_char
        module procedure :: replace_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Removes all occurrences of a substring within the original string.
    interface remove
        module procedure :: remove_char
        module procedure :: remove_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Removes the specified number of characters from the string at the
    !! specified location.
    interface remove_at
        module procedure :: remove_at_char
        module procedure :: remove_at_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Converts an ASCII character string to all upper case.
    interface to_upper
        module procedure :: to_upper_char
        module procedure :: to_upper_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Converts an ASCII character string to all lower case.
    interface to_lower
        module procedure :: to_lower_char
        module procedure :: to_lower_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Tests to see if a substring exists within a parent string.
    interface contains_string
        module procedure :: contains_char
        module procedure :: contains_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Attempts to parse a string to a 64-bit floating-point value.
    interface parse_real64
        module procedure :: parse_real64_char
        module procedure :: parse_real64_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Attempts to parse a string to a 32-bit floating-point value.
    interface parse_real32
        module procedure :: parse_real32_char
        module procedure :: parse_real32_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Attempts to parse a string to an 8-bit integer value.
    interface parse_int8
        module procedure :: parse_int8_char
        module procedure :: parse_int8_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Attempts to parse a string to a 16-bit integer value.
    interface parse_int16
        module procedure :: parse_int16_char
        module procedure :: parse_int16_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Attempts to parse a string to a 32-bit integer value.
    interface parse_int32
        module procedure :: parse_int32_char
        module procedure :: parse_int32_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Attempts to parse a string to a 64-bit integer value.
    interface parse_int64
        module procedure :: parse_int64_char
        module procedure :: parse_int64_str
    end interface

! ------------------------------------------------------------------------------
    !> @brief Converts a number to a string.
    interface to_string
        module procedure :: to_string_r64
        module procedure :: to_string_r32
        module procedure :: to_string_i8
        module procedure :: to_string_i16
        module procedure :: to_string_i32
        module procedure :: to_string_i64
        module procedure :: to_string_c64
        module procedure :: to_string_c32
    end interface

! ------------------------------------------------------------------------------
end module
