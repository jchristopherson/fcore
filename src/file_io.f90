! file_io.f90

!> @brief This module contains routines to support file I/O operations.
module file_io
    use iso_fortran_env
    use ferror
    use fcore_constants
    use strings
    implicit none
    private
    public :: file_manager
    public :: text_writer
    public :: file_reader
    public :: text_reader

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a base type for managint file I/O.
    type file_manager
    private
        !> @brief The unit value.
        integer(int32) :: m_unit = -1
        !> @brief The filename.
        character(len = :), allocatable :: m_fname
    contains
        !> @brief Forces closure of the file, if open, whenever the object goes
        !! out of scope.
        final :: fm_clean_up
        !> @brief Returns the unit value for the file object.
        procedure, public :: get_unit => fm_get_unit
        !> @brief Sets the unit value for the file object.
        procedure, private :: set_unit => fm_set_unit
        ! !> @brief Creates a new, unused unit value to identify the file.
        ! procedure, public :: create_new_unit => fm_new_unit
        !> @brief Determines if the file is already opened.
        procedure, public :: is_open => fm_get_is_opened
        !> @brief Closes the file.
        procedure, public :: close => fm_close
        !> @brief Gets the filename.
        procedure, public :: get_filename => fm_get_fname
        !> @brief Sets the filename.
        procedure, public :: set_filename => fm_set_fname
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for writing text files.
    type, extends(file_manager) :: text_writer
    contains
        !> @brief Opens a text file for writing.
        procedure, public :: open => tw_open
        !> @brief Writes text to the file, but does not advance to the next 
        !! line.
        procedure, public :: write => tw_write_txt
        !> @brief Writes text to the file, but does advance to the next line.
        procedure, public :: write_line => tw_write_txt_line
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for reading files.
    type, extends(file_manager) :: file_reader
        !> @brief The current file position.
        integer(int32) :: m_position = 0
    contains
        !> @brief Gets the current position within the file.
        procedure, public :: get_position => fr_get_position
        !> @brief Sets the position within the file.
        procedure, public :: set_position => fr_set_position
        !> @brief Moves the current position to the start of the file.
        procedure, public :: move_to_start => fr_move_to_start
        !> @brief Tests to see if the current position denotes the end-of-file.
        procedure, public :: is_end_of_file => fr_eof
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for reading text files.
    type, extends(file_reader) :: text_reader
    contains
        !> @brief Opens a text file for reading.
        procedure, public :: open => tr_open
        !> @brief Reads the entire contents of an ASCII text file into a string.
        procedure, public :: read_all => tr_read_full_file
        !> @brief Reads a single character from an ASCII text file.
        procedure, public :: read_char => tr_read_char
        !> @brief Reads a single line from an ASCII text file.
        procedure, public :: read_line => tr_read_line
        !> @brief Reads the entire contents of an ASCII text file into a string,
        !! and breaks the contents into lines.
        procedure, public :: read_lines => tr_read_lines
    end type

! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    interface
        module function fm_get_unit(this) result(rst)
            class(file_manager), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine fm_set_unit(this, x)
            class(file_manager), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        module subroutine fm_new_unit(this)
            class(file_manager), intent(inout) :: this
        end subroutine

        module function fm_get_is_opened(this) result(rst)
            class(file_manager), intent(in) :: this
            logical :: rst
        end function

        module subroutine fm_close(this, del)
            class(file_manager), intent(inout) :: this
            logical, intent(in), optional :: del
        end subroutine

        module subroutine fm_clean_up(this)
            type(file_manager), intent(inout) :: this
        end subroutine

        pure module function fm_get_fname(this) result(rst)
            class(file_manager), intent(in) :: this
            character(len = :), allocatable :: rst
        end function

        module subroutine fm_set_fname(this, x)
            class(file_manager), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    interface
        module subroutine tw_open(this, fname, append, err)
            class(text_writer), intent(inout) :: this
            character(len = *), intent(in) :: fname
            logical, intent(in), optional :: append
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine tw_write_txt(this, txt, err)
            class(text_writer), intent(in) :: this
            character(len = *), intent(in) :: txt
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine tw_write_txt_line(this, txt, err)
            class(text_writer), intent(in) :: this
            character(len = *), intent(in) :: txt
            class(errors), intent(inout), optional, target :: err
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    interface
        pure module function fr_get_position(this) result(rst)
            class(file_reader), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine fr_set_position(this, x)
            class(file_reader), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        module subroutine fr_move_to_start(this)
            class(file_reader), intent(inout) :: this
        end subroutine

        module function fr_eof(this) result(rst)
            class(file_reader), intent(in) :: this
            logical :: rst
        end function
    end interface

! ------------------------------------------------------------------------------
    interface
        module subroutine tr_open(this, fname, err)
            class(text_reader), intent(inout) :: this
            character(len = *), intent(in) :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function tr_read_full_file(this, err) result(rst)
            class(text_reader), intent(in) :: this
            class(errors), intent(inout), optional, target :: err
            character(len = :), allocatable :: rst
        end function

        module function tr_read_char(this, err) result(rst)
            class(text_reader), intent(inout) :: this
            class(errors), intent(inout), optional, target :: err
            character :: rst
        end function

        module function tr_read_line(this, err) result(rst)
            class(text_reader), intent(inout) :: this
            class(errors), intent(inout), optional, target :: err
            character(len = :), allocatable :: rst
        end function

        module function tr_read_lines(this, err) result(rst)
            class(text_reader), intent(inout) :: this
            class(errors), intent(inout), optional, target :: err
            type(string), allocatable, dimension(:) :: rst
        end function
    end interface

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
