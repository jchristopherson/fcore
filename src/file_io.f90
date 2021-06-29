! file_io.f90

!> @brief This module contains routines to support file I/O operations.
module file_io
    use iso_fortran_env
    use iso_c_binding
    use ferror
    use fcore_constants
    use strings
    implicit none
    private
    public :: file_manager
    public :: text_writer
    public :: file_reader
    public :: text_reader
    public :: binary_writer
    public :: binary_reader
    public :: is_little_endian
    public :: swap_bytes
    public :: file_path
    public :: split_path
    public :: folder_contents
    public :: get_folder_contents
    public :: find_all_files

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
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_unit(class(file_manager) this)
        !! @endcode
        !!
        !! @param[in,out] this The file_manager object.
        !!
        !! @return The Fortran unit value for this stream.
        procedure, public :: get_unit => fm_get_unit
        !> @brief Sets the unit value for the file object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_unit(class(file_manager) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The file_manager object.
        !! @param[in] x The Fortran unit value.
        procedure, private :: set_unit => fm_set_unit
        ! !> @brief Creates a new, unused unit value to identify the file.
        ! procedure, public :: create_new_unit => fm_new_unit
        !> @brief Determines if the file is already opened.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical is_open(class(file_manager) this)
        !! @endcode
        !!
        !! @param[in] this The file_manager object.
        !!
        !! @return Returns true if the file is opened; else, false.
        procedure, public :: is_open => fm_get_is_opened
        !> @brief Closes the file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine close(class(file_manager) this, logical del)
        !! @endcode
        !!
        !! @param[in,out] this The file_manager object.
        !! @param[in] del An optional input, that if set, determines if the file
        !!  should be deleted once closed.  The default is false such that the
        !!  file remains.
        procedure, public :: close => fm_close
        !> @brief Gets the filename.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) get_filename(class(file_manager) this)
        !! @endcode
        !!
        !! @param[in] this The file_manager object.
        !! 
        !! @return The filename.
        procedure, public :: get_filename => fm_get_fname
        !> @brief Sets the filename.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_filename(class(file_manager) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The file_manager object.
        !! @param[in] x The filename.
        procedure, public :: set_filename => fm_set_fname
        !> @brief Gets the size of the currently open file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_file_size(class(file_manager) this)
        !! @endcode
        !!
        !! @param[in] this The file_manager object.
        !!
        !! @return The file size, in bytes.
        procedure, public :: get_file_size => fm_get_size
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for writing text files.
    type, extends(file_manager) :: text_writer
    contains
        !> @brief Opens a text file for writing.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine open(class(text_writer) this, character(len = *) fname, logical append, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The text_writer object.
        !! @param[in] fname The name of the file to open.
        !! @param[in] append An optional argument that, if specified, determines
        !!  if the file should be appended.  If not supplied, and a file exists,
        !!  the file will be overwritten.  If no file exists, it simply will be
        !!  created.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be opened.
        procedure, public :: open => tw_open
        !> @brief Writes text to the file, but does not advance to the next 
        !! line.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine write(class(text_writer) this, character(len = *) txt, class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The text_writer object.
        !! @param[in] txt The text to write.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_UNOPENED_ERROR: Occurs if the file has not yet been opened.
        generic, public :: write => tw_write_txt, tw_write_txt_str
        !> @brief Writes text to the file, but does advance to the next line.
        !!
        !! @par Syntax #1
        !! @code{.f90}
        !! subroutine write_line(class(text_writer) this, class(string) txt, class(errors) err)
        !! @endcode
        !!
        !! @par Syntax #2
        !! @code{.f90}
        !! subroutine write_line(class(text_writer) this, character(len = *) txt, class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The text_writer object.
        !! @param[in] txt The text to write.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_UNOPENED_ERROR: Occurs if the file has not yet been opened.
        generic, public :: write_line => tw_write_txt_line, &
            tw_write_txt_line_str

        procedure :: tw_write_txt
        procedure :: tw_write_txt_str
        procedure :: tw_write_txt_line
        procedure :: tw_write_txt_line_str
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for reading files.
    type, extends(file_manager) :: file_reader
    private
        !> @brief The current file position.
        integer(int32) :: m_position = 0
    contains
        !> @brief Gets the current position within the file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_position(class(file_reader) this)
        !! @endcode
        !!
        !! @param[in] this The file_reader object.
        !! @return The current position.
        procedure, public :: get_position => fr_get_position
        !> @brief Sets the position within the file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_position(class(file_reader) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The file_reader object.
        !! @param[in] x The file position.
        procedure, public :: set_position => fr_set_position
        !> @brief Moves the current position to the start of the file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine move_to_start(class(file_reader) this)
        !! @endcode
        !!
        !! @param[in,out] this The file_reader object.
        procedure, public :: move_to_start => fr_move_to_start
        !> @brief Tests to see if the current position denotes the end-of-file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical is_end_of_file(class(file_reader) this)
        !! @endcode
        !!
        !! @param[in] this The file_reader object.
        !!
        !! @return Returns true if the current position is the end-of-file; 
        !!  else, false.
        procedure, public :: is_end_of_file => fr_eof
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for reading text files.
    type, extends(file_reader) :: text_reader
    contains
        !> @brief Opens a text file for reading.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine text_reader(class(text_reader) this, character(len = *) fname, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The text_reader object.
        !! @param[in] fname The name of the file to open.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be opened.
        procedure, public :: open => tr_open
        !> @brief Reads the entire contents of an ASCII text file into a string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) read_all(class(text_reader) this, class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The text_reader object.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_UNOPENED_ERROR: Occurs if the file hasn't been opened.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory
        !!      available.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
        !!
        !! @return The string containing the file contents.  Notice, the line
        !!  termination characters have not been stripped out of the string.
        !!
        !! @par Remarks
        !! Notice, the position indicator is not referenced, or utilized, for 
        !! this read operation.  Regardless of its status, the entire file is 
        !! read.
        procedure, public :: read_all => tr_read_full_file
        !> @brief Reads a single character from an ASCII text file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character read_char(class(text_reader) this, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The text_reader object.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_UNOPENED_ERROR: Occurs if the file hasn't been opened.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory
        !!      available.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
        !!
        !! @return The character.
        !!
        !! @par Remarks
        !! On output, the position indicator is incremented by one character.
        procedure, public :: read_char => tr_read_char
        !> @brief Reads a single line from an ASCII text file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) read_line(class(text_reader) this, class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The text_reader object.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_UNOPENED_ERROR: Occurs if the file hasn't been opened.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory
        !!      available.
        !!
        !! @return The string containing the line contents.
        !!
        !! @par Remarks
        !! On output, the position indicator is incremented to account for the
        !! length of the line, including any termination characters.
        procedure, public :: read_line => tr_read_line
        !> @brief Reads the entire contents of an ASCII text file into a string,
        !! and breaks the contents into lines.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! type(string)(:) read_lines(class(text_reader) this, class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The text_reader object.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_UNOPENED_ERROR: Occurs if the file hasn't been opened.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory
        !!      available.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
        !!
        !! @return An array of strings
        !!
        !! @par Remarks
        !! Notice, the position indicator is not referenced, or utilized, for this
        !! read operation.  Regardless of its status, the entire file is read.
        procedure, public :: read_lines => tr_read_lines
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for writing binary files.
    type, extends(file_reader) :: binary_writer
    private
        !> @brief A buffer used to store data until flushed.
        integer(int8), allocatable, dimension(:) :: m_buffer
        !> @brief The actual number of items in the buffer
        integer(int32) :: m_count = 0
    contains
        !> @brief Forces a write operation on all buffer contents, closes the 
        !! file, and performs any necessary clean-up operations.
        final :: bw_clean_up
        !> @brief Gets the capacity of the buffer, in bytes.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_capacity(class(binary_writer) this)
        !! @endcode
        !!
        !! @param[in] this The binary_writer object.
        !!
        !! @return The buffer capacity, in bytes.
        procedure, public :: get_capacity => bw_get_capacity
        !> @brief Sets the capacity of the buffer, in bytes.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_capacity(class(binary_writer) this, integer(int32) n, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The binary_writer object.
        !! @param[in] n The size, in bytes, to make the buffer.  This value must
        !!  be greater than zero.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_INVALID_INPUT_ERROR: Occurs if @p n is less than or equal to
        !!      zero.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        procedure, public :: set_capacity => bw_set_capacity
        !> @brief Gets the number of bytes stored within the buffer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_count(class(binary_writer) this)
        !! @endcode
        !!
        !! @param[in] this The binary_writer object.
        !! 
        !! @return The number of bytes stored within the buffer.
        procedure, public :: get_count => bw_get_count
        !> @brief Clears the buffer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear_buffer(class(binary_writer) this)
        !! @endcode
        !!
        !! @param[in,out] this The binary_writer object.
        procedure, public :: clear_buffer => bw_clear_buffer
        !> @brief Opens a binary file for writing.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine open(class(binary_writer) this, character(len = *) fname, logical append, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The binary_writer object.
        !! @param[in] fname The name of the file to open.
        !! @param[in] append An optional argument that, if specified, determines
        !!  if the file should be appended.  If not supplied, and a file exists,
        !!  the file will be overwritten.  If no file exists, it simply will be
        !!  created.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be opened.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        procedure, public :: open => bw_open
        !> @brief Closes the file.  This will also force writing of all buffer
        !! contents.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine close(class(binary_writer) this)
        !! @endcode
        !!
        !! @param[in,out] this The binary_writer object.
        !! @param[in] del An optional input, that if set, determines if the file
        !!  should be deleted once closed.  The default is false such that the
        !!  file remains.
        procedure, public :: close => bw_close
        !> @brief Flushes the buffer by writing the contents to file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine flush_buffer(class(binary_writer) this)
        !! @endcode
        !!
        !! @param[in,out] this The binary_writer object.
        procedure, public :: flush_buffer => bw_flush_buffer
        !> @brief Pushes an item onto the buffer for writing.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine push(class(binary_writer) this, generic x, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The binary_writer object.
        !! @param[in] x The data to push onto the buffer.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        generic, public :: push => bw_append_byte, bw_append_byte_array, &
            bw_append_r64, bw_append_r64_array, bw_append_r64_matrix, &
            bw_append_r32, bw_append_r32_array, bw_append_r32_matrix, &
            bw_append_i16, bw_append_i16_array, bw_append_i16_matrix, &
            bw_append_i32, bw_append_i32_array, bw_append_i32_matrix, &
            bw_append_i64, bw_append_i64_array, bw_append_i64_matrix, &
            bw_append_c64, bw_append_c64_array, bw_append_c64_matrix, &
            bw_append_c32, bw_append_c32_array, bw_append_c32_matrix, &
            bw_append_char, bw_append_str

        procedure :: bw_append_byte
        procedure :: bw_append_byte_array
        procedure :: bw_append_r64
        procedure :: bw_append_r64_array
        procedure :: bw_append_r64_matrix
        procedure :: bw_append_r32
        procedure :: bw_append_r32_array
        procedure :: bw_append_r32_matrix
        procedure :: bw_append_i16
        procedure :: bw_append_i16_array
        procedure :: bw_append_i16_matrix
        procedure :: bw_append_i32
        procedure :: bw_append_i32_array
        procedure :: bw_append_i32_matrix
        procedure :: bw_append_i64
        procedure :: bw_append_i64_array
        procedure :: bw_append_i64_matrix
        procedure :: bw_append_c64
        procedure :: bw_append_c64_array
        procedure :: bw_append_c64_matrix
        procedure :: bw_append_c32
        procedure :: bw_append_c32_array
        procedure :: bw_append_c32_matrix
        procedure :: bw_append_char
        procedure :: bw_append_str
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for reading binary files.
    type, extends(file_reader) :: binary_reader
    contains
        !> @brief Opens a binary file for reading.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine open(class(binary_reader) this, character(len = *) fname, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The binary_reader object.
        !! @param[in] fname The name of the file to open.
        !! @param[in] append An optional argument that, if specified, determines
        !!  if the file should be appended.  If not supplied, and a file exists,
        !!  the file will be overwritten.  If no file exists, it simply will be
        !!  created.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be opened.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        procedure, public :: open => br_open
        !> @brief Reads a specified number of bytes from the file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int8)(:) read_bytes(class(binary_reader) this, integer(int32) n, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The binary_reader object.
        !! @param[in] n The number of bytes to read.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!  - FCORE_INVALID_INPUT_ERROR: Occurs if the requested read size is
        !!      less than or equal to zero.
        !!
        !! @result An array containing the results.
        procedure, public :: read_bytes => br_read_byte_count
        !> @brief Reads a single byte from the file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int8) read_byte(class(binary_reader) this, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The binary_reader object.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!
        !! @result The results.
        procedure, public :: read_byte => br_read_byte
        !> @brief Reads the entire contents of the file into a buffer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int8)(:) read_all(class(binary_reader) this, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The binary_reader object.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!
        !! @result An array containing the results.
        procedure, public :: read_all => br_read_all
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a container for parts of a file path.
    type file_path
        !> @brief The drive.
        character(len = :), allocatable :: drive
        !> @brief The directory.
        character(len = :), allocatable :: directory
        !> @brief The filename.
        character(len = :), allocatable :: filename
        !> @brief The file extension.  Notice, the '.' is included 
        !! (e.g. ".txt").
        character(len = :), allocatable :: extension
    end type

! ------------------------------------------------------------------------------
    !> @brief Defins a container describing folder contents.
    type folder_contents
        !> @brief A list of files in the folder.
        type(string), allocatable, dimension(:) :: files
        !> @brief A list of folders with the folder.
        type(string), allocatable, dimension(:) :: folders
        !> @brief The folder
        character(len = :), allocatable :: folder
    end type

! ******************************************************************************
! C-INTEROP INTERFACES
! ------------------------------------------------------------------------------
    interface
        !> @brief An interface to the C split_file_path routine.
        subroutine split_file_path_c(path, drive, dir, fname, ext) &
                bind(C, name = "split_file_path_c")
            use iso_c_binding
            character(kind = c_char), intent(in) :: path(*)
            character(kind = c_char), intent(out) :: drive(*), dir(*), &
                fname(*), ext(*)
        end subroutine

        !> @brief An interface to the C get_directory_contents_c routine.
        function get_directory_contents_c(dir, nbuffers, bufferSize, fnames, &
                nnames, nameLengths, dirnames, ndir, dirLengths) &
                bind(C, name = "get_directory_contents_c") result(rst)
            use iso_c_binding
            character(kind = c_char), intent(in) :: dir(*)
            integer(c_int), intent(in), value :: nbuffers, bufferSize
            type(c_ptr), intent(out) :: fnames(nbuffers), dirNames(nbuffers)
            integer(c_int), intent(out) :: nnames, nameLengths(nbuffers), &
                ndir, dirLengths(nbuffers)
            logical(c_bool) :: rst
        end function
    end interface

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

        module function fm_get_size(this) result(rst)
            class(file_manager), intent(in) :: this
            integer(int32) :: rst
        end function
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

        module subroutine tw_write_txt_str(this, txt, err)
            class(text_writer), intent(in) :: this
            class(string), intent(in) :: txt
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine tw_write_txt_line(this, txt, err)
            class(text_writer), intent(in) :: this
            character(len = *), intent(in) :: txt
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine tw_write_txt_line_str(this, txt, err)
            class(text_writer), intent(in) :: this
            class(string), intent(in) :: txt
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
    interface
        pure module function bw_get_capacity(this) result(rst)
            class(binary_writer), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine bw_set_capacity(this, n, err)
            class(binary_writer), intent(inout) :: this
            integer(int32), intent(in) :: n
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function bw_get_count(this) result(rst)
            class(binary_writer), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine bw_clear_buffer(this)
            class(binary_writer), intent(inout) :: this
        end subroutine

        module subroutine bw_open(this, fname, append, err)
            class(binary_writer), intent(inout) :: this
            character(len = *), intent(in) :: fname
            logical, intent(in), optional :: append
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_close(this, del)
            class(binary_writer), intent(inout) :: this
            logical, intent(in), optional :: del
        end subroutine

        module subroutine bw_flush_buffer(this)
            class(binary_writer), intent(inout) :: this
        end subroutine

        module subroutine bw_append_byte(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int8), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_byte_array(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int8), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_r64(this, x, err)
            class(binary_writer), intent(inout) :: this
            real(real64), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_r64_array(this, x, err)
            class(binary_writer), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_r64_matrix(this, x, err)
            class(binary_writer), intent(inout) :: this
            real(real64), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_r32(this, x, err)
            class(binary_writer), intent(inout) :: this
            real(real32), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_r32_array(this, x, err)
            class(binary_writer), intent(inout) :: this
            real(real32), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_r32_matrix(this, x, err)
            class(binary_writer), intent(inout) :: this
            real(real32), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_i16(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int16), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_i16_array(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int16), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_i16_matrix(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int16), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_i32(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int32), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_i32_array(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int32), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_i32_matrix(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int32), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_i64(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int64), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_i64_array(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_i64_matrix(this, x, err)
            class(binary_writer), intent(inout) :: this
            integer(int64), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_c64(this, x, err)
            class(binary_writer), intent(inout) :: this
            complex(real64), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_c64_array(this, x, err)
            class(binary_writer), intent(inout) :: this
            complex(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_c64_matrix(this, x, err)
            class(binary_writer), intent(inout) :: this
            complex(real64), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_c32(this, x, err)
            class(binary_writer), intent(inout) :: this
            complex(real32), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_c32_array(this, x, err)
            class(binary_writer), intent(inout) :: this
            complex(real32), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_c32_matrix(this, x, err)
            class(binary_writer), intent(inout) :: this
            complex(real32), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_char(this, x, err)
            class(binary_writer), intent(inout) :: this
            character(len = *), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_append_str(this, x, err)
            class(binary_writer), intent(inout) :: this
            class(string), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine bw_clean_up(this)
            type(binary_writer), intent(inout) :: this
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    interface
        module subroutine br_open(this, fname, err)
            class(binary_reader), intent(inout) :: this
            character(len = *), intent(in) :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function br_read_byte_count(this, n, err) result(rst)
            class(binary_reader), intent(inout) :: this
            integer(int32), intent(in) :: n
            class(errors), intent(inout), optional, target :: err
            integer(int8), allocatable, dimension(:) :: rst
        end function

        module function br_read_byte(this, err) result(rst)
            class(binary_reader), intent(inout) :: this
            class(errors), intent(inout), optional, target :: err
            integer(int8) :: rst
        end function

        module function br_read_all(this, err) result(rst)
            class(binary_reader), intent(inout) :: this
            class(errors), intent(inout), optional, target :: err
            integer(int8), allocatable, dimension(:) :: rst
        end function
    end interface

! ------------------------------------------------------------------------------
    interface
        !> @brief Splits the supplied path into components.
        !!
        !! @param[in] path The path to split.
        !!
        !! @return The split file path.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use file_io
        !!     implicit none
        !!
        !!     ! The path to split
        !!     character(len = *), parameter :: p = "D:\SomeFilePath\With Spaces\File Name X.ext"
        !!
        !!     ! Split the path
        !!     type(file_path) :: parts
        !!     parts = split_path(p)
        !!
        !!     ! Display the results
        !!     print '(A)', "Drive: " // parts%drive
        !!     print '(A)', "Directory: " // parts%directory
        !!     print '(A)', "Filename: " // parts%filename
        !!     print '(A)', "Extension: " // parts%extension
        !! end program
        !! @endcode
        !! The above program produces the following output.
        !! @code{.txt}
        !! Drive: D:
        !! Directory: \SomeFilePath\With Spaces\
        !! Filename: File Name X
        !! Extension: .ext
        !! @endcode
        module function split_path(path) result(rst)
            character(len = *), intent(in) :: path
            type(file_path) :: rst
        end function

        !> @brief Gets a list of all contents of a folder.
        !!
        !! @param[in] folder The path to interogate.
        !!
        !! @return A list of the folder contents.
        module function get_folder_contents(folder) result(rst)
            character(len = *), intent(in) :: folder
            type(folder_contents) :: rst
        end function

        !> @brief Finds all files with the specified extension within a 
        !! directory.
        !!
        !! @param[in] folder The directory (folder) to search.
        !! @param[in] ext The extension to match.  Notice, the extenion must 
        !!  include a '.' character prior to the extension (e.g. ".txt"); 
        !!  otherwise, the extension will not be found.
        !! @param[in] subfolders An optional input used to determine if 
        !!  subfolders should be searched.  The default is false, such that the 
        !!  subfolders are not searched.  If set to true, all subfolders are 
        !!  searched.
        !! @param[in,out] err An optional errors-based object that if provided
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!
        !! @return A list of all located files matching the required criteria.
        recursive module function find_all_files(folder, ext, subfolders, err) &
                result(rst)
            character(len = *), intent(in) :: folder, ext
            logical, intent(in), optional :: subfolders
            class(errors), intent(inout), optional, target :: err
            type(string), allocatable, dimension(:) :: rst
        end function
    end interface

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order.
    !!
    !! @param[in,out] x The value whose byte order is to be swapped.
    !!
    !! @return The resulting byte-swapped value.
    interface swap_bytes
        module procedure :: swap_bytes_r64
        module procedure :: swap_bytes_r32
        module procedure :: swap_bytes_c64
        module procedure :: swap_bytes_c32
        module procedure :: swap_bytes_i64
        module procedure :: swap_bytes_i32
        module procedure :: swap_bytes_i16
    end interface

    interface
        !> @brief Determines if the current machine is little-endian or 
        !! big-endian.
        !!
        !! @return Returns true if the current machine is little-endian; else, 
        !!  false if big-endian.
        pure module function is_little_endian() result(rst)
            logical :: rst
        end function

        elemental module function swap_bytes_r64(x) result(rst)
            real(real64), intent(in) :: x
            real(real64) :: rst
        end function

        elemental module function swap_bytes_r32(x) result(rst)
            real(real32), intent(in) :: x
            real(real32) :: rst
        end function

        elemental module function swap_bytes_i16(x) result(rst)
            integer(int16), intent(in) :: x
            integer(int16) :: rst
        end function

        elemental module function swap_bytes_i32(x) result(rst)
            integer(int32), intent(in) :: x
            integer(int32) :: rst
        end function

        elemental module function swap_bytes_i64(x) result(rst)
            integer(int64), intent(in) :: x
            integer(int64) :: rst
        end function

        elemental module function swap_bytes_c64(x) result(rst)
            complex(real64), intent(in) :: x
            complex(real64) :: rst
        end function

        elemental module function swap_bytes_c32(x) result(rst)
            complex(real32), intent(in) :: x
            complex(real32) :: rst
        end function
    end interface

! ------------------------------------------------------------------------------
end module
