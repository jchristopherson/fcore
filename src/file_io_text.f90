! file_io_text.f90

submodule (file_io) file_io_text
contains
! ------------------------------------------------------------------------------
    !> @brief Opens a text file for writing.
    !!
    !! @param[in,out] this The text_writer object.
    !! @param[in] fname The name of the file to open.
    !! @param[in] append An optional argument that, if specified, determines
    !!  if the file should be appended.  If not supplied, and a file exists,
    !!  the file will be overwritten.  If no file exists, it simply will be
    !!  created.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be opened.
    module subroutine tw_open(this, fname, append, err)
        ! Arguments
        class(text_writer), intent(inout) :: this
        character(len = *), intent(in) :: fname
        logical, intent(in), optional :: append
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag, val
        logical :: append2File
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        append2File = .false.
        if (present(append)) append2File = append

        ! Close the file if already open
        call this%close()

        ! Open the file
        if (append2File) then
            open(newunit = val, file = fname, position = "append", &
                iostat = flag)
        else
            open(newunit = val, file = fname, iostat = flag)
        end if
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("tw_open", trim(errmsg), &
                FCORE_FILE_IO_ERROR)
            return
        end if

        call this%set_unit(val)
        call this%set_filename(fname)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes text to the file, but does not advance to the next line.
    !!
    !! @param[in] this The text_writer object.
    !! @param[in] txt The text to write.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    module subroutine tw_write_txt(this, txt, err)
        ! Arguments
        class(text_writer), intent(in) :: this
        character(len = *), intent(in) :: txt
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (.not.this%is_open()) then
            call errmgr%report_error("tw_write_txt", &
                "The file is not opened.", FCORE_UNOPENED_ERROR)
            return
        end if

        ! Process
        write(this%get_unit(), '(A)', advance = 'no') txt
    end subroutine

! --------------------
    !> @brief Writes text to the file, but does not advance to the next line.
    !!
    !! @param[in] this The text_writer object.
    !! @param[in] txt The text to write.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    module subroutine tw_write_txt_str(this, txt, err)
        ! Arguments
        class(text_writer), intent(in) :: this
        class(string), intent(in) :: txt
        class(errors), intent(inout), optional, target :: err

        ! Process
        call this%write(txt%str, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes text to the file, but does advance to the next line.
    !!
    !! @param[in] this The text_writer object.
    !! @param[in] txt The text to write.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    module subroutine tw_write_txt_line(this, txt, err)
        ! Arguments
        class(text_writer), intent(in) :: this
        character(len = *), intent(in) :: txt
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (.not.this%is_open()) then
            call errmgr%report_error("tw_write_txt_line", &
                "The file is not opened.", FCORE_UNOPENED_ERROR)
            return
        end if

        ! Process
        write(this%get_unit(), '(A)') txt
    end subroutine

! --------------------
    !> @brief Writes text to the file, but does advance to the next line.
    !!
    !! @param[in] this The text_writer object.
    !! @param[in] txt The text to write.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    module subroutine tw_write_txt_line_str(this, txt, err)
        ! Arguments
        class(text_writer), intent(in) :: this
        class(string), intent(in) :: txt
        class(errors), intent(inout), optional, target :: err

        ! Process
        call this%write_line(txt%str, err)
    end subroutine

! ******************************************************************************
    !> @brief Opens a text file for reading.
    !!
    !! @param[in,out] this The text_reader object.
    !! @param[in] fname The name of the file to open.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be opened.
    module subroutine tr_open(this, fname, err)
        ! Arguments
        class(text_reader), intent(inout) :: this
        character(len = *), intent(in) :: fname
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag, val
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Close if already open
        call this%close()

        ! Process
        open(newunit = val, file = fname, form = "unformatted", &
            access = "stream", iostat = flag)
        if (flag > 0) then
            write(errmsg, '(AI0A)') &
                "The file could not be opened.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("tr_open", trim(errmsg), &
                FCORE_FILE_IO_ERROR)
            return
        end if
        call this%move_to_start()
        call this%set_filename(fname)
        call this%set_unit(val)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reads the entire contents of an ASCII text file into a string.
    !!
    !! @param[in] this The text_reader object.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_UNOPENED_ERROR: Occurs if the file hasn't been opened.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory
    !!      available.
    !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
    !!
    !! @return The string containing the file contents.  Notice, the line
    !!  termination characters have not been stripped out of the string.
    !!
    !! @par Remarks
    !! Notice, the position indicator is not referenced, or utilized, for this
    !! read operation.  Regardless of its status, the entire file is read.
    module function tr_read_full_file(this, err) result(rst)
        ! Arguments
        class(text_reader), intent(in) :: this
        class(errors), intent(inout), optional, target :: err
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32) :: fsize, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the file is open
        if (.not.this%is_open()) then
            call errmgr%report_error("tr_read_full_file", &
                "The file is not opened.", FCORE_UNOPENED_ERROR)
            return
        end if

        ! Determine the file size, and allocate a buffer
        inquire(file = this%get_filename(), size = fsize)
        if (fsize == 0) return
        allocate(character(len = fsize) :: rst, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("tr_read_full_file", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Read the file
        read(unit = this%get_unit(), pos = 1, iostat = flag) rst
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be read.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("tr_read_full_file", trim(errmsg), &
                FCORE_FILE_IO_ERROR)
            return
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a single character from an ASCII text file.
    !!
    !! @param[in,out] this The text_reader object.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_UNOPENED_ERROR: Occurs if the file hasn't been opened.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory
    !!      available.
    !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
    !!
    !! @return The character.
    !!
    !! @par Remarks
    !! On output, the position indicator is incremented by one character.
    module function tr_read_char(this, err) result(rst)
        ! Arguments
        class(text_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        character :: rst

        ! Local Variables
        integer(int32) :: flag, pos
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the file is open
        if (.not.this%is_open()) then
            call errmgr%report_error("tr_read_char", &
                "The file is not opened.", FCORE_UNOPENED_ERROR)
            return
        end if

        ! Read the character
        pos = this%get_position()
        read(this%get_unit(), pos = pos, iostat = flag) rst
        if (flag > 0) then
            write(errmsg, '(AI0A)') &
                "The file could not be read.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("tr_read_char", trim(errmsg), &
                FCORE_FILE_IO_ERROR)
            return
        else if (flag < 0) then
            call errmgr%report_error("tr_read_char", &
                "The end of the file has been encountered.", &
                FCORE_END_OF_FILE_ERROR)
            return
        end if

        ! Increment the position
        ! FYI: storage_size returns the storage size of x in bits, but position
        ! is in bytes; hence division by 8
        call this%set_position(pos + storage_size(rst) / 8)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a single line from an ASCII text file.
    !!
    !! @param[in] this The text_reader object.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
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
    module function tr_read_line(this, err) result(rst)
        ! Arguments
        class(text_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32) :: i, flag, fsize, pos
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        character(len = :), allocatable :: buffer
        character :: c, eol, cr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        cr = char(13) ! Carriage Return Character
        eol = new_line(eol)
        i = 0

        ! Ensure the file is open
        if (.not.this%is_open()) then
            call errmgr%report_error("tr_read_line", &
                "The file is not opened.", FCORE_UNOPENED_ERROR)
            return
        end if

        ! Allocate space for a buffer that is sufficiently sized to hold an
        ! entire line.
        inquire(file = this%get_filename(), size = fsize)
        if (fsize == 0) return
        allocate(character(len = fsize) :: buffer, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("tr_read_full_file", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Read in each character until we reach EOF or EOL
        do
            ! Read
            pos = this%get_position()
            read(unit = this%get_unit(), pos = pos, iostat = flag) c
            if (flag < 0) then
                ! EOF reached
                exit
            else if (flag > 0) then
                write(errmsg, "(AI0A)") &
                    "The file could not be read.  Error code ", flag, &
                    " was encountered."
                call errmgr%report_error("tr_read_line", trim(errmsg), &
                    FCORE_FILE_IO_ERROR)
                return
            end if

            ! Increment the file position
            call this%set_position(pos + storage_size(c) / 8)

            ! If we encounter a carriage return, simply cycle the loop
            if (c == cr) cycle

            ! See if we're at the EOL yet
            if (c == eol) exit

            ! Store the value
            i = i + 1
            buffer(i:i) = c
        end do

        ! Trim the buffer to fit tightly
        rst = buffer(1:i)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads the entire contents of an ASCII text file into a string,
    !! and breaks the contents into lines.
    !!
    !! @param[in] this The text_reader object.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
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
    module function tr_read_lines(this, err) result(rst)
        ! Arguments
        class(text_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        type(string), allocatable, dimension(:) :: rst

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = :), allocatable :: contents
        character :: cr, eol
        integer(int32) :: i, n, j
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        cr = char(13) ! Carriage Return Character
        eol = new_line(eol)

        ! Read the contents of the file
        contents = this%read_all(errmgr)
        if (errmgr%has_error_occurred()) return

        ! Split the contents by EOL character - remove any carriage return
        ! characters
        rst = split_string(remove(contents, cr), eol)

        ! Trim empty lines from the end of the file
        n = size(rst)
        j = 0
        do i = n, 1, -1
            if (len(rst(i)%str) == 0) then
                j = j + 1
            else
                exit
            end if
        end do
        if (j /= 0) then
            n = n - j
            rst = rst(1:n)
        end if
    end function

! ------------------------------------------------------------------------------
end submodule