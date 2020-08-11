! file_io_binary.f90

submodule (file_io) file_io_binary
contains
! ******************************************************************************
! BINARY_WRITER CLASS
! ------------------------------------------------------------------------------
    !> @brief Gets the capacity of the buffer, in bytes.
    !!
    !! @param[in] this The binary_writer object.
    !!
    !! @return The buffer capacity, in bytes.
    pure module function bw_get_capacity(this) result(rst)
        ! Arguments
        class(binary_writer), intent(in) :: this
        integer(int32) :: rst

        ! Process
        if (.not.allocated(this%m_buffer)) then
            rst = 0
        else
            rst = size(this%m_buffer)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets the capacity of the buffer, in bytes.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] n The size, in bytes, to make the buffer.  This value must
    !!  be greater than zero.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_INVALID_INPUT_ERROR: Occurs if @p n is less than or equal to
    !!      zero.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_set_capacity(this, n, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: copy
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure N is positive and non-zero
        if (n <= 0) then
            call errmgr%report_error("bw_set_capacity", &
                "The buffer capacity must be greater than 0.", &
                FCORE_INVALID_INPUT_ERROR)
            return
        end if

        ! Ensure the buffer is initially allocated
        if (.not.allocated(this%m_buffer)) then
            allocate(this%m_buffer(n), stat = flag)
            if (flag /= 0) go to 100
            return
        end if

        ! Create a copy of the existing buffer
        copy = this%m_buffer(1:this%m_count)

        ! Reconstruct the newly resized buffer
        deallocate(this%m_buffer)
        allocate(this%m_buffer(n), stat = flag)
        if (flag /= 0) go to 100
        this%m_count = min(nOld, n)
        this%m_buffer(1:this%m_count) = copy(1:this%m_count)

        ! End
        return
    
    100 continue
        call errmgr%report_error("bw_set_capacity", &
            "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
        return
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the number of bytes stored within the buffer.
    !!
    !! @param[in] this The binary_writer object.
    !! 
    !! @return The number of bytes stored within the buffer.
    pure module function bw_get_count(this) result(rst)
        ! Arguments
        class(binary_writer), intent(in) :: this
        integer(int32) :: rst

        ! Process
        rst = this%m_count
    end function


! ------------------------------------------------------------------------------
    !> @brief Clears the buffer.
    !!
    !! @param[in,out] this The binary_writer object.
    module subroutine bw_clear_buffer(this)
        ! Arguments
        class(binary_writer), intent(inout) :: this

        ! Process
        if (.not.allocated(this%m_buffer)) return
        this%m_buffer = 0
        this%m_count = 0
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Opens a binary file for writing.
    !!
    !! @param[in,out] this The binary_writer object.
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
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_open(this, fname, append, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        character(len = *), intent(in) :: fname
        logical, intent(in), optional :: append
        class(errors), intent(inout), optional, target :: err

        ! Parameters
        integer(int32), parameter :: DEFAULT_BUFFER_SIZE = 4096

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
                form = "unformatted", access = "stream", iostat = flag)
        else
            open(newunit = val, file = fname, form = "unformatted", &
                access = "stream", iostat = flag)
        end if
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("bw_open", trim(errmsg), &
                FCORE_FILE_IO_ERROR)
            return
        end if
        call this%set_unit(val)
        call this%set_filename(fname)

        ! Establish a buffer - if not already allocated
        call this%set_capacity(DEFAULT_BUFFER_SIZE, errmgr)
        if (errmgr%has_error_occurred()) return
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Closes the file.  This will also force writing of all buffer
    !! contents.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] del An optional input, that if set, determines if the file
    !!  should be deleted once closed.  The default is false such that the
    !!  file remains.
    module subroutine bw_close(this, del)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        logical, intent(in), optional :: del

        ! Exit if the file isn't open
        if (.not.this%is_open()) return

        ! Flush the buffer
        call this%flush_buffer()

        ! Close the file
        call this%file_manager%close(del)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Flushes the buffer.
    !!
    !! @param[in,out] this The binary_writer object.
    module subroutine bw_flush_buffer(this)
        ! Arguments
        class(binary_writer), intent(inout) :: this

        ! Local Variables
        integer(int32) :: n

        ! Initialization
        n = this%get_count()
        
        ! Quick Return
        if (n == 0) return

        ! Write the buffer
        write(this%get_unit()) this%m_buffer(1:n)

        ! Zero the buffer and reset the counter
        this%m_buffer = 0
        this%m_count = 0
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a single byte onto the buffer.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The data to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_byte(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int8), intent(in) :: x
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

        ! Append onto the end of the buffer
        if (this%m_count + 1 >= this%get_capacity()) then
            call this%set_capacity(this%get_capacity() * 2, errmgr)
            if (errmgr%has_error_occurred()) return
        end if
        this%m_buffer(this%m_count + 1) = x
        this%m_count = this%m_count + 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an array of byte values onto the buffer.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_byte_array(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int8), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: n
        
        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        if (this%m_count + n >= this%get_capacity()) then
            call this%set_capacity( &
                max(this%m_count + n, this%get_capacity() * 2), &
                errmgr)
            if (errmgr%has_error_occurred()) return
        end if
        this%m_buffer(this%m_count + 1:this%m_count + n) = x
        this%m_count = this%m_count + n
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a 64-bit real value onto the buffer.  No correction is made
    !! for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_r64(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real64), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an array of 64-bit real values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_r64_array(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a matrix of 64-bit real values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_r64_matrix(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real64), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a 32-bit real value onto the buffer.  No correction is made
    !! for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_r32(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real32), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an array of 32-bit real values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_r32_array(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real32), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a matrix of 32-bit real values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_r32_matrix(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real32), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a 16-bit integer value onto the buffer.  No correction is 
    !! made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_i16(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int16), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an array of 16-bit integer values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_i16_array(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int16), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a matrix of 16-bit integer values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_i16_matrix(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int16), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a 32-bit integer value onto the buffer.  No correction is 
    !! made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_i32(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int32), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an array of 32-bit integer values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_i32_array(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int32), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a matrix of 32-bit integer values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_i32_matrix(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int32), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a 64-bit integer value onto the buffer.  No correction is 
    !! made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_i64(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int64), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an array of 64-bit integer values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_i64_array(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a matrix of 64-bit integer values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_i64_matrix(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int64), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a 64-bit complex value onto the buffer.  No correction is 
    !! made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_c64(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real64), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an array of 64-bit complex values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_c64_array(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a matrix of 64-bit complex values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_c64_matrix(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real64), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a 32-bit complex value onto the buffer.  No correction is 
    !! made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_c32(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real32), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an array of 32-bit complex values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_c32_array(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real32), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a matrix of 32-bit complex values onto the buffer.  No 
    !! correction is made for endianess.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_c32_matrix(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real32), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a character string onto the buffer.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_char(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        character(len = *), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes a character string onto the buffer.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] x The array to push onto the buffer.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine bw_append_str(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        class(string), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: buffer

        ! Process
        buffer = transfer(x, buffer)
        call this%push(buffer, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Forces a write operation on all buffer contents, closes the file,
    !! and performs any necessary clean-up operations.
    !!
    !! @param[in,out] this The binary_writer object.
    module subroutine bw_clean_up(this)
        ! Arguments
        type(binary_writer), intent(inout) :: this

        ! Flush the buffer & force any pending write actions
        call this%flush_buffer()

        ! The parent destructor should close the file.  Regardless, we'll to it
        ! here to ensure it's done properly
        call this%close()
    end subroutine

! ------------------------------------------------------------------------------



! ******************************************************************************
! BINARY_READER CLASS
! ------------------------------------------------------------------------------
    !> @brief Opens a binary file for reading.
    !!
    !! @param[in,out] this The binary_reader object.
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
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    module subroutine br_open(this, fname, err)
        ! Arguments
        class(binary_reader), intent(inout) :: this
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

        ! Close, if already open
        call this%close()

        ! Process
        open(newunit = val, file = fname, form = "unformatted", &
            access = "stream", iostat = flag)
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_open", trim(errmsg), &
                FCORE_FILE_IO_ERROR)
        end if
        call this%set_unit(val)
        call this%set_filename(fname)
        call this%move_to_start()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reads a specified number of bytes from the file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of bytes to read.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    !!  - FCORE_INVALID_INPUT_ERROR: Occurs if the requested read size is
    !!      less than or equal to zero.
    !!
    !! @result An array containing the results.
    module function br_read_byte_count(this, n, err) result(rst)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int8), allocatable, dimension(:) :: rst

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: fsize, flag, nbytes
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the file is opened for reading
        if (.not.this%is_open()) then
            call errmgr%report_error("br_read_byte_count", &
                "The file is not opened for reading.", &
                FCORE_UNOPENED_ERROR)
            return
        end if

        ! Determine the number of bytes to read, and then ensure its a positive,
        ! non-zero value
        ! inquire(file = this%get_filename(), size = fsize)
        fsize = this%get_file_size()
        nbytes = min(n, fsize - this%get_position() + 1)
        if (nbytes < 0) then
            write(errmsg, '(AI0A)') "Expected a positive value for number" // & 
                " of bytes to read, but found ", nbytes, "."
            call errmgr%report_error("br_read_byte_count", trim(errmsg), &
                FCORE_INVALID_INPUT_ERROR)
            return
        end if

        allocate(rst(nbytes), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("br_read_byte_count", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Perform the read operation
        read(this%get_unit(), pos = this%get_position(), iostat = flag) rst
        if (flag > 0) then
            write(errmsg, '(AI0A)') & 
                "The file could not be read.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_byte_count", trim(errmsg), &
                FCORE_FILE_IO_ERROR)
            return
        end if

        ! Adjust the position
        call this%set_position(this%get_position() + nbytes)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a single byte from the file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    !!
    !! @result The results.
    module function br_read_byte(this, err) result(rst)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        integer(int8) :: rst

        ! Local Variables
        integer(int8), allocatable, dimension(:) :: x

        ! Process
        x = this%read_bytes(1, err)
        rst = x(1)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads the entire contents of the file into a buffer.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_FILE_IO_ERROR: Occurs if the file could not be read.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    !!
    !! @result An array containing the results.
    module function br_read_all(this, err) result(rst)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        integer(int8), allocatable, dimension(:) :: rst

        ! Local Variables
        integer(int32) :: n

        ! Process
        call this%move_to_start()
        ! inquire(file = this%get_filename(), size = n)
        n = this%get_file_size()
        rst = this%read_bytes(n, err)
    end function

! ------------------------------------------------------------------------------
end submodule
