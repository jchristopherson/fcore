! file_io_path.f90

submodule (file_io) file_io_path
contains
! ------------------------------------------------------------------------------
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
        ! Arguments
        character(len = *), intent(in) :: path
        type(file_path) :: rst

        ! Local Variables
        character(kind = c_char, len = :), allocatable :: cpath
        character(kind = c_char, len = 1024) :: drive, dir, fname, ext

        ! Process
        cpath = path // C_NULL_CHAR
        call split_file_path_c(cpath, drive, dir, fname, ext)

        ! Convert the results to Fortran strings
        rst%drive = to_fortran_string(drive, c_string_length(drive))
        rst%directory = to_fortran_string(dir, c_string_length(dir))
        rst%filename = to_fortran_string(fname, c_string_length(fname))
        rst%extension = to_fortran_string(ext, c_string_length(ext))
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a list of all contents of a folder.
    !!
    !! @param[in] folder The path to interogate.
    !!
    !! @return A list of the folder contents.
    module function get_folder_contents(folder) result(rst)
        ! Arguments
        character(len = *), intent(in) :: folder
        type(folder_contents) :: rst

        ! Parameters
        integer(c_int), parameter :: BUFFER_COUNT = 2048
        integer(c_int), parameter :: BUFFER_LENGTH = 2048

        ! Local Variables
        integer(c_int) :: i, n, nnames, ndir, nameLengths(BUFFER_COUNT), &
            dirLengths(BUFFER_COUNT)
        character(kind = c_char, len = :), allocatable :: cfolder
        character(len = BUFFER_LENGTH), dimension(BUFFER_COUNT), target :: &
            nameBuffer, folderBuffer
        type(c_ptr) :: namePtr(BUFFER_COUNT), folderPtr(BUFFER_COUNT)
        logical(c_bool) :: check

        ! Initialization
        cfolder = folder // C_NULL_CHAR
        do i = 1, BUFFER_COUNT
            namePtr(i) = c_loc(nameBuffer(i))
            folderPtr(i) = c_loc(folderBuffer(i))
        end do

        ! Get the folder info
        check = get_directory_contents_c(cfolder, BUFFER_COUNT, BUFFER_LENGTH, &
            namePtr, nnames, nameLengths, folderPtr, ndir, dirLengths)
        
        ! Extract the output
        if (check) then
            allocate(rst%files(nnames))
            do i = 1, nnames
                n = nameLengths(i)
                rst%files(i)%str = nameBuffer(i)(1:n)
            end do

            allocate(rst%folders(ndir))
            do i = 1, ndir
                n = dirLengths(i)
                rst%folders(i)%str = folderBuffer(i)(1:n)
            end do

            rst%folder = folder
        else
            allocate(rst%files(0))
            allocate(Rst%folders(0))
            rst%folder = folder
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Finds all files with the specified extension within a directory.
    !!
    !! @param[in] folder The directory (folder) to search.
    !! @param[in] ext The extension to match.  Notice, the extenion must include
    !!  a '.' character prior to the extension (e.g. ".txt"); otherwise, the
    !!  extension will not be found.
    !! @param[in] subfolders An optional input used to determine if subfolders
    !!  should be searched.  The default is false, such that the subfolders are
    !!  not searched.  If set to true, all subfolders are searched.
    !! @param[in,out] err An optional errors-based object that if provided can 
    !!  be used to retrieve information relating to any errors encountered 
    !!  during execution.  If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling.  Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    !!
    !! @return A list of all located files matching the required criteria.
    recursive module function find_all_files(folder, ext, subfolders, err) &
            result(rst)
        ! Required Modules
        use collections

        ! Arguments
        character(len = *), intent(in) :: folder, ext
        logical, intent(in), optional :: subfolders
        class(errors), intent(inout), optional, target :: err
        type(string), allocatable, dimension(:) :: rst

        ! Local Variables
        type(folder_contents) :: contents
        type(file_path) :: path
        type(list) :: buffer
        type(string), allocatable, dimension(:) :: strBuffer
        integer(int32) :: i, j, flag
        logical :: sf
        class(*), pointer :: ptr
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        sf = .false.
        if (present(subfolders)) sf = subfolders

        ! Determine the contents of the folder
        contents = get_folder_contents(folder)

        ! Add any items with the extension to the list
        do i = 1, size(contents%files)
            path = split_path(contents%files(i)%str)
            if (path%extension == ext) then
                call buffer%push(contents%files(i))
            end if
        end do

        ! See if we need to search any subfolders
        if (sf) then
            do i = 1, size(contents%folders)
                if (allocated(strBuffer)) deallocate(strBuffer)
                strBuffer = find_all_files(contents%folders(i)%str, ext, &
                    subfolders, errmgr)
                if (errmgr%has_error_occurred()) return
                do j = 1, size(strBuffer)
                    call buffer%push(strBuffer(j))
                end do
            end do
        end if

        ! Convert the list to a string array
        allocate(rst(buffer%get_count()), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("find_all_files", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if
        do i = 1, buffer%get_count()
            ptr => buffer%get(i)
            select type (ptr)
                class is (string)
                    rst(i)%str = ptr%str
            end select
        end do
    end function

! ------------------------------------------------------------------------------
end submodule
