! folder_ops_example.f90

program main
    use iso_fortran_env
    use file_io
    use strings
    implicit none

    ! Local Variables
    integer(int32) :: i
    character(len = 256) :: cwd
    type(folder_contents) :: contents
    type(string), allocatable :: textFiles(:)
    type(file_path) :: path

    ! Get the current working directory
    call getcwd(cwd)
    print '(A)', "Working Directory: " // trim(cwd)

    ! Obtain a list of the directory contents
    contents = get_folder_contents(trim(cwd))
    print '(AI0)', "Folder Count: ", size(contents%folders)
    print '(AI0)', "File Count: ", size(contents%files)

    ! Print out all folders
    if (size(contents%folders) > 0) print '(A)', new_line('a') // "Folders: "
    do i = 1, size(contents%folders)
        print '(AI0A)', "Folder ", i, ": " // contents%folders(i)%str
    end do

    ! Print out all files
    if (size(contents%files) > 0) print '(A)', new_line('a') // "Files: "
    do i = 1, size(contents%files)
        print '(AI0A)', "File ", i, ": " // contents%files(i)%str
    end do

    ! Get a list of all text files within the directory and any sub-folders.
    ! Searching the sub-folders requires us to tell the routine (.true. 
    ! argument) as the default is to not search sub-folders.
    textFiles = find_all_files(trim(cwd), ".txt", .true.)
    print '(AI0A)', new_line('a') // "Found ", size(textFiles), " text files."
    do i = 1, size(textFiles)
        print '(AI0A)', "File: ", i, ": " // textFiles(i)%str
    end do

    ! Split up the path into drive, filename, path, and extension
    path = split_path(textFiles(1)%str)
    print '(A)', new_line('a') // "Path Splitting:"
    print '(A)', "Drive: " // path%drive
    print '(A)', "Directory: " // path%directory
    print '(A)', "Filename: " // path%filename
    print '(A)', "Extension: " // path%extension
end program
