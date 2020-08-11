# fcore
A Fortran library containing string handling, i/o routines, etc.

## String Handling Example
The following example illustrates some of the string handling abilities within the library.
```fortran
program main
    use iso_fortran_env
    use strings
    use regular_expressions
    implicit none

    ! Local Variables
    integer(int32) :: i
    type(string) :: str1, strUp, strDn
    type(string), allocatable :: matches(:)

    ! Create a simple string
    str1 = "This is an example string containing letters and numbers: 1.2345"
    print '(A)', "Example String 1: " // str1%str
    print '(AI0)', "String Length: ", str1%length()
    
    ! Convert to all upper case
    strUp = str1%to_upper()
    print '(A)', "Converted to upper case: " // strUp%str

    ! Convert to all lower case
    strDn = str1%to_lower()
    print '(A)', "Converted to lower case: " // strDn%str

    ! Look for numeric values using regular expressions
    matches = regex_search(str1%str, "\d.+")
    print '(AI0A)', "Found ", size(matches), " matches"
    do i = 1, size(matches)
        print '(AI0A)', "Match ", i, ": " // matches(i)%str
    end do
end program
```
The output of the above program is as follows.
```text
Example String 1: This is an example string containing letters and numbers: 1.2345
String Length: 64
Converted to upper case: THIS IS AN EXAMPLE STRING CONTAINING LETTERS AND NUMBERS: 1.2345
Converted to lower case: this is an example string containing letters and numbers: 1.2345
Found 1 matches
Match 1: 1.2345
```

## Text I/O Example
The following example illustrates some of the text file read/write functionallity within the library.
```fortran
program main
    use file_io
    implicit none

    ! Parameters
    character(len = *), parameter :: fname = "test_text_file.txt"

    ! Local Variables
    type(text_writer) :: writer
    type(text_reader) :: reader
    character(len = :), allocatable :: buffer

    ! Let's write a file by first opening a file object to write to
    call writer%open(fname)
    
    ! Write some stuff (line-by-line)
    call writer%write_line("This is line 1.")
    call writer%write_line("This is line 2.")
    call writer%write_line("This,is,a,comma,delimited,string.")

    ! Write without advancing to the next line
    call writer%write("Just trying to write without advancing lines. ")
    call writer%write("Here's some more text. ")
    call writer%write_line("Here's the end of the line.")

    ! Close the file.  The object's finalizer would normally take care of this
    ! action, but the intent here is to use this file again before the writer
    ! object would be cleaned up; therefore, it's worth manually closing the
    ! file.
    call writer%close()

    ! Read in the file by first opening it for reading.
    call reader%open(fname)

    ! Read in lines, and print them
    print '(A)', reader%read_line()
    print '(A)', reader%read_line()
    print '(A)', reader%read_line()
    print '(A)', reader%read_line()

    ! Close the file
    call reader%close()

    ! Alternatively, a more efficient means of reading the file would be to
    ! read in the entire contents into a single string.  The string could then
    ! be manipulated as desired.  This can be accomplished as follows.
    call reader%open(fname)
    buffer = reader%read_all()
    print '(A)', buffer

    ! This time we'll delete the file as part of the close operation
    call reader%close(.true.)
end program
```
The output of the above program is as follows.
```text
This is line 1.
This is line 2.
This,is,a,comma,delimited,string.
Just trying to write without advancing lines. Here's some more text. Here's the end of the line.
This is line 1.
This is line 2.
This,is,a,comma,delimited,string.
Just trying to write without advancing lines. Here's some more text. Here's the end of the line.
```

## File System Example
The following example illustrates some of the file system operations available in this library.
```fortran
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
```
The output of the above program is as follows.  Notice, the file paths have been altered as my personal directory structure is not relevant for the purposes of this example.
```text
Working Directory: C:\~\Documents\github\fcore
Folder Count: 9
File Count: 6

Folders:
Folder 1: C:\~\Documents\github\fcore\.git
Folder 2: C:\~\Documents\github\fcore\.vscode
Folder 3: C:\~\Documents\github\fcore\bin
Folder 4: C:\~\Documents\github\fcore\build
Folder 5: C:\~\Documents\github\fcore\examples
Folder 6: C:\~\Documents\github\fcore\include
Folder 7: C:\~\Documents\github\fcore\lib
Folder 8: C:\~\Documents\github\fcore\src
Folder 9: C:\~\Documents\github\fcore\tests

Files:
File 1: C:\~\Documents\github\fcore\.gitignore
File 2: C:\~\Documents\github\fcore\.travis.yml
File 3: C:\~\Documents\github\fcore\CMakeLists.txt
File 4: C:\~\Documents\github\fcore\fcoreConfig.cmake.in
File 5: C:\~\Documents\github\fcore\LICENSE
File 6: C:\~\Documents\github\fcore\README.md

Found 13 text files.
File: 1: C:\~\Documents\github\fcore\CMakeLists.txt
File: 2: C:\~\Documents\github\fcore\build\CMakeCache.txt
File: 3: C:\~\Documents\github\fcore\build\CMakeFiles\CMakeRuleHashes.txt
File: 4: C:\~\Documents\github\fcore\build\CMakeFiles\TargetDirectories.txt
File: 5: C:\~\Documents\github\fcore\build\examples\CMakeFiles\folder_ops_example.dir\link.txt
File: 6: C:\~\Documents\github\fcore\build\examples\CMakeFiles\strings_example.dir\link.txt
File: 7: C:\~\Documents\github\fcore\build\examples\CMakeFiles\text_io_example.dir\link.txt
File: 8: C:\~\Documents\github\fcore\build\src\CMakeFiles\fcore.dir\link.txt
File: 9: C:\~\Documents\github\fcore\build\tests\test_text_1.txt
File: 10: C:\~\Documents\github\fcore\build\tests\CMakeFiles\fcore_tests.dir\link.txt
File: 11: C:\~\Documents\github\fcore\examples\CMakeLists.txt
File: 12: C:\~\Documents\github\fcore\src\CMakeLists.txt
File: 13: C:\~\Documents\github\fcore\tests\CMakeLists.txt

Path Splitting:
Drive: C:
Directory: \~\Documents\github\fcore\
Filename: CMakeLists
Extension: .txt
```