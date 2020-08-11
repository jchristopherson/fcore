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
    ! be parsed as desired.  This can be accomplished as follows.
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
