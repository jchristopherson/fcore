! text_io_example.f90

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
