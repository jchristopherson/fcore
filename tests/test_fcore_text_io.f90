! test_fcore_text_io.f90

module test_fcore_text_io
    use iso_fortran_env
    use file_io
    use strings
    implicit none
contains
    function test_text_read_write() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: fname = "test_text_1.txt"
        character(len = *), parameter :: line1 = "This is just a test message"
        character(len = *), parameter :: line2 = "with two lines."

        ! Local Variables
        type(text_writer) :: writer
        type(text_reader) :: reader
        character(len = :), allocatable :: msg
        type(string), allocatable, dimension(:) :: lines

        ! Initialization
        rst = .true.
        msg = line1 // new_line('a') // line2

        ! Write the file
        call writer%open(fname)
        call writer%write(msg)
        call writer%close()

        ! Read in the file
        call reader%open(fname)
        lines = reader%read_lines()
        call writer%close()

        ! Ensure there are two lines
        if (size(lines) /= 2) then
            rst = .false.
            print '(AI0A)', "TEST_TEXT_READ_WRITE (Test #1): " // &
                "Expected an array of length 2, but found an array " // &
                "of length ", size(lines), "."
            return
        end if
    end function

end module
