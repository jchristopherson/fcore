! test_fcore_binary_io.f90

module test_fcore_binary_io
    use iso_fortran_env
    use file_io
    implicit none
contains
    function test_binary_read_write() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        character(len = *), parameter :: fname = "test_binary_1.bin"
        real(real64), parameter :: item1 = 250.0d0
        integer(int64), parameter :: item2 = 50000
        integer(int16), parameter :: item3 = 10
        real(real32), parameter :: item4 = 10.5

        ! Local Variables
        type(binary_writer) :: writer
        type(binary_reader) :: reader
        real(real64) :: value1
        integer(int64) :: value2
        integer(int16) :: value3
        real(real32) :: value4

        ! Initialization
        rst = .true.

        ! Write the file
        call writer%open(fname)
        call writer%push(item1)
        call writer%set_capacity(10000) ! This is just for testing purposes
        call writer%push(item2)
        call writer%push(item3)
        call writer%push(item4)
        call writer%close() ! writes to file, and then closes the file

        ! Read in the file
        call reader%open(fname)
        value1 = transfer(reader%read_bytes(8), value1)
        value2 = transfer(reader%read_bytes(8), value2)
        value3 = transfer(reader%read_bytes(2), value3)
        value4 = transfer(reader%read_bytes(4), value4)
        call reader%close()

        ! Check the results
        if (item1 /= value1) then
            rst = .false.
            print '(AF5.1AF5.1A)', "TEST_BINARY_READ_WRITE (Test #1): " // &
                "Expected to find ", item1, ", but found ", value1, "."
        end if

        if (item2 /= value2) then
            rst = .false.
            print '(AI0AI0A)', "TEST BINARY_READ_WRITE (Test #2): " // &
                "Expected to find ", item2, ", but found ", value2, "."
        end if

        if (item3 /= value3) then
            rst = .false.
            print '(AI0AI0A)', "TEST BINARY_READ_WRITE (Test #3): " // &
                "Expected to find ", item3, ", but found ", value3, "."
        end if

        if (item4 /= value4) then
            rst = .false.
            print '(AF4.1AF4.1A)', "TEST_BINARY_READ_WRITE (Test #4): " // &
                "Expected to find ", item4, ", but found ", value4, "."
        end if
    end function
end module
