! strings_example.f90

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