! fcore_constants.f90

!> @brief A module providing constants used by the FCORE library.
module fcore_constants
    use iso_fortran_env
    implicit none
    
    !> @brief Occurs if there is insufficient memory available for the
    !! requested operation.
    integer(int32), parameter :: FCORE_OUT_OF_MEMORY_ERROR = 10000
    !> @brief Occurs if an invalid input is provided.
    integer(int32), parameter :: FCORE_INVALID_INPUT_ERROR = 10001
    !> @brief Occurs if the supplied index is out of range.
    integer(int32), parameter :: FCORE_INDEX_OUT_OF_RANGE_ERROR = 10002
    !> @brief Occurs if the array is not appropriately sized.
    integer(int32), parameter :: FCORE_ARRAY_SIZE_ERROR = 10003
    !> @brief Occurs if an I/O error is encountered.
    integer(int32), parameter :: FCORE_FILE_IO_ERROR = 10004
    !> @brief Occurs if a read or write operation is attempted on an unopened 
    !! file.
    integer(int32), parameter :: FCORE_UNOPENED_ERROR = 10005
    !> @brief Occurs if a read operation occurs after the end-of-file is
    !! encountered.
    integer(int32), parameter :: FCORE_END_OF_FILE_ERROR = 10006
    !> @brief Occurs if an attempt is made to write to a nonexistent key.
    integer(int32), parameter :: FCORE_NONEXISTENT_KEY_ERROR = 10007
    !> @brief Occurs if an attempt is made to overwrite an existing key 
    !! inadvertently.
    integer(int32), parameter :: FCORE_EXISTING_KEY_ERROR = 10008
    !> @brief Occurs if a data type error is encountered.
    integer(int32), parameter :: FCORE_DATA_TYPE_ERROR = 10009
    !> @brief Occurs if an attempt to write to a null reference is made.
    integer(int32), parameter :: FCORE_NULL_REFERENCE_ERROR = 10010
    !> @brief Occurs if an attempt to perform an operation on an uninitialized
    !! object is made.
    integer(int32), parameter :: FCORE_UNINITIALIZED_OBJECT_ERROR = 10011
end module
