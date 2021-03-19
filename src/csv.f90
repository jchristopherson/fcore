! csv.f90

!> @brief A module for interacting with CSV files.
module csv
    use iso_fortran_env
    use strings
    use collections
    use ferror
    use fcore_constants
    implicit none
    private
    public :: CSV_NUMERIC_DATA
    public :: CSV_STRING_DATA
    public :: CSV_LOGICAL_DATA
    public :: csv_column

! ------------------------------------------------------------------------------
    !> @brief Defines a numeric data type for a CSV column.
    integer(int32), parameter :: CSV_NUMERIC_DATA = 5000
    !> @brief Defines a string data type for a CSV column.
    integer(int32), parameter :: CSV_STRING_DATA = 5001
    !> @brief Defines a logical data type for a CSV column.
    integer(int32), parameter :: CSV_LOGICAL_DATA = 5002

! ******************************************************************************
! CSV_COLUMN.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a single column in a CSV data set.
    type csv_column
    private
        !> @brief The column header.
        character(len = :), allocatable :: m_header
        !> @brief The data type stored within the column.
        integer(int32) :: m_dataType = CSV_STRING_DATA
        !> @brief The stored data.
        type(list) :: m_data
    contains
        !> @brief Gets the column header string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function get_header(class(csv_column) this)
        !! @endcode
        !!
        !! @param[in] this The csv_column object.
        !! @return The header string.
        procedure, public :: get_header => cc_get_header
        !> @brief Sets the column header string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_header(class(csv_column) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The csv_column object.
        !! @param[in] x The header string.
        procedure, public :: set_header => cc_set_header
        !> @brief Gets information regarding the data type stored in the column.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_data_type(class(csv_column) this)
        !! @endcode
        !!
        !! @param[in] this The csv_column object.
        !! @return A flag denoting the data type stored within the column.  The
        !!  values are limited to the following:
        !!  - CSV_NUMERIC_DATA
        !!  - CSV_STRING_DATA
        !!  - CSV_LOGICAL_DATA
        procedure, public :: get_data_type => cc_get_data_type
        !> @brief Sets information regarding the data type stored in the column.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_data_type(class(csv_column) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The csv_column object.
        !! @param[in] x A flag denoting the data type stored within the column. 
        !!  The values are limited to the following:
        !!  - CSV_NUMERIC_DATA
        !!  - CSV_STRING_DATA
        !!  - CSV_LOGICAL_DATA
        !! If a value not in the list is specified, the column will default to
        !! CSV_STRING_DATA.
        procedure, public :: set_data_type => cc_set_data_type
        !> @brief Gets the number of items in the column.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_count(class(csv_column) this)
        !! @endcode
        !!
        !! @param[in] this The csv_column object.
        !! @return The number of items in the column.
        procedure, public :: get_count => cc_get_data_count
        !> @brief Gets the requested item from the collection.
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! character(len = :) function get(class(csv_column) this, integer(int32) index, character(len = *) id, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 2
        !! @code{.f90}
        !! real(real64) function get(class(csv_column) this, integer(int32) index, real(real64) id, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 3
        !! @code{.f90}
        !! logical function get(class(csv_column) this, integer(int32) index, logical id, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The csv_column object.
        !! @param[in] index The index of the item to retrieve.
        !! @param[in] id A dummy type defining the data type to retrieve.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p index is outside
        !!      the bounds of the column.
        !!  - FCORE_DATA_TYPE_ERROR: Occurs if @p id doesn't match the stored
        !!      data type.
        !!
        !! @return The requested value.
        generic, public :: get => cc_get_string_item, cc_get_numeric_item, &
            cc_get_logical_item
        !> @brief Sets the requested item from the collection.
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! subroutine set(class(csv_column) this, integer(int32) index, character(len = *) x, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 2
        !! @code{.f90}
        !! subroutine set(class(csv_column) this, integer(int32) index, real(real64) x, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 3
        !! @code{.f90}
        !! subroutine set(class(csv_column) this, integer(int32) index, logical x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The csv_column object.
        !! @param[in] index The index of the item to retrieve.
        !! @param[in] x The item to place into the column.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p index is outside
        !!      the bounds of the column.
        !!  - FCORE_DATA_TYPE_ERROR: Occurs if @p x doesn't match the stored
        !!      data type.
        generic, public :: set => cc_set_string_item, cc_set_numeric_item, &
            cc_set_logical_item
        !> @brief Gets the contents of the column as an array.
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! type(string)(:) function get_array(class(csv_column) this, character(len = *) id, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 2
        !! @code{.f90}
        !! real(real64)(:) function get_array(class(csv_column) this, real(real64) id, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 3
        !! @code{.f90}
        !! logical(:) function get_array(class(csv_column) this, logical id, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The csv_column object.
        !! @param[in] id A dummy type defining the data type to retrieve.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_DATA_TYPE_ERROR: Occurs if @p id doesn't match the stored
        !!      data type.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient 
        !!      memory available to construct the array.
        !!
        !! @return The resulting array.
        generic, public :: get_array => cc_get_string_array, &
            cc_get_numeric_array, cc_get_logical_array
        !> @brief Sets the contents of the column as an array.  Notice, this
        !! action will completely eliminate any contents of the existing
        !! column, and resize the column tightly to this array.
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! subroutine set_array(class(csv_column) this, class(string) x(:), optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 2
        !! @code{.f90}
        !! subroutine set_array(class(csv_column) this, real(real64) x(:), optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 3
        !! @code{.f90}
        !! subroutine set_array(class(csv_column) this, logical x(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The csv_column object.
        !! @param[in] x The array.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_DATA_TYPE_ERROR: Occurs if @p id doesn't match the stored
        !!      data type.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient 
        !!      memory available.
        generic, public :: set_array => cc_set_string_array, &
            cc_set_numeric_array, cc_set_logical_array
        !> @brief Inserts an item into the column.
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! subroutine insert(class(csv_column) this, integer(int32) index, character(len = *) x, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 2
        !! @code{.f90}
        !! subroutine insert(class(csv_column) this, integer(int32) index, real(real64) x, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! subroutine insert(class(csv_column) this, integer(int32) index, logical x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The csv_column object.
        !! @param[in] index The index defining where the item should be 
        !!  inserted.
        !! @param[in] x The item to insert.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_DATA_TYPE_ERROR: Occurs if @p id doesn't match the stored
        !!      data type.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p index is outside
        !!      the bounds of the column.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient 
        !!      memory available.
        generic, public :: insert => cc_insert_string_item, &
            cc_insert_numeric_item, cc_insert_logical_item
        !> @brief Appends an item onto the column.
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! subroutine append(class(csv_column) this, character(len = *) x, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 2
        !! @code{.f90}
        !! subroutine append(class(csv_column) this, real(real64) x, optional class(errors) err)
        !! @endcode
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! subroutine append(class(csv_column) this, logical x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The csv_column object.
        !! @param[in] x The item to append.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_DATA_TYPE_ERROR: Occurs if @p id doesn't match the stored
        !!      data type.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient 
        !!      memory available.
        generic, public :: append => cc_append_string_item, &
            cc_append_numeric_item, cc_append_logical_item
        !> @brief Removes an item from the column.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine remove(class(csv_column), integer(int32) index, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The csv_column object.
        !! @param[in] index The index of the item to remove.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p index is outside
        !!      the bounds of the column.
        procedure, public :: remove => cc_remove_item

        procedure :: cc_get_string_item
        procedure :: cc_get_numeric_item
        procedure :: cc_get_logical_item
        procedure :: cc_set_string_item
        procedure :: cc_set_numeric_item
        procedure :: cc_set_logical_item
        procedure :: cc_get_string_array
        procedure :: cc_get_numeric_array
        procedure :: cc_get_logical_array
        procedure :: cc_set_string_array
        procedure :: cc_set_numeric_array
        procedure :: cc_set_logical_array
        procedure :: cc_insert_string_item
        procedure :: cc_insert_numeric_item
        procedure :: cc_insert_logical_item
        procedure :: cc_append_string_item
        procedure :: cc_append_numeric_item
        procedure :: cc_append_logical_item
    end type

    interface
        pure module function cc_get_header(this) result(rst)
            class(csv_column), intent(in) :: this
            character(len = :), allocatable :: rst
        end function

        module subroutine cc_set_header(this, x)
            class(csv_column), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        pure module function cc_get_data_type(this) result(rst)
            class(csv_column), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine cc_set_data_type(this, x)
            class(csv_column), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function cc_get_data_count(this) result(rst)
            class(csv_column), intent(in) :: this
            integer(int32) :: rst
        end function

        module function cc_get_string_item(this, index, id, err) result(rst)
            class(csv_column), intent(in) :: this
            integer(int32), intent(in) :: index
            character(len = *), intent(in) :: id
            class(errors), intent(inout), optional, target :: err
            character(len = :), allocatable :: rst
        end function

        module function cc_get_numeric_item(this, index, id, err) result(rst)
            class(csv_column), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: id
            class(errors), intent(inout), optional, target :: err
            real(real64) :: rst
        end function

        module function cc_get_logical_item(this, index, id, err) result(rst)
            class(csv_column), intent(in) :: this
            integer(int32), intent(in) :: index
            logical, intent(in) :: id
            class(errors), intent(inout), optional, target :: err
            logical :: rst
        end function

        module subroutine cc_set_string_item(this, index, x, err)
            class(csv_column), intent(inout) :: this
            integer(int32), intent(in) :: index
            character(len = *), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_set_numeric_item(this, index, x, err)
            class(csv_column), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_set_logical_item(this, index, x, err)
            class(csv_column), intent(inout) :: this
            integer(int32), intent(in) :: index
            logical, intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function cc_get_string_array(this, id, err) result(rst)
            class(csv_column), intent(in) :: this
            character(len = *), intent(in) :: id
            class(errors), intent(inout), optional, target :: err
            type(string), allocatable, dimension(:) :: rst
        end function

        module function cc_get_numeric_array(this, id, err) result(rst)
            class(csv_column), intent(in) :: this
            real(real64), intent(in) :: id
            class(errors), intent(inout), optional, target :: err
            real(real64), allocatable, dimension(:) :: rst
        end function

        module function cc_get_logical_array(this, id, err) result(rst)
            class(csv_column), intent(in) :: this
            logical, intent(in) :: id
            class(errors), intent(inout), optional, target :: err
            logical, allocatable, dimension(:) :: rst
        end function

        module subroutine cc_set_string_array(this, x, err)
            class(csv_column), intent(inout) :: this
            class(string), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_set_numeric_array(this, x, err)
            class(csv_column), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_set_logical_array(this, x, err)
            class(csv_column), intent(inout) :: this
            logical, intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_insert_string_item(this, index, x, err)
            class(csv_column), intent(inout) :: this
            integer(int32), intent(in) :: index
            character(len = *), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_insert_numeric_item(this, index, x, err)
            class(csv_column), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_insert_logical_item(this, index, x, err)
            class(csv_column), intent(inout) :: this
            integer(int32), intent(in) :: index
            logical, intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_append_string_item(this, x, err)
            class(csv_column), intent(inout) :: this
            character(len = *), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_append_numeric_item(this, x, err)
            class(csv_column), intent(inout) :: this
            real(real64), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_append_logical_item(this, x, err)
            class(csv_column), intent(inout) :: this
            logical, intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine cc_remove_item(this, index, err)
            class(csv_column), intent(inout) :: this
            integer(int32), intent(in) :: index
            class(errors), intent(inout), optional, target :: err
        end subroutine

        ! TO DO:
        ! - sort - must return an index tracking array
        ! - reorder based upon an index tracking array
    end interface

! ******************************************************************************
! CSV_TABLE.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a CSV data table.
    type csv_table
    private
        ! TO DO: A linked list of CSV_COLUMN objects.  First, a LINKED_LIST
        ! type must be employed in collections.f90.  It seems more efficient
        ! to employ a linked list here vs. a list where reallocations to adjust
        ! size may mean copying significantly large amounts of data; whereas
        ! the linked list could resize without data movement.  Additionally,
        ! it seems the penalty for iterating over the collection is likely
        ! not significant as there are not likely to be large numbers of
        ! columns in the table.
    end type

! ------------------------------------------------------------------------------
end module