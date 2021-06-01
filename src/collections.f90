! collections.f90

!> @brief Provides types supporting various collections of objects.
module collections
    use iso_fortran_env
    use ferror
    implicit none
    private
    public :: list
    public :: items_equal
    public :: compare_items
    public :: dictionary
    public :: hash_code
    public :: linked_list
    public :: data_table

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief A container type allowing storage of any Fortran type.
    type container
        !> A pointer to a polymorphic variable allowing storage of any type.
        class(*), pointer :: item => null()
    end type

    !> @brief A node in a linked list container.
    type node
        !> A pointer to a polymorphic variable allowing storage of any type.
        class(*), pointer :: item => null()
        !> A pointer to the next node in the collection.
        type(node), pointer :: next => null()
        !> A pointer to the previous node in the collection.
        type(node), pointer :: previous => null()
    end type

    !> @brief Defines a generic list.
    type list
    private
        !> A collection of container objects.
        type(container), allocatable, dimension(:) :: m_list
        !> The actual number of items in m_list
        integer(int32) :: m_count = 0
    contains
        !> @brief Cleans up resources held by the list.
        final :: list_destroy
        !> @brief Gets the capacity of the list.
        procedure, public :: get_capacity => list_get_capacity
        !> @brief Sets the capacity of the list.
        procedure, public :: set_capacity => list_set_capacity
        !> @brief Gets the number of items in the list.
        procedure, public :: get_count => list_get_count
        !> @brief Gets an item from the list.
        procedure, public :: get => list_get
        !> @brief Sets an item into the list.
        procedure, public :: set => list_set
        !> @brief Pushes an item onto the end of the list.
        procedure, public :: push => list_push
        !> @brief Pops the last item from the end of the list.
        procedure, public :: pop => list_pop
        !> @brief Clears the contents of the list.
        procedure, public :: clear => list_clear
        !> @brief Creates a deep copy of the list.
        procedure, public :: copy => list_copy
        !> @brief Inserts an item into the list.
        procedure, public :: insert => list_insert
        !> @brief Removes an item from the list.
        procedure, public :: remove => list_remove
        !> @brief Reverses the contents of the list.
        procedure, public :: reverse => list_reverse
        !> @brief Tests to see if the list contains the specified item.
        procedure, public :: contains => list_contains
        !> @brief Finds the index of the first item in the list that matches
        !! the specified object.
        procedure, public :: index_of => list_index_of
        !> @brief Finds the indices of all items in the list that match the 
        !! specified object.
        procedure, public :: indices_of_all => list_indices_of_all
        !> @brief Swaps two items in the list
        procedure, public :: swap_items => list_swap
        !> @brief Sorts an array into ascending order.
        procedure, public :: sort => list_sort

        !> @brief Stores an item in the collection.  If the collection isn't 
        !! large enough to accomodate, it is automatically resized to 
        !! accomodate.
        procedure, private :: store => list_store
    end type

    !> @brief Defines a key-value pair.
    type key_value_pair
        !> @brief The key
        integer(int64) :: key
        !> A pointer to a polymorphic variable allowing storage of any type.
        class(*), pointer :: value => null()
    end type

    !> @brief Defines a generic dictionary.
    type dictionary
    private
        !> @brief A collection of key_value_pair objects.
        type(list) :: m_list
    contains
        !> @brief Cleans up resources held by the dictionary.
        final :: dict_final
        !> @brief Gets the number of items in the dictionary.
        procedure, public :: get_count => dict_get_count
        !> @brief Determines if the dictionary contains the specified key.
        procedure, public :: contains_key => dict_contains_key
        !> @brief Gets the requested item from the dictionary.
        procedure, public :: get => dict_get
        !> @brief Sets an item into the dictionary.
        procedure, public :: set => dict_set
        !> @brief Adds a new key-value pair to the dictionary.
        procedure, public :: add => dict_add
        !> @brief Removes an item from the dictionary.
        procedure, public :: remove => dict_remove
        !> @brief Clears the contents of the entire dictionary.
        procedure, public :: clear => dict_clear

        !> @brief Returns the index in the underlying collection of the entry 
        !! that contains the matching key.
        procedure, private :: index_of_key => dict_index_of_key
    end type

    !> @brief A hash code generation object.
    !!
    !! @par Remarks
    !! This object uses the CRC-32 code provided by zmiimz, and is available
    !! at https://github.com/zmiimz/fortran_notes.
    type hash_code
    private
        integer(int64), dimension(256) :: table
        logical :: have_table = .false.
        integer(int64) :: crcpoly = int(z'edb88320', int64)
        integer(int64) :: crcinv = int(z'5b358fd3', int64)
        integer(int64) :: initxor = int(z'ffffffff', int64)
        integer(int64) :: finalxor = int(z'ffffffff', int64)
    contains
        !> @brief Initializes the hash code generator object.
        procedure, public :: initialize => hc_init
        !> @brief Gets a unique hash code for the supplied string.
        procedure, public :: get => hc_get
    end type

    !> @brief Defines a generic linked-list container.
    type linked_list
    private
        !> @brief The number of nodes in the container.
        integer(int32) :: m_nodeCount = 0
        !> @brief A pointer to the first node in the container.
        type(node), pointer :: m_first => null()
        !> @brief A pointer to the last node in the container.
        type(node), pointer :: m_last => null()
        !> @brief A pointer to the current node. - for iteration purposes
        type(node), pointer :: m_current => null()
    contains
        !> @brief Cleans up resources held by the list.
        final :: ll_final
        !> @brief Gets the number of items in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_count(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in] this The linked_list object.
        !! @return The number of items in the list.
        procedure, public :: get_count => ll_get_count
        !> @brief Moves to the first item in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical move_to_first(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @return Returns true if the move was successful (there was something
        !!  defined to move to); else, false if the move was not completed.
        procedure, public :: move_to_first => ll_move_to_first
        !> @brief Moves to the last item in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical move_to_last(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @return Returns true if the move was successful (there was something
        !!  defined to move to); else, false if the move was not completed.
        procedure, public :: move_to_last => ll_move_to_last
        !> @brief Moves to the next item in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical move_to_next(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @return Returns true if the move was successful (there was something
        !!  defined to move to); else, false if the move was not completed.
        procedure, public :: move_to_next => ll_move_to_next
        !> @brief Moves to the previous item in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical move_to_previous(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @return Returns true if the move was successful (there was something
        !!  defined to move to); else, false if the move was not completed.
        procedure, public :: move_to_previous => ll_move_to_previous
        !> @brief Clears the entire contents of the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in] this The linked_list object.
        procedure, public :: clear => ll_clear
        !> @brief Pushes a new item onto the end of the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine push(class(linked_list) this, class(*) x, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @param[in] x The object to store.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: push => ll_push
        !> @brief Pops the last item off of the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine pop(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        procedure, public :: pop => ll_pop
        !> @brief Gets a pointer to the current item.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(*), pointer get(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in] this The linked_list object.
        !! @return The requested pointer.
        procedure, public :: get => ll_get_item
        !> @brief Sets an item into the list at the current location.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set(class(linked_list) this, class(*) x, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @param[in] x The item to place in the list.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: set => ll_set_item
        !> @brief Tests to see if the specified item exists in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical contains(class(linked_list) this, class(*) item, procedure(items_equal) fcn)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @param[in] item The item to search for.
        !! @param[in] fcn A pointer to the routine used to compare items.
        !! @return Returns true if @p item is found; else, false.
        procedure, public :: contains => ll_contains
        !> @brief Moves to the first occurrence of the item that matches the
        !!  specified criteria.  If no match is found the move does not happen.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical move_to(class(linked_list) this, class(*) item, procedure(items_equal) fcn)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @param[in] item The item to search for.
        !! @param[in] fcn A pointer to the routine used to compare items.
        !! @return Returns true if the item was found and the move was 
        !!  successful; else, false if the item wasn't found and the move did
        !!  not occur.
        procedure, public :: move_to => ll_move_to_matching
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a table convenient for storing mixed-type data.
    type data_table
    private
        !> @brief The data table.
        type(container), allocatable, dimension(:,:) :: m_table
    contains
        final :: dt_final
        !> @brief Clears the entire contents of the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear(class(data_table) this)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        procedure, public :: clear => dt_clear
        !> @brief Gets the number of rows in the table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_row_count(class(data_table) this)
        !! @endcode
        !!
        !! @param[in] this The data_table object.
        !! @return The number of rows in the table.
        procedure, public :: get_row_count => dt_get_row_count
        !> @brief Gets the number of columns in the table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_column_count(class(data_table) this)
        !! @endcode
        !!
        !! @param[in] this The data_table object.
        !! @return The number of columns in the table.
        procedure, public :: get_column_count => dt_get_column_count
        !> @brief Initializes the data table.  Notice, if the data table was
        !! already initialized, this routine will clear the existing table
        !! and construct a new table as requested.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(data_table) this, integer(int32) m, integer(int32) n, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] m The number of rows (must be at least 1).
        !! @param[in] n The number of columns (must be at least 1).
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        !!  - FCORE_INVALID_INPUT_ERROR: Occurs if @p m or @p n is less than
        !!      or equal to zero.
        procedure, public :: initialize => dt_initialize
        !> @brief Gets an item from the table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(*) pointer get(class(data_table) this, integer(int32) i, integer(int32) j, class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The data_table object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p i or @p j are outside
        !!      the bounds of the table.
        procedure, public :: get => dt_get
        !> @brief Sets an item into the table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set(class(data_table) this, integer(int32) i, integer(int32) j, class(*) x, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @param[in] x The item to set into the table.  Notice, a copy is made
        !!  and the table takes care of management of the memory occupied by 
        !!  the copy.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_NULL_REFERENCE_ERROR: Occurs if the table hasn't yet been
        !!      initialized.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p i or @p j are outside
        !!      the bounds of the table.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: set => dt_set
        !> @brief Inserts a series of rows into the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine insert_rows(class(data_table) this, integer(int32) rstart, class(*) x(:,:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] rstart The index of the row at which the insertion begins.
        !! @param[in] x An M-by-N matrix of items to insert into the table.  The
        !!  number of columns (N) must be the same as the number of columns in
        !!  this table.  A copy of each item is made, and the data_table takes
        !!  care of management of the memory occupied by each copy.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p rstart is outside 
        !!      the bounds of the table.
        !!  - FCORE_ARRAY_SIZE_ERROR: Occurs if @p x does not have the same
        !!      number of columns as the data_table.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: insert_rows => dt_insert_rows
        !> @brief Inserts a single row into the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine insert_row(class(data_table) this, integer(int32) i, class(*) x(:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] i The row index where @p x should be inserted into the table.
        !! @param[in] x The N-element array where N is equal to the number of
        !!  columns in this data_table.  A copy of each item is made, and the 
        !!  data_table takes care of management of the memory occupied by each 
        !!  copy.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p i is outside 
        !!      the bounds of the table.
        !!  - FCORE_ARRAY_SIZE_ERROR: Occurs if @p x does not have the same
        !!      number of items as the data_table has columns.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: insert_row => dt_insert_row
        !> @brief Inserts a series of columns into the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine insert_columns(class(data_table) this, integer(int32) cstart, class(*) x(:,:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] cstart The index of the column at which the insertion 
        !!  begins.
        !! @param[in] x An M-by-N matrix of items to insert into the table.  The
        !!  number of rows (M) must be the same as the number of rows in
        !!  this table.  A copy of each item is made, and the data_table takes
        !!  care of management of the memory occupied by each copy.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p cstart is outside 
        !!      the bounds of the table.
        !!  - FCORE_ARRAY_SIZE_ERROR: Occurs if @p x does not have the same
        !!      number of rows as the data_table.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: insert_columns => dt_insert_columns
        !> @brief Inserts a single column into the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine insert_column(class(data_table) this, integer(int32) i, class(*) x(:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] i The column index where @p x should be inserted into 
        !!  the table.
        !! @param[in] x The N-element array where N is equal to the number of
        !!  rows in this data_table.  A copy of each item is made, and the 
        !!  data_table takes care of management of the memory occupied by each 
        !!  copy.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p i is outside 
        !!      the bounds of the table.
        !!  - FCORE_ARRAY_SIZE_ERROR: Occurs if @p x does not have the same
        !!      number of items as the data_table has rows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: insert_column => dt_insert_column
        !> @brief Appends a series of rows onto the end of the table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine append_rows(class(data_table) this, class(*) x(:,:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] x An M-by-N matrix of items to append onto the table.  The
        !!  number of columns (N) must be the same as the number of columns in
        !!  this table.  A copy of each item is made, and the data_table takes
        !!  care of management of the memory occupied by each copy.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_ARRAY_SIZE_ERROR: Occurs if @p x does not have the same
        !!      number of columns as the data_table.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: append_rows => dt_append_rows
        !> @brief Appends a single row onto the end of the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine append_row(class(data_table) this, class(*) x(:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] x The N-element array where N is equal to the number of
        !!  columns in this data_table.  A copy of each item is made, and the 
        !!  data_table takes care of management of the memory occupied by each 
        !!  copy.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_ARRAY_SIZE_ERROR: Occurs if @p x does not have the same
        !!      number of items as the data_table has columns.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: append_row => dt_append_row
        !> @brief Appends a series of columns onto the end of the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine append_columns(class(data_table) this, class(*) x(:,:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] x An M-by-N matrix of items to insert into the table.  The
        !!  number of rows (M) must be the same as the number of rows in
        !!  this table.  A copy of each item is made, and the data_table takes
        !!  care of management of the memory occupied by each copy.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_ARRAY_SIZE_ERROR: Occurs if @p x does not have the same
        !!      number of rows as the data_table.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: append_columns => dt_append_columns
        !> @brief Appends a single column onto the end of the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine append_column(class(data_table) this, class(*) x(:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] x The N-element array where N is equal to the number of
        !!  rows in this data_table.  A copy of each item is made, and the 
        !!  data_table takes care of management of the memory occupied by each 
        !!  copy.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_ARRAY_SIZE_ERROR: Occurs if @p x does not have the same
        !!      number of items as the data_table has rows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        procedure, public :: append_column => dt_append_column
        !> @brief Removes a series of rows from the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine remove_rows(class(data_table) this, integer(int32) rstart, integer(int32) nrows, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] rstart The row index at which to start removing rows.
        !! @param[in] nrows The number of rows to remove (must be greater than
        !!  0).
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        !!  - FCORE_UNINITIALIZED_OBJECT_ERROR: Occurs if the table has not
        !!      been initialized.
        !!  - FCORE_INVALID_INPUT_ERROR: Occurs if @p nrows is less than 1, or
        !!      if the @p nrows is too large resulting in removing more rows 
        !!      than are available in the table.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p rstart is outside
        !!      the bounds of the table.
        procedure, public :: remove_rows => dt_remove_rows
        !> @brief Removes a series of columns from the data_table.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine remove_columns(class(data_table) this, integer(int32) cstart, integer(int32) ncols, class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The data_table object.
        !! @param[in] cstart The column index at which to start removing 
        !!  columns.
        !! @param[in] ncols The number of columns to remove (must be greater 
        !!  than 0).
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
        !!  - FCORE_UNINITIALIZED_OBJECT_ERROR: Occurs if the table has not
        !!      been initialized.
        !!  - FCORE_INVALID_INPUT_ERROR: Occurs if @p ncols is less than 1, or
        !!      if the @p ncols is too large resulting in removing more columns 
        !!      than are available in the table.
        !!  - FCORE_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p cstart is outside
        !!      the bounds of the table.
        procedure, public :: remove_columns => dt_remove_columns
    end type

! ******************************************************************************
! FUNCTION PROTOTYPES
! ------------------------------------------------------------------------------
    interface
        !> @brief Compares two items, and returns a logical value signifying
        !! the comparison results.
        !!
        !! @param[in] item1 The first item.
        !! @param[in] item2 The second item.
        !!
        !! @return Returns true if the comparison is valid; else, false.
        function items_equal(item1, item2) result(rst)
            class(*), intent(in) :: item1, item2
            logical :: rst
        end function

        !> @brief Compares two items, and returns 1 if @p item1 is greater than
        !! @p item2, 0 if @p item1 is equal to @p item2, or -1 if @p item1 is
        !! less than @p item2.
        !!
        !! @param[in] item1 The first item.
        !! @param[in] item2 The second item.
        !!
        !! @return Returns 1 if @p item1 is greater than @p item2, 0 if
        !! @p item1 is equal to @p item2, or -1 if @p item1 is less than 
        !! @p item2.
        function compare_items(item1, item2) result(rst)
            use iso_fortran_env, only : int32
            class(*), intent(in) :: item1, item2
            integer(int32) :: rst
        end function
    end interface

! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    interface ! collections_list.f90
        pure module function list_get_capacity(this) result(rst)
            class(list), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine list_set_capacity(this, n, err)
            class(list), intent(inout) :: this
            integer(int32), intent(in) :: n
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function list_get_count(this) result(rst)
            class(list), intent(in) :: this
            integer(int32) :: rst
        end function

        module function list_get(this, i) result(x)
            class(list), intent(in) :: this
            integer(int32), intent(in) :: i
            class(*), pointer :: x
        end function

        module subroutine list_set(this, i, x, err)
            class(list), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(*), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_store(this, i, x, err)
            class(list), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(*), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_push(this, x, err)
            class(list), intent(inout) :: this
            class(*), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_pop(this)
            class(list), intent(inout) :: this
        end subroutine

        module subroutine list_clear(this)
            class(list), intent(inout) :: this
        end subroutine

        module function list_copy(this, err) result(rst)
            class(list), intent(in) :: this
            class(errors), intent(inout), optional, target :: err
            type(list) :: rst
        end function

        module subroutine list_insert(this, i, x, err)
            class(list), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(*), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_remove(this, i, err)
            class(list), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_destroy(this)
            type(list), intent(inout) :: this
        end subroutine

        module subroutine list_reverse(this)
            class(list), intent(inout) :: this
        end subroutine

        module function list_contains(this, item, fcn) result(rst)
            class(list), intent(in) :: this
            class(*), intent(in) :: item
            procedure(items_equal), pointer, intent(in) :: fcn
            logical :: rst
        end function

        module function list_index_of(this, item, fcn) result(rst)
            class(list), intent(in) :: this
            class(*), intent(in) :: item
            procedure(items_equal), pointer, intent(in) :: fcn
            integer(int32) :: rst
        end function

        module function list_indices_of_all(this, item, fcn, err) result(rst)
            class(list), intent(in) :: this
            class(*), intent(in) :: item
            procedure(items_equal), pointer, intent(in) :: fcn
            class(errors), intent(inout), optional, target :: err
            integer(int32), allocatable, dimension(:) :: rst
        end function

        module subroutine list_swap(this, i1, i2, err)
            class(list), intent(inout) :: this
            integer(int32), intent(in) :: i1, i2
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_sort(this, fcn)
            class(list), intent(inout) :: this
            procedure(compare_items), pointer, intent(in) :: fcn
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    interface ! collections_dictionary.f90
        pure module function dict_get_count(this) result(rst)
            class(dictionary), intent(in) :: this
            integer(int32) :: rst
        end function

        module function dict_index_of_key(this, key) result(rst)
            class(dictionary), intent(in) :: this
            integer(int64), intent(in) :: key
            integer(int32) :: rst
        end function

        module function dict_contains_key(this, key) result(rst)
            class(dictionary), intent(in) :: this
            integer(int64), intent(in) :: key
            logical :: rst
        end function

        module function dict_get(this, key) result(rst)
            class(dictionary), intent(in) :: this
            integer(int64), intent(in) :: key
            class(*), pointer :: rst
        end function

        module subroutine dict_set(this, key, item, err)
            class(dictionary), intent(inout) :: this
            integer(int64), intent(in) :: key
            class(*), intent(in) :: item
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dict_add(this, key, item, err)
            class(dictionary), intent(inout) :: this
            integer(int64), intent(in) :: key
            class(*), intent(in) :: item
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function dict_remove(this, key) result(rst)
            class(dictionary), intent(inout) :: this
            integer(int64), intent(in) :: key
            logical :: rst
        end function

        module subroutine dict_clear(this)
            class(dictionary), intent(inout) :: this
        end subroutine

        module subroutine dict_final(this)
            type(dictionary), intent(inout) :: this
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    interface ! collections_hash.f90
        module subroutine hc_init(this)
            class(hash_code), intent(inout) :: this
        end subroutine

        module function hc_get(this, str) result(rst)
            class(hash_code), intent(inout) :: this
            character(len = *), intent(in) :: str
            integer(int64) :: rst
        end function
    end interface

! ------------------------------------------------------------------------------
    interface ! collections_linked_list.f90
        pure module function ll_get_count(this) result(rst)
            class(linked_list), intent(in) :: this
            integer(int32) :: rst
        end function
        
        module function ll_move_to_first(this) result(rst)
            class(linked_list), intent(inout) :: this
            logical :: rst
        end function

        module function ll_move_to_last(this) result(rst)
            class(linked_list), intent(inout) :: this
            logical :: rst
        end function

        module function ll_move_to_next(this) result(rst)
            class(linked_list), intent(inout) :: this
            logical :: rst
        end function

        module function ll_move_to_previous(this) result(rst)
            class(linked_list), intent(inout) :: this
            logical :: rst
        end function

        module subroutine ll_clear(this)
            class(linked_list), intent(inout) :: this
        end subroutine

        module subroutine ll_final(this)
            type(linked_list), intent(inout) :: this
        end subroutine

        module subroutine ll_push(this, x, err)
            class(linked_list), intent(inout) :: this
            class(*), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine ll_pop(this)
            class(linked_list), intent(inout) :: this
        end subroutine

        module function ll_get_item(this) result(rst)
            class(linked_list), intent(in) :: this
            class(*), pointer :: rst
        end function

        module subroutine ll_set_item(this, x, err)
            class(linked_list), intent(inout) :: this
            class(*), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function ll_contains(this, item, fcn) result(rst)
            class(linked_list), intent(inout) :: this
            class(*), intent(in) :: item
            procedure(items_equal), pointer, intent(in) :: fcn
            logical :: rst
        end function

        module function ll_move_to_matching(this, item, fcn) result(rst)
            class(linked_list), intent(inout) :: this
            class(*), intent(in) :: item
            procedure(items_equal), pointer, intent(in) :: fcn
            logical :: rst
        end function
    end interface

! ------------------------------------------------------------------------------
    interface ! collections_data.f90
        module subroutine dt_clear(this)
            class(data_table), intent(inout) :: this
        end subroutine

        module subroutine dt_final(this)
            type(data_table), intent(inout) :: this
        end subroutine

        pure module function dt_get_row_count(this) result(rst)
            class(data_table), intent(in) :: this
            integer(int32) :: rst
        end function

        pure module function dt_get_column_count(this) result(rst)
            class(data_table), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine dt_initialize(this, m, n, err)
            class(data_table), intent(inout) :: this
            integer(int32), intent(in) :: m, n
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function dt_get(this, i, j, err) result(rst)
            class(data_table), intent(in) :: this
            integer(int32) :: i, j
            class(errors), intent(inout), optional, target :: err
            class(*), pointer :: rst
        end function

        module subroutine dt_set(this, i, j, x, err)
            class(data_table), intent(inout) :: this
            integer(int32), intent(in) :: i, j
            class(*), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_insert_rows(this, rstart, x, err)
            class(data_table), intent(inout) :: this
            integer(int32), intent(in) :: rstart
            class(*), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_insert_row(this, i, x, err)
            class(data_table), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(*), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_insert_columns(this, cstart, x, err)
            class(data_table), intent(inout) :: this
            integer(int32), intent(in) :: cstart
            class(*), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_insert_column(this, i, x, err)
            class(data_table), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(*), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_append_rows(this, x, err)
            class(data_table), intent(inout) :: this
            class(*), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_append_row(this, x, err)
            class(data_table), intent(inout) :: this
            class(*), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_append_columns(this, x, err)
            class(data_table), intent(inout) :: this
            class(*), intent(in), dimension(:,:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_append_column(this, x, err)
            class(data_table), intent(inout) :: this
            class(*), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_remove_rows(this, rstart, nrows, err)
            class(data_table), intent(inout) :: this
            integer(int32), intent(in) :: rstart, nrows
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine dt_remove_columns(this, cstart, ncols, err)
            class(data_table), intent(inout) :: this
            integer(int32), intent(in) :: cstart, ncols
            class(errors), intent(inout), optional, target :: err
        end subroutine
        

        ! TO DO:
        ! - get row
        ! - get column
        ! - get sub-table
        ! - get/set column headers
        ! - contains
        ! - find
        ! - index of
        ! - sort by column
    end interface

! ------------------------------------------------------------------------------
end module
