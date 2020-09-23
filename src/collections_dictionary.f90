! collections_dictionary.f90

submodule (collections) collections_dictionary
    use fcore_constants
contains
! ------------------------------------------------------------------------------
    !> @brief Gets the number of items in the dictionary.
    !!
    !! @param[in] this The dictionary object.
    !! @return The number of items in the dictionary.
    pure module function dict_get_count(this) result(rst)
        ! Arguments
        class(dictionary), intent(in) :: this
        integer(int32) :: rst

        ! Process
        rst = this%m_list%get_count()
    end function

! ------------------------------------------------------------------------------
    !> @brief Returns the index in the underlying collection of the entry that
    !! contains the matching key.
    !!
    !! @param[in] this The dictionary object.
    !! @param[in] key The desired key.
    !!
    !! @return If found, the index of the matching entry in the underlying
    !!  collection.  If not found, a value of 0 is returned.
    module function dict_index_of_key(this, key) result(rst)
        ! Arguments
        class(dictionary), intent(in) :: this
        integer(int64), intent(in) :: key
        integer(int32) :: rst

        ! Local Variables
        integer(int32) :: i, n
        class(*), pointer :: item

        ! Initialization
        rst = 0

        ! Cycle over each item until we find the appropriate key
        n = this%m_list%get_count()
        do i = 1, n
            item => this%m_list%get(i)
            select type (item)
                class is (key_value_pair)
                    if (item%key == key) then
                        rst = i
                        exit
                    end if
            end select
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Determines if the dictionary contains the specified key.
    !!
    !! @param[in] this The dictionary object.
    !! @param[in] key The desired key.
    !!
    !! @return Returns true if the dictionary contains @p key; else, returns
    !!  false.
    module function dict_contains_key(this, key) result(rst)
        ! Arguments
        class(dictionary), intent(in) :: this
        integer(int64), intent(in) :: key
        logical :: rst

        ! Local Variables
        integer(int32) :: index

        ! Process
        index = this%index_of_key(key)
        rst = (index > 0)
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the requested item from the dictionary.
    !!
    !! @param[in] this The dictionary object.
    !! @param[in] key The key of the desired object.
    !!
    !! @return A pointer to the requested item.  A null pointer is returned
    !!  if the key doesn't exist in the collection.
    module function dict_get(this, key) result(rst)
        ! Arguments
        class(dictionary), intent(in) :: this
        integer(int64), intent(in) :: key
        class(*), pointer :: rst

        ! Local Variables
        integer(int32) :: index
        class(*), pointer :: item

        ! Process
        nullify(rst)
        index = this%index_of_key(key)
        if (index > 0) then
            item => this%m_list%get(index)
            select type (item)
                class is (key_value_pair)
                    rst => item%value
            end select
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets an item into the dictionary.  If the item's key doesn't
    !!  already exist an error is thrown.
    !!
    !! @param[in,out] this The dictionary object.
    !! @param[in] key The key.
    !! @param[in] item The item to place into the dictionary.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
    !!  - FCORE_NONEXISTENT_KEY_ERROR: Occurs if @p key could not be found.
    module subroutine dict_set(this, key, item, err)
        ! Arguments
        class(dictionary), intent(inout) :: this
        integer(int64), intent(in) :: key
        class(*), intent(in) :: item
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: index, flag
        class(*), pointer :: ptr, cpy
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        index = this%index_of_key(key)
        if (index <= 0) then
            ! ERROR
            call errmgr%report_error("dict_set", &
                "The specified key could not be found in the dictionary.", &
                FCORE_NONEXISTENT_KEY_ERROR)
            return
        end if

        ! Replace the item
        ptr => this%m_list%get(index)
        select type (ptr)
            class is (key_value_pair)
                ! Delete the old object
                if (associated(ptr%value)) deallocate(ptr%value)

                ! Create a copy of item, and store that copy
                allocate(cpy, source = item, stat = flag)
                if (flag /= 0) then
                    call errmgr%report_error("dict_set", &
                        "Insufficient memory available.", &
                        FCORE_OUT_OF_MEMORY_ERROR)
                    return
                end if
                ptr%value => cpy
                call this%m_list%set(index, ptr, errmgr)
                if (errmgr%has_error_occurred()) return
        end select
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Adds a new key-value pair to the dictionary.
    !!
    !! @param[in,out] this The dictionary object.
    !! @param[in] key The key.
    !! @param[in] item The item to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FCORE_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory.
    !!  - FCORE_EXISTING_KEY_ERROR: Occurs if the key already exists within the
    !!      collection.
    module subroutine dict_add(this, key, item, err)
        class(dictionary), intent(inout) :: this
        integer(int64), intent(in) :: key
        class(*), intent(in) :: item
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: index, flag
        type(key_value_pair) :: kvp
        class(*), pointer :: cpy
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        index = this%index_of_key(key)
        if (index > 0) then
            call errmgr%report_error("dict_add", "An item with the " // &
                "specified key already exists within this dictionary.", &
                FCORE_EXISTING_KEY_ERROR)
            return
        end if

        allocate(cpy, source = item, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("dict_add", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if

        kvp%key = key
        kvp%value => cpy
        call this%m_list%push(kvp, errmgr)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Removes an item from the dictionary.
    !!
    !! @param[in,out] this The dictionary object.
    !! @param[in] key The key.
    !! @return Returns true if the item was found within the dictionary and
    !!  removed; else, returns false if the key was not found in the 
    !!  dictionary.
    module function dict_remove(this, key) result(rst)
        ! Arguments
        class(dictionary), intent(inout) :: this
        integer(int64), intent(in) :: key
        logical :: rst

        ! Local Variables
        integer(int32) :: index
        class(*), pointer :: item

        ! Process
        rst = .true.
        index = this%index_of_key(key)
        if (index <= 0) then
            rst = .false.
            return
        end if

        item => this%m_list%get(index)
        select type (item)
            class is (key_value_pair)
                if (associated(item%value)) deallocate(item%value)
        end select

        call this%m_list%remove(index)
    end function

! ------------------------------------------------------------------------------
    !> @brief Clears the contents of the entire dictionary.
    !!
    !! @param[in,out] this The dictionary object.
    module subroutine dict_clear(this)
        ! Arguments
        class(dictionary), intent(inout) :: this

        ! Local Variables
        integer(int32) :: i, n
        class(*), pointer :: item

        ! Process
        n = this%m_list%get_count()
        do i = 1, n
            item => this%m_list%get(i)
            select type (item)
                class is (key_value_pair)
                    if (associated(item%value)) deallocate(item%value)
            end select
        end do
        call this%m_list%clear()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Cleans up resources held by the dictionary.
    !!
    !! @param[in,out] this The dictionary object.
    module subroutine dict_final(this)
        type(dictionary), intent(inout) :: this
        call this%clear()
    end subroutine

! ------------------------------------------------------------------------------
end submodule
