! strings_string_builder.f90

submodule (strings) strings_string_builder
    !> @brief The size of the default buffer to use for string construction.
    integer(int32), parameter :: STRING_BUFFER_SIZE = 2048
contains
! ------------------------------------------------------------------------------
    !> @brief Initializes the string_builder object.
    !!
    !! @param[in,out] this The string_builder object.
    module subroutine sb_init(this)
        ! Arguments
        class(string_builder), intent(inout) :: this

        ! Initialization
        this%m_length = 0
        if (.not.allocated(this%m_buffer)) &
            allocate(character(len = STRING_BUFFER_SIZE) :: this%m_buffer)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Appends to the string.
    !!
    !! @param[in,out] this The string_builder object.
    !! @param[in] txt The string to append.
    module subroutine sb_append(this, txt)
        ! Arguments
        class(string_builder), intent(inout) :: this
        character(len = *), intent(in) :: txt

        ! Local Variables
        integer(int32) :: space, n, start, finish, nb
        character(len = :), allocatable :: temp

        ! Process
        if (.not.allocated(this%m_buffer)) call this%initialize()
        space = len(this%m_buffer) - this%m_length
        n = len(txt)
        if (space < n) then
            ! Reallocate a larger buffer
            nb = len(this%m_buffer)
            allocate(character(len = nb + max(n, STRING_BUFFER_SIZE)) :: temp)
            temp(1:nb) = this%m_buffer
            this%m_buffer = temp
        end if
        start = this%m_length + 1
        finish = start + n - 1
        this%m_buffer(start:finish) = txt(1:n)
        this%m_length = this%m_length + n
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Returns the contents as a single string.
    !!
    !! @param[in] this The string_builder object.
    !! @return The string.
    pure module function sb_to_string(this) result(txt)
        class(string_builder), intent(in) :: this
        character(len = :), allocatable :: txt
        txt = this%m_buffer(1:this%m_length)
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the current length of the string being built.
    !!
    !! @param[in] this The string_builder object.
    !! @return The length.
    pure module function sb_get_length(this) result(n)
        class(string_builder), intent(in) :: this
        integer(int32) :: n
        n = this%m_length
    end function

! ------------------------------------------------------------------------------
    !> @brief Clears the buffer.
    !!
    !! @param[in,out] this The string_builder object.
    module subroutine sb_clear(this)
        ! Arguments
        class(string_builder), intent(inout) :: this

        ! Process
        this%m_length = 0
    end subroutine

! ------------------------------------------------------------------------------
end submodule