! collections_linked_list.f90

submodule (collections) collections_linked_list
    use fcore_constants
contains
! ------------------------------------------------------------------------------
    pure module function ll_get_count(this) result(rst)
        class(linked_list), intent(in) :: this
        integer(int32) :: rst
        rst = this%m_nodeCount
    end function

! ------------------------------------------------------------------------------
    module function ll_move_to_first(this) result(rst)
        class(linked_list), intent(inout) :: this
        logical :: rst
        if (associated(this%m_first)) then
            this%m_current => this%m_first
            rst = .true.
        else
            rst = .false.
        end if
    end function

! ------------------------------------------------------------------------------
    module function ll_move_to_last(this) result(rst)
        class(linked_list), intent(inout) :: this
        logical :: rst
        if (associated(this%m_last)) then
            this%m_current => this%m_last
            rst = .true.
        else
            rst = .false.
        end if
    end function

! ------------------------------------------------------------------------------
    module function ll_move_to_next(this) result(rst)
        class(linked_list), intent(inout) :: this
        logical :: rst
        if (.not.associated(this%m_current)) then
            rst = .false.
            return
        end if
        if (associated(this%m_current%next)) then
            this%m_current => this%m_current%next
            rst = .true.
        else
            rst = .false.
        end if
    end function

! ------------------------------------------------------------------------------
    module function ll_move_to_previous(this) result(rst)
        class(linked_list), intent(inout) :: this
        logical :: rst
        if (.not.associated(this%m_current)) then
            rst = .false.
            return
        end if
        if (associated(this%m_current%previous)) then
            this%m_current => this%m_current%previous
            rst = .true.
        else
            rst = .false.
        end if
    end function

! ------------------------------------------------------------------------------
    module subroutine ll_clear(this)
        ! Arguments
        class(linked_list), intent(inout) :: this

        ! Local Variables
        integer(int32) :: i
        class(node), pointer :: currentNode

        ! Process
        if (this%get_count() == 0) return
        this%m_current => this%m_first
        do i = 1, this%get_count()
            ! Get a pointer to the next node
            currentNode => this%m_current
            if (associated(currentNode)) then
                ! Delete the current node)
                if (associated(currentNode%item)) deallocate(currentNode%item)

                ! Move to the next node
                this%m_current => currentNode%next

                ! Clean up the current node
                deallocate(currentNode)
            end if
        end do
        this%m_nodeCount = 0
        this%m_first => null()
        this%m_last => null()
        this%m_current => null()
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine ll_final(this)
        type(linked_list), intent(inout) :: this
        call this%clear()
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine ll_push(this, x, err)
        ! Arguments
        class(linked_list), intent(inout) :: this
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag
        type(node), pointer :: newNode
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Set up error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Create a new node
        allocate(newNode, stat = flag)
        if (flag == 0) allocate(newNode%item, source = x, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("ll_push", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Make the necessary links
        if (this%m_nodeCount == 0) then
            this%m_first => newNode
            this%m_last => newNode
            this%m_current => newNode
            this%m_nodeCount = 1
        else
            newNode%previous => this%m_last
            this%m_last%next => newNode
            this%m_last => newNode
            this%m_nodeCount = this%m_nodeCount + 1
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine ll_pop(this)
        ! Arguments
        class(linked_list), intent(inout) :: this

        ! Local Variables
        type(node), pointer :: lastNode

        ! Quick Return
        if (this%m_nodeCount <= 1) then
            call this%clear()
            return
        end if

        ! Destroy the last node, and make the previous node the last node
        lastNode => this%m_last%previous
        lastNode%next => null()

        if (associated(this%m_last%item)) deallocate(this%m_last%item)
        deallocate(this%m_last)

        this%m_last => lastNode
        this%m_nodeCount = this%m_nodeCount - 1
    end subroutine

! ------------------------------------------------------------------------------
    module function ll_get_item(this) result(rst)
        class(linked_list), intent(in) :: this
        class(*), pointer :: rst
        rst => null()
        if (associated(this%m_current)) rst => this%m_current%item
    end function

! ------------------------------------------------------------------------------
    module subroutine ll_set_item(this, x, err)
        ! Arguments
        class(linked_list), intent(inout) :: this
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Set up the error handling
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Quick Return
        if (.not.associated(this%m_current)) return

        ! Process
        if (associated(this%m_current%item)) deallocate(this%m_current%item)
        allocate(this%m_current%item, source = x, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("ll_set_item", &
                "Insufficient memory available.", FCORE_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function ll_contains(this, item, fcn) result(rst)
        ! Arguments
        class(linked_list), intent(inout) :: this
        class(*), intent(in) :: item
        procedure(items_equal), pointer, intent(in) :: fcn
        logical :: rst

        ! Local Variables
        logical :: check
        class(node), pointer :: current

        ! Process
        current => this%m_current
        check = this%move_to_first()
        rst = .false.
        do while (check)
            if (fcn(item, this%get())) then
                rst = .true.
                exit
            end if
            check = this%move_to_next()
        end do
        this%m_current => current
    end function

! ------------------------------------------------------------------------------
    module function ll_move_to_matching(this, item, fcn) result(rst)
        ! Arguments
        class(linked_list), intent(inout) :: this
        class(*), intent(in) :: item
        procedure(items_equal), pointer, intent(in) :: fcn
        logical :: rst

        ! Local Variables
        logical :: check

        ! Process
        check = this%move_to_first()
        rst = .false.
        do while (check)
            if (fcn(item, this%get())) then
                rst = .true.
                exit
            end if
            check = this%move_to_next()
        end do
    end function

! ------------------------------------------------------------------------------
end submodule
