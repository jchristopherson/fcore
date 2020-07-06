! file_io.f90

!> @brief This module contains routines to support file I/O operations.
module file_io
    use iso_fortran_env
    use ferror
    implicit none
    private
    public :: file_manager

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a base type for managint file I/O.
    type file_manager
    private
        !> @brief The unit value.
        integer(int32) :: m_unit = -1
        !> @brief Delete file upon closing?
        logical :: m_deleteOnClose = .false.
    contains
        procedure, public :: get_unit => fm_get_unit
    end type

! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    interface
        module function fm_get_unit(this) result(rst)
            class(file_manager), intent(inout) :: this
            integer(int32) :: rst
        end function
    end interface

! ------------------------------------------------------------------------------
end module
