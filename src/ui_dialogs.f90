! ui_dialogs.f90

!> @brief This module provides means for displaying GUI type dialogs.
module ui_dialogs
    use iso_c_binding
    use iso_fortran_env
    use strings
    implicit none
    private
    public :: DIALOG_RESULT_CANCEL
    public :: DIALOG_RESULT_OK
    public :: DIALOG_RESULT_YES
    public :: DIALOG_RESULT_NO
    public :: DIALOG_RESULT_RETRY
    public :: DIALOG_RESULT_ABORT
    public :: DIALOG_RESULT_IGNORE
    public :: DIALOG_RESULT_CONTINUE
    public :: MSGBX_NO_ICON
    public :: MSGBX_NO_BUTTON
    public :: MSGBX_ICON_ERROR
    public :: MSGBX_ICON_WARNING
    public :: MSGBX_ICON_QUESTION
    public :: MSGBX_ICON_INFORMATION
    public :: MSGBX_BTN_OK
    public :: MSGBX_BTN_OK_CANCEL
    public :: MSGBX_BTN_YES_NO
    public :: MSGBX_BTN_YES_NO_CANCEL
    public :: MSGBX_BTN_RETRY_CANCEL
    public :: MSGBX_BTN_ABORT_RETRY_IGNORE
    public :: MSGBX_BTN_CANCEL_RETRY_CONTINUE
    public :: dialog_result
    public :: text_output_dialog_result
    public :: file_filter
    public :: initialize_ui_environment
    public :: clean_up_ui_environment
    public :: show_open_file_dialog
    public :: show_open_multifile_dialog
    public :: show_save_file_dialog
    public :: show_browse_folder_dialog
    public :: show_message_box

    ! TO DO: Message-Box type dialog & associated icons, buttons, etc.

! ------------------------------------------------------------------------------
    !> @brief The dialog result is a cancel button press.
    integer(int32), parameter :: DIALOG_RESULT_CANCEL = 0
    !> @brief The dialog result is an OK button press.
    integer(int32), parameter :: DIALOG_RESULT_OK = 1
    !> @brief The dialog result is a yes button press.
    integer(int32), parameter :: DIALOG_RESULT_YES = 2
    !> @brief The dialog result is a no button press.
    integer(int32), parameter :: DIALOG_RESULT_NO = 3
    !> @brief The dialog result is a retry button press.
    integer(int32), parameter :: DIALOG_RESULT_RETRY = 4
    !> @brief The dialog result is an abort button press.
    integer(int32), parameter :: DIALOG_RESULT_ABORT = 5
    !> @brief The dialog result is an ignore button press.
    integer(int32), parameter :: DIALOG_RESULT_IGNORE = 6
    !> @brief The dialog result is a continue button press.
    integer(int32), parameter :: DIALOG_RESULT_CONTINUE = 7
    !> @brief Indicates that no icon should be used for the dialog.
    integer(int32), parameter :: MSGBX_NO_ICON = 0
    !> @brief Indicates that no button should be used for the dialog.
    integer(int32), parameter :: MSGBX_NO_BUTTON = 0
    !> @brief Indicates that an error icon should be used for the dialog.
    integer(int32), parameter :: MSGBX_ICON_ERROR = 1000
    !> @brief Indicates that a warning icon should be used for the dialog.
    integer(int32), parameter :: MSGBX_ICON_WARNING = 1001
    !> @brief Indicates that a question icon should be used for the dialog.
    integer(int32), parameter :: MSGBX_ICON_QUESTION = 1002
    !> @brief Indicates that an information icon should be used for the dialog.
    integer(int32), parameter :: MSGBX_ICON_INFORMATION = 1003
    !> @brief Indicates that an OK button should be used.
    integer(int32), parameter :: MSGBX_BTN_OK = 1004
    !> @brief Indicates that an OK-Cancel button pair should be used.
    integer(int32), parameter :: MSGBX_BTN_OK_CANCEL = 1005
    !> @brief Indicates that a Yes-No button pair should be used.
    integer(int32), parameter :: MSGBX_BTN_YES_NO = 1006
    !> @brief Indicates that a Yes-No-Cancel button set should be used.
    integer(int32), parameter :: MSGBX_BTN_YES_NO_CANCEL = 1007
    !> @brief Indicates that a Retry-Cancel button set should be used.
    integer(int32), parameter :: MSGBX_BTN_RETRY_CANCEL = 1008
    !> @brief Indicates that an Abory-Retry-Ignore button set should be used.
    integer(int32), parameter :: MSGBX_BTN_ABORT_RETRY_IGNORE = 1009
    !> @brief Indicates that a Cancel-Retry-Continue button set should be used.
    integer(int32), parameter :: MSGBX_BTN_CANCEL_RETRY_CONTINUE = 1010

! ------------------------------------------------------------------------------
    !> @brief A type containing results from the dialog interaction.
    type dialog_result
        !> @brief The result flag.
        integer(int32) :: result
    end type

! ------------------------------------------------------------------------------
    !> @brief A type containing results from a dialog that returns text such
    !! as an open-file dialog.
    type, extends(dialog_result) :: text_output_dialog_result
        !> @brief A list of text returned by the dialog
        type(string), allocatable, dimension(:) :: string_list
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a file-type filter.
    type file_filter
        !> @brief The filter name.
        character(len = :), allocatable :: name
        !> @brief The filter pattern.
        character(len = :), allocatable :: pattern
    end type

! ------------------------------------------------------------------------------
    interface
        !> @brief Initializes the UI environment thereby allowing utilization
        !! of the dialogs.  This call must be made prior to calling and dialog
        !! routine.
        !!
        !! @return Returns true if the initialization was successful; else, 
        !! false.
        function init_ui_env_c() bind(C, name = "init_ui_env_c") result(rst)
            use iso_c_binding
            logical(c_bool) :: rst
        end function

        !> @brief Cleans up and finalizes the UI environment.  This should be
        !! called after all UI interactions are complete.
        subroutine clean_up_ui_environment() bind(C, name = "clean_up_ui_env_c")
            ! No arguments
        end subroutine

        !> @brief Shows the user an Open-File dialog.  Notice, the UI 
        !! environment must be initialized prior to calling this routine.
        !!
        !! @param[out] nameBuffer A buffer to which the user-selected 
        !!  filename will be written.
        !! @param[in] bufferSize The size of @p nameBuffer.
        !! @param[out] numChars The number of characters actually written
        !!  to @p nameBuffer.  Notice, if the user presses cancel, or does
        !!  not select any file this value will be 0.
        !! @param[in] nfilters The number of filter strings.
        !! @param[in] filterName An array containing the filter names.
        !! @param[in] filterPattern An array containing the filter pattern.
        subroutine show_open_file_dialog_c(nameBuffer, bufferSize, numChars, &
                nfilters, filterName, filterPattern) &
                bind(C, name = "show_open_file_dialog_c")
            use iso_c_binding
            character(kind = c_char), intent(out) :: nameBuffer(*)
            integer(c_int), intent(in), value :: bufferSize, nfilters
            integer(c_int), intent(out) :: numChars
            type(c_ptr), intent(in) :: filterName(nfilters), &
                filterPattern(nfilters)
        end subroutine

        !> @brief Shows the user an Open-File dialog capable of returning 
        !! multiple files.  Notice, the UI environment must be initialized prior
        !! to calling this routine.
        !!
        !! @param[out] nameBuffers An array of string buffers where the !!
        !!  filenames will be written.
        !! @param[in] numBuffers The number of buffers available.
        !! @param[in] bufferSize The size of each buffer.
        !! @param[out] numNames The number of names actually written.
        !! @param[out] numChars An array containing the number of characters 
        !!  written in each buffer.
        !! @param[in] nfilters The number of filter strings.
        !! @param[in] filterName An array containing the filter names.
        !! @param[in] filterPattern An array containing the filter pattern.
        subroutine show_multi_file_open_file_dialog_c(nameBuffers, numBuffers, &
                bufferSize, numNames, numChars, nfilters, filterName, &
                filterPattern) &
                bind(C, name = "show_multi_file_open_file_dialog_c")
            use iso_c_binding
            integer(c_int), intent(in), value :: numBuffers, bufferSize, &
                nfilters
            type(c_ptr), intent(out) :: nameBuffers(numBuffers)
            integer(c_int), intent(out) :: numNames, numChars(numBuffers)
            type(c_ptr), intent(in) :: filterName(nfilters), &
                filterPattern(nfilters)
        end subroutine

        !> @brief Shows the user a Save-File dialog.  Notice, the UI environment
        !! must be initialized prior to calling this routine.
        !!
        !! @param[in] defaultExt The default file extension to append.
        !! @param[in] nfilters The number of filter strings.
        !! @param[in] filterName An array containing the filter names.
        !! @param[in] filterPattern An array containing the filter pattern.
        !! @param[out] name The user-defined filename.
        !! @param[in] bufferSize the available size of @p name.
        !! @param[out] numChars The actual number of characters written to
        !!  @p name.
        subroutine show_save_file_dialog_c(defaultExt, nfilters, filterName, &
                filterPattern, name, bufferSize, numChars) &
                bind(C, name = "show_save_file_dialog_c")
            use iso_c_binding
            character(kind = c_char), intent(in) :: defaultExt(*)
            integer(c_int), intent(in), value :: nfilters, bufferSize
            type(c_ptr), intent(in) :: filterName(nfilters), &
                filterPattern(nfilters)
            character(kind = c_char), intent(out) :: name(*)
            integer(c_int), intent(out) :: numChars
        end subroutine

        !> @brief Shows the user a Browse-Folder dialog.  Notice, the UI 
        !! environment must be initialized prior to calling this routine.
        !!
        !! @param[out] buffer A buffer to which the output will be written.
        !! @param[in] bufferSize The available size of @p buffer.
        !! @param[out] numChars The actual number of characters written to
        !!  @p buffer.
        subroutine show_browse_folder_dialog_c(buffer, bufferSize, numChars) &
                bind(C, name = "show_browse_folder_dialog_c")
            use iso_c_binding
            character(kind = c_char), intent(out) :: buffer(*)
            integer(c_int), intent(in), value :: bufferSize
            integer(c_int), intent(out) :: numChars
        end subroutine

        !> @brief Shows the user a message box.
        !!
        !! @param[in] parent A pointer to the parent window.  This can be null
        !!  in the event there isn't a parent window.
        !! @param[in] txt The message box text.
        !! @param[in] title The title of the message box.
        !! @param[in] buttons
        !! @param[in] icon
        !! 
        !! @return The dialog result.
        function show_message_box_c(parent, txt, title, buttons, icon) &
                result(rst) bind(C, name = "show_message_box_c")
            use iso_c_binding
            type(c_ptr), intent(in), value :: parent
            character(kind = c_char), intent(in) :: txt(*), title(*)
            integer(c_int), intent(in), value :: buttons, icon
            integer(c_int) :: rst
        end function
    end interface

! ------------------------------------------------------------------------------

contains
! ------------------------------------------------------------------------------
    !> @brief Initializes the UI environment thereby allowing utilization
    !! of the dialogs.  This call must be made prior to calling and dialog
    !! routine.
    !!
    !! @return Returns true if the initialization was successful; else, 
    !! false.
    function initialize_ui_environment() result(rst)
        ! Arguments
        logical :: rst

        ! Process
        rst = logical(init_ui_env_c())
    end function

! ------------------------------------------------------------------------------
    !> @brief Shows the user an Open-File dialog capable of single-file 
    !! selection.
    !!
    !! @param[in] filters An optional array containing file filter information.
    !!
    !! @return The results from the dialog.
    function show_open_file_dialog(filters) result(rst)
        ! Arguments
        class(file_filter), intent(in), dimension(:), optional :: filters
        type(text_output_dialog_result) :: rst

        ! Parameters
        integer(c_int), parameter :: bufferSize = 2048
        integer(c_int), parameter :: maxFilters = 100

        ! Local Variables
        integer(c_int) :: i, nChars, nfilters
        character(kind = c_char, len = bufferSize) :: buffer
        type(c_ptr) :: nullarray(1), namePtrs(maxFilters), &
            patternPtrs(maxFilters)
        character(len = 1024), dimension(maxFilters), target :: nameList, &
            patternList

        ! Initialization
        nullarray = c_null_ptr

        ! Show the user the dialog
        if (present(filters)) then
            nfilters = min(size(filters), maxFilters)
            do i = 1, nfilters
                nameList(i) = filters(i)%name // C_NULL_CHAR
                patternList(i) = filters(i)%pattern // C_NULL_CHAR

                namePtrs(i) = c_loc(nameList(i))
                patternPtrs(i) = c_loc(patternList(i))
            end do
            call show_open_file_dialog_c(buffer, bufferSize, nChars, nfilters, &
                namePtrs(1:nfilters), patternPtrs(1:nfilters))
        else
            call show_open_file_dialog_c(buffer, bufferSize, nChars, 0, &
                nullarray, nullarray)
        end if

        ! Copy the buffer back to a Fortran-friendly string
        if (nChars > 0) then
            rst%result = DIALOG_RESULT_OK
            allocate(rst%string_list(1))
            rst%string_list(1)%str = to_fortran_string(buffer, nChars)
        else
            rst%result = DIALOG_RESULT_CANCEL
            allocate(rst%string_list(0))
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Shows the user an Open-File dialog capable of multiple-file 
    !! selection.
    !!
    !! @param[in] filters An optional array containing file filter information.
    !!
    !! @return The results from the dialog.
    function show_open_multifile_dialog(filters) result(rst)
        ! Arguments
        ! Arguments
        class(file_filter), intent(in), dimension(:), optional :: filters
        type(text_output_dialog_result) :: rst

        ! Parameters
        integer(c_int), parameter :: numBuffers = 1000
        integer(c_int), parameter :: bufferSize = 2048
        integer(c_int), parameter :: maxFilters = 100

        ! Local Variables
        integer(c_int) :: i, nFilters, nNames, nChars(numBuffers)
        character(len = bufferSize), dimension(numBuffers), target :: buffer
        type(c_ptr) :: nullarray(1), namePtrs(maxFilters), &
            patternPtrs(maxFilters), bufferPtrs(numBuffers)
        character(len = 1024), dimension(maxFilters), target :: nameList, &
            patternList

        ! Initialization
        nullarray = C_NULL_PTR
        do i = 1, numBuffers
            bufferPtrs(i) = c_loc(buffer(i))
        end do

        ! Show the user the dialog
        if (present(filters)) then
            nFilters = min(size(filters), maxFilters)
            do i = 1, nFilters
                nameList(i) = filters(i)%name // C_NULL_CHAR
                patternList(i) = filters(i)%pattern // C_NULL_CHAR

                namePtrs(i) = c_loc(nameList(i))
                patternPtrs(i) = c_loc(patternList(i))
            end do
            call show_multi_file_open_file_dialog_c(bufferPtrs, numBuffers, &
                bufferSize, nNames, nChars, nFilters, namePtrs(1:nFilters), &
                patternPtrs(1:nFilters))
        else
            call show_multi_file_open_file_dialog_c(bufferPtrs, numBuffers, &
                bufferSize, nNames, nChars, 0, nullarray, nullarray)
        end if

        ! Collect the output from the buffer
        if (nNames > 0) then
            rst%result = DIALOG_RESULT_OK
            allocate(rst%string_list(nNames))
            do i = 1, nNames
                rst%string_list(i)%str = to_fortran_string(buffer(i), nChars(i))
            end do
        else
            rst%result = DIALOG_RESULT_CANCEL
            allocate(rst%string_list(0))
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Shows the user a Save-File dialog.
    !!
    !! @param[in] defaultExt The default extension to append to the filename in 
    !!  the event the user does not specify an extension (e.g. "txt").
    !! @param[in] filters An array containing file filter information.
    !!
    !! @return The results from the dialog.
    function show_save_file_dialog(defaultExt, filters) result(rst)
        ! Arguments
        character(len = *), intent(in) :: defaultExt
        class(file_filter), intent(in), dimension(:), optional :: filters
        type(text_output_dialog_result) :: rst

        ! Parameters
        integer(c_int), parameter :: bufferSize = 2048
        integer(c_int), parameter :: maxFilters = 100

        ! Local Variables
        integer(c_int) :: i, nchars, nfilters
        character(kind = c_char, len = bufferSize) :: buffer
        type(c_ptr) :: nullarray(1), namePtrs(maxFilters), &
            patternPtrs(maxFilters)
        character(len = 1024), dimension(maxFilters), target :: nameList, &
            patternList
        character(kind = c_char, len = :), allocatable :: ext

        ! Initialization
        nullarray = C_NULL_PTR
        ext = defaultExt // C_NULL_CHAR

        ! Show the user the dialog
        if (present(filters)) then
            nfilters = min(size(filters), maxFilters)
            do i = 1, nfilters
                nameList(i) = filters(i)%name // C_NULL_CHAR
                patternList(i) = filters(i)%pattern // C_NULL_CHAR

                namePtrs(i) = c_loc(nameList(i))
                patternPtrs(i) = c_loc(patternList(i))
            end do
            call show_save_file_dialog_c(ext, nfilters, namePtrs(1:nfilters), &
                patternPtrs(1:nfilters), buffer, bufferSize, nchars)
        else
            call show_save_file_dialog_c(ext, 0, nullarray, nullarray, buffer, &
                bufferSize, nchars)
        end if

        ! Collect the output from the buffer
        if (nchars > 0) then
            rst%result = DIALOG_RESULT_OK
            allocate(rst%string_list(1))
            rst%string_list(1)%str = to_fortran_string(buffer, nchars)
        else
            rst%result = DIALOG_RESULT_CANCEL
            allocate(rst%string_list(0))
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Shows the user a dialog allowing them to browse to a folder.
    !!
    !! @return The results from the dialog.
    function show_browse_folder_dialog() result(rst)
        ! Arguments
        type(text_output_dialog_result) :: rst

        ! Parameters
        integer(c_int), parameter :: bufferSize = 2048

        ! Local Variables
        integer(c_int) :: nchars
        character(kind = c_char, len = bufferSize) :: buffer

        ! Process
        call show_browse_folder_dialog_c(buffer, bufferSize, nchars)
        if (nchars > 0) then
            rst%result = DIALOG_RESULT_OK
            allocate(rst%string_list(1))
            rst%string_list(1)%str = to_fortran_string(buffer, nchars)
        else
            rst%result = DIALOG_RESULT_CANCEL
            allocate(Rst%string_list(0))
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Shows the user a message box dialog.
    !!
    !! @param[in] txt The text to display in the message box.
    !! @param[in] title The title to display in the header of the message box.
    !! @param[in] buttons Defines what button group to display.  The following
    !!  choices are acceptable: MSGBX_BTN_OK, MSGBX_BTN_OK_CANCEL, 
    !!  MSGBX_BTN_YES_NO, MSGBX_BTN_YES_NO_CANCEL, MSGBX_BTN_RETRY_CANCEL, 
    !!  MSGBX_BTN_ABORT_RETRY_IGNORE, MSGBX_BTN_CANCEL_RETRY_CONTINUE.
    !! @param[in] icon Defines what icons to display.  The following choices
    !!  are acceptable: MSGBX_NO_ICON, MSGBX_NO_BUTTON, MSGBX_ICON_ERROR, 
    !!  MSGBX_ICON_WARNING, MSGBX_ICON_QUESTION, MSGBX_ICON_INFORMATION.
    !! @param[in] parent An optional argument defining a pointer to the parent
    !!  window.  If not supplied, no parent is assumed.
    !!
    !! @return The results from the dialog.
    function show_message_box(txt, title, buttons, icon, parent) result(rst)
        ! Arguments
        character(len = *), intent(in) :: txt, title
        integer(int32), intent(in) :: buttons, icon
        type(c_ptr), optional :: parent
        type(dialog_result) :: rst

        ! Local Variables
        character(kind = c_char, len = :), allocatable :: ctxt, ctitle
        type(c_ptr) :: parentPtr

        ! Initialization
        ctxt = txt // C_NULL_CHAR
        ctitle = title // C_NULL_CHAR
        if (present(parent)) then
            parentPtr = parent
        else
            parentPtr = c_null_ptr
        end if

        ! Process
        rst%result = show_message_box_c(parentPtr, ctxt, ctitle, buttons, icon)
    end function

! ------------------------------------------------------------------------------
end module
