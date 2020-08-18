! ui_dialog_example.f90

program main
    use ui_dialogs
    implicit none

    ! Local Variables
    logical :: check
    type(dialog_result) :: mbox
    type(text_output_dialog_result) :: openfile, savefile, folder
    type(file_filter) :: filters(2)

    ! Initialize the UI environment
    check = initialize_ui_environment()
    if (.not.check) then
        print '(A)', "Could not initialize the UI environement.  Good bye."
        return
    end if

    ! Show the user a message box
    mbox = show_message_box("Testing...", "Example", MSGBX_BTN_OK_CANCEL, &
        MSGBX_ICON_INFORMATION)
    if (mbox%result == DIALOG_RESULT_OK) then
        print '(A)', "The user pressed OK."
    else if (mbox%result == DIALOG_RESULT_CANCEL) then
        print '(A)', "The user pressed Cancel."
    else
        print '(AI0A)', "The user pressed an unknown button with result: ", &
            mbox%result, "."
    end if

    ! Set up file filters
    filters(1)%name = "Text Files (*.txt)"
    filters(1)%pattern = "*.txt"

    filters(2)%name = "All Files (*.*)"
    filters(2)%pattern = "*.*"

    ! Open an file
    openfile = show_open_file_dialog(filters)
    if (openfile%result == DIALOG_RESULT_OK) then
        print '(A)', "The user selected file: " // openfile%string_list(1)%str
    end if

    ! Save a file
    savefile = show_save_file_dialog("txt", filters)
    if (savefile%result == DIALOG_RESULT_OK) then
        print '(A)', "The user wishes to save file: " // &
            savefile%string_list(1)%str
    end if

    ! Locate a directory
    folder = show_browse_folder_dialog()
    if (folder%result == DIALOG_RESULT_OK) then
        print '(A)', "Selected Folder: " // folder%string_list(1)%str
    end if

    ! Be sure to clean up
    call clean_up_ui_environment()
end program
