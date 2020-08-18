# Strings Example
add_executable(strings_example strings_example.f90)
target_link_libraries(strings_example fcore)

# Text I/O Example
add_executable(text_io_example text_io_example.f90)
target_link_libraries(text_io_example fcore)

# Folder Operations Example
add_executable(folder_ops_example folder_ops_example.f90)
target_link_libraries(folder_ops_example fcore)

# UI Dialog Example
add_executable(ui_dialog_example ui_dialog_example.f90)
target_link_libraries(ui_dialog_example fcore)
