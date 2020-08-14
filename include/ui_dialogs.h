#ifndef UI_DIALOGS_H_
#define UI_DIALOGS_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

bool init_ui_env_c();
void clean_up_ui_env_c();
void show_open_file_dialog_c(char *nameBuffer, int bufferSize, int *numChars, 
    int nfilters, const char **filterName, const char **filterPattern);
void show_multi_file_open_file_dialog_c(char **nameBuffers, int numBuffers, 
    int bufferSize, int *numNames, int *numChars, int nfilters, 
    const char **filterName, const char **filterPattern);
void show_save_file_dialog_c(const char *defaultExt, int nfilters, 
    const char **filterName, const char **filterPattern, char *name,
    int bufferSize, int *numChars);
void show_browse_folder_dialog_c(char *buffer, int bufferSize, int *numChars);

#ifdef __cplusplus
}
#endif
#endif
