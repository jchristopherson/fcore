#ifndef UI_DIALOGS_H_
#define UI_DIALOGS_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DIALOG_RESULT_CANCEL            0
#define DIALOG_RESULT_OK                1
#define DIALOG_RESULT_YES               2
#define DIALOG_RESULT_NO                3
#define DIALOG_RESULT_RETRY             4
#define DIALOG_RESULT_ABORT             5
#define DIALOG_RESULT_IGNORE            6
#define DIALOG_RESULT_CONTINUE          7
#define MSGBX_NO_ICON                   0
#define MSGBX_NO_BUTTON                 0
#define MSGBX_ICON_ERROR                1000
#define MSGBX_ICON_WARNING              1001
#define MSGBX_ICON_QUESTION             1002
#define MSGBX_ICON_INFORMATION          1003
#define MSGBX_BTN_OK                    1004
#define MSGBX_BTN_OK_CANCEL             1005
#define MSGBX_BTN_YES_NO                1006
#define MSGBX_BTN_YES_NO_CANCEL         1007
#define MSGBX_BTN_RETRY_CANCEL          1008
#define MSGBX_BTN_ABORT_RETRY_IGNORE    1009
#define MSGBX_BTN_CANCEL_RETRY_CONTINUE 1010

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
int show_message_box_c(void *parent, const char *txt, const char *title, 
    int buttons, int icon);

#ifdef __cplusplus
}
#endif
#endif
