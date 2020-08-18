// ui_dialogs.cpp

#include "ui_dialogs.h"
#include <cstdlib>
#include <cwchar>
#include <vector>

// Windows-Specific Code
#include <windows.h>
#include <windowsx.h>
#include <shobjidl.h>
// End Windows-Specific Code

using namespace std;

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

// More Windows Stuff
bool init_com_sta() {
    HRESULT hr = CoInitializeEx(NULL, 
        COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE);
    return SUCCEEDED(hr);
}

// REF: 
// https://docs.microsoft.com/en-us/windows/win32/learnwin32/example--the-open-dialog-box
// https://docs.microsoft.com/en-us/previous-versions/windows/desktop/legacy/bb776913(v%3Dvs.85)
// https://docs.microsoft.com/en-us/previous-versions/windows/desktop/legacy/dd940349%28v%3dvs.85%29
// http://codexpert.ro/blog/2013/05/21/file-open-dialog-with-multiple-selection-part-3-cutting-the-dog-tail/
// https://docs.microsoft.com/en-us/previous-versions/windows/desktop/legacy/bb776913(v=vs.85)
void ShowUserOpenFileDialog(IFileOpenDialog *pFileOpen, char *nameBuffer, 
    int bufferSize, int *numChars)
{
    // Show the dialog
    HRESULT hr = pFileOpen->Show(NULL);

    // Get the filename from the dialog
    if (SUCCEEDED(hr)) {
        IShellItem *pItem;
        hr = pFileOpen->GetResult(&pItem);
        if (SUCCEEDED(hr)) {
            PWSTR pszFilePath;
            hr = pItem->GetDisplayName(SIGDN_FILESYSPATH, &pszFilePath);
            
            // Copy the filename to the output buffer
            size_t lstr = wcslen(pszFilePath);
            *numChars = (int)wcstombs(nameBuffer, pszFilePath, MIN(bufferSize, lstr));

            // Clean Up
            CoTaskMemFree(pszFilePath);
        }
        pItem->Release();
    }
}



void ShowUserMultiFileOpenDialog(IFileOpenDialog *pFileOpen, char **nameBuffers, 
    int numBuffers, int *numNames, int bufferSize, int *numChars)
{
    // Show the dialog
    HRESULT hr = pFileOpen->Show(NULL);

    // Get the filenames from the dialog
    if (SUCCEEDED(hr)) {
        // See https://docs.microsoft.com/en-us/previous-versions/windows/desktop/legacy/bb776913(v=vs.85)
        IShellItemArray *rsts;
        hr = pFileOpen->GetResults(&rsts);
        if (SUCCEEDED(hr)) {
            // Retrieve the results
            DWORD nItems;
            rsts->GetCount(&nItems);
            *numNames = MIN((int)nItems, numBuffers);
            for (int i = 0; i < *numNames; ++i) {
                IShellItem *pItem;
                hr = rsts->GetItemAt((DWORD)i, &pItem);
                if (SUCCEEDED(hr)) {
                    PWSTR pszFilePath;
                    hr = pItem->GetDisplayName(SIGDN_FILESYSPATH, &pszFilePath);

                    size_t lstr = wcslen(pszFilePath);
                    numChars[i] = (int)wcstombs(nameBuffers[i], pszFilePath, MIN(bufferSize, lstr));

                    CoTaskMemFree(pszFilePath);
                }
                pItem->Release();
            }

            // Clean Up
            rsts->Release();
        }
    }
}



vector<COMDLG_FILTERSPEC> ToFilterSpec(int nfilters, const char **filterName, 
    const char **filterPattern)
{
    const int BUFFER_SIZE = 2048;
    vector<COMDLG_FILTERSPEC> list(nfilters);
    wchar_t *nameBuffer, *patternBuffer;

    for (int i = 0; i < nfilters; ++i) {
        nameBuffer = new wchar_t[BUFFER_SIZE];
        size_t nchars = mbstowcs(nameBuffer, filterName[i], BUFFER_SIZE - 1);

        patternBuffer = new wchar_t[BUFFER_SIZE];
        nchars = mbstowcs(patternBuffer, filterPattern[i], BUFFER_SIZE - 1);

        COMDLG_FILTERSPEC spec = {nameBuffer, patternBuffer};
        list[i] = spec;
    }
    
    return list;
}

void CleanUpFilterSpecList(vector<COMDLG_FILTERSPEC> &list) {
    for (size_t i = 0; i < list.size(); ++i) {
        auto item = list[i];
        delete [] item.pszName;
        delete [] item.pszSpec;
    }
}



void ShowUserSaveDialog(IFileSaveDialog *dlg, char *name, int bufferSize, int *numChars) {
    // Show the dialog
    HRESULT hr = dlg->Show(NULL);

    // Get the filename
    if (SUCCEEDED(hr)) {
        IShellItem *pItem;
        hr = dlg->GetResult(&pItem);
        if (SUCCEEDED(hr)) {
            PWSTR filePath;
            hr = pItem->GetDisplayName(SIGDN_FILESYSPATH, &filePath);

            // Copy the filename to the output buffer
            size_t lstr = wcslen(filePath);
            *numChars = (int)wcstombs(name, filePath, MIN(bufferSize, lstr));

            // Clean Up
            CoTaskMemFree(filePath);
        }
        pItem->Release();
    }
}

// End of Windows Stuff


bool init_ui_env_c() 
{
    // Windows-Specific Call
    return init_com_sta();
    // End Windows-Specific Call
}



void clean_up_ui_env_c() 
{
    // Windows-Specific Call
    CoUninitialize();
    // End Windows-Specific Call
}



void show_open_file_dialog_c(char *nameBuffer, int bufferSize, int *numChars, 
    int nfilters, const char **filterName, const char **filterPattern)
{
    // Initialize outputs
    *numChars = 0;

    // Windows-Specific Code
    
    // Create the FileOpenDialog object
    IFileOpenDialog *pFileOpen;
    HRESULT hr = CoCreateInstance(CLSID_FileOpenDialog, NULL, 
        CLSCTX_ALL, IID_IFileOpenDialog, 
        reinterpret_cast<void**>(&pFileOpen));
    if (SUCCEEDED(hr)) {
        // Set the filter options
        if (nfilters > 0) {
            auto filters = ToFilterSpec(nfilters, filterName, filterPattern);
            hr = pFileOpen->SetFileTypes(filters.size(), filters.data());
            if (SUCCEEDED(hr)) {
                // Show the dialog
                ShowUserOpenFileDialog(pFileOpen, nameBuffer, bufferSize, 
                    numChars);
            }
            CleanUpFilterSpecList(filters);
        }
        else {
            // No filters - just show the dialog
            ShowUserOpenFileDialog(pFileOpen, nameBuffer, bufferSize, numChars);
        }
        pFileOpen->Release();
    }
    // End Window-Specific Code
}



void show_multi_file_open_file_dialog_c(char **nameBuffers, int numBuffers, 
    int bufferSize, int *numNames, int *numChars, int nfilters, 
    const char **filterName, const char **filterPattern)
{
    // Initialize outputs
    *numNames = 0;

    // Windows-Specific Code

    // Create the FileOpenDialog object
    IFileOpenDialog *pFileOpen;
    HRESULT hr = CoCreateInstance(CLSID_FileOpenDialog, NULL, CLSCTX_ALL, 
        IID_IFileOpenDialog, reinterpret_cast<void**>(&pFileOpen));
    if (SUCCEEDED(hr)) {
        DWORD dwOptions;
        hr = pFileOpen->GetOptions(&dwOptions);
        if (SUCCEEDED(hr)) {
            hr = pFileOpen->SetOptions(dwOptions | FOS_ALLOWMULTISELECT);
        }

        // Set the filter options
        if (nfilters > 0) {
            auto filters = ToFilterSpec(nfilters, filterName, filterPattern);
            hr = pFileOpen->SetFileTypes(filters.size(), filters.data());
            if (SUCCEEDED(hr)) {
                ShowUserMultiFileOpenDialog(pFileOpen, nameBuffers, numBuffers, 
                    numNames, bufferSize, numChars);
            }
            CleanUpFilterSpecList(filters);
        }
        else {
            // No filters - just show the dialog
            ShowUserMultiFileOpenDialog(pFileOpen, nameBuffers, numBuffers, 
                numNames, bufferSize, numChars);
        }
        pFileOpen->Release();
    }
    // End Window-Specific Code
}



void show_save_file_dialog_c(const char *defaultExt, int nfilters, 
    const char **filterName, const char **filterPattern, char *name,
    int bufferSize, int *numChars)
{
    // Initialization
    *numChars = 0;

    // Windows-Specific Code

    // Create the dialog
    IFileSaveDialog *dlg;
    HRESULT hr = CoCreateInstance(CLSID_FileSaveDialog, NULL, CLSCTX_ALL, 
        IID_IFileSaveDialog, reinterpret_cast<void**>(&dlg));

    // Set the default file extension
    size_t n = strlen(defaultExt) + 1;
    wchar_t *ext = new wchar_t[n + 1];
    mbtowc(ext, defaultExt, n);
    dlg->SetDefaultExtension(ext);
    delete [] ext;

    // Show the dialog
    if (SUCCEEDED(hr)) {
        // Set the filter options
        if (nfilters > 0) {
            // Set the filters, and then show the dialog
            auto filters = ToFilterSpec(nfilters, filterName, filterPattern);
            hr = dlg->SetFileTypes(filters.size(), filters.data());
            if (SUCCEEDED(hr)) {
                ShowUserSaveDialog(dlg, name, bufferSize, numChars);
            }
        }
        else {
            // No filters, just show the dialog
            ShowUserSaveDialog(dlg, name, bufferSize, numChars);
        }
        dlg->Release();
    }
    // End Window-Specific Code
}



void show_browse_folder_dialog_c(char *buffer, int bufferSize, int *numChars)
{
    // Initialize outputs
    *numChars = 0;

    // Windows-Specific Code

    // Create the file dialog
    IFileDialog *dlg;
    HRESULT hr = CoCreateInstance(CLSID_FileOpenDialog, NULL, 
        CLSCTX_ALL, IID_IFileOpenDialog, reinterpret_cast<void**>(&dlg));
    if (SUCCEEDED(hr)) {
        // Establish options
        DWORD opt;
        hr = dlg->GetOptions(&opt);
        if (SUCCEEDED(hr)) {
            hr = dlg->SetOptions(opt | FOS_PICKFOLDERS);
            if (SUCCEEDED(hr)) {
                hr = dlg->Show(NULL);
                if (SUCCEEDED(hr)) {
                    IShellItem *pItem;
                    hr = dlg->GetResult(&pItem);
                    if (SUCCEEDED(hr)) {
                        PWSTR path;
                        hr = pItem->GetDisplayName(SIGDN_FILESYSPATH, &path);

                        size_t lstr = wcslen(path);
                        *numChars = (int)wcstombs(buffer, path, MIN(bufferSize, lstr));

                        CoTaskMemFree(path);
                    }
                    pItem->Release();
                }
            }
        }
        dlg->Release();
    }
    // End Window-Specific Code
}


int show_message_box_c(void *parent, const char *txt, const char *title, 
    int buttons, int icon)
{
    // Windows-Specific Code
    HWND hWnd = (HWND)parent;
    UINT btn, icn, type;
    int result;
    size_t nchars;
    wchar_t txtBuffer[4096], titleBuffer[4096];

    switch (buttons) {
        case MSGBX_BTN_OK:
            btn = MB_OK;
            break;

        case MSGBX_BTN_OK_CANCEL:
            btn = MB_OKCANCEL;
            break;

        case MSGBX_BTN_YES_NO:
            btn = MB_YESNO;
            break;

        case MSGBX_BTN_YES_NO_CANCEL:
            btn = MB_YESNOCANCEL;
            break;

        case MSGBX_BTN_RETRY_CANCEL:
            btn = MB_RETRYCANCEL;
            break;

        case MSGBX_BTN_ABORT_RETRY_IGNORE:
            btn = MB_ABORTRETRYIGNORE;
            break;

        case MSGBX_BTN_CANCEL_RETRY_CONTINUE:
            btn = MB_CANCELTRYCONTINUE;
            break;

        default:
            btn = MSGBX_NO_BUTTON;
            break;
    }

    switch (icon) {
        case MSGBX_ICON_ERROR:
            icn = MB_ICONERROR;
            break;

        case MSGBX_ICON_WARNING:
            icn = MB_ICONWARNING;
            break;

        case MSGBX_ICON_QUESTION:
            icn = MB_ICONQUESTION;
            break;

        case MSGBX_ICON_INFORMATION:
            icn = MB_ICONINFORMATION;
            break;

        default:
            icn = MSGBX_NO_ICON;
            break;
    }

    if (btn == MSGBX_NO_BUTTON && icn == MSGBX_NO_ICON) {
        type = 0;
    }
    else if (btn == MSGBX_NO_BUTTON && icn != MSGBX_NO_ICON) {
        type = icn;
    }
    else if (btn != MSGBX_NO_BUTTON && icn == MSGBX_NO_ICON) {
        type = btn;
    }
    else {
        type = btn | icn;
    }

    nchars = mbstowcs(txtBuffer, txt, MIN(4096, strlen(txt) + 1));
    nchars = mbstowcs(titleBuffer, title, MIN(4096, strlen(title) + 1));
    result = MessageBox(hWnd, txtBuffer, titleBuffer, type);

    switch (result) {
        case IDABORT:
            result = DIALOG_RESULT_ABORT;
            break;

        case IDCANCEL:
            result = DIALOG_RESULT_CANCEL;
            break;

        case IDCONTINUE:
            result = DIALOG_RESULT_CONTINUE;
            break;

        case IDIGNORE:
            result = DIALOG_RESULT_IGNORE;
            break;

        case IDNO:
            result = DIALOG_RESULT_NO;
            break;

        case IDOK:
            result = DIALOG_RESULT_OK;
            break;

        case IDRETRY:
            result = DIALOG_RESULT_RETRY;
            break;

        case IDYES:
            result = DIALOG_RESULT_YES;
            break;
    }

    // End Window-Specific Code

    // End
    return result;
}
