// system_file_io.cpp

#include "system_file_io.h"
#include <cstdlib>
#include <cwchar>
#include <vector>

// Windows-Specific Code
#include <windows.h>
#include <windowsx.h>
#include <shobjidl.h>

bool get_dir_contents_win32(const char *dir, int nbuffers, int bufferSize, 
    char **fnames, int *nnames, int *nameLengths, char **dirnames, int *ndir, 
    int *dirLengths);
// End Windows-Specific Code

using namespace std;

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

/* ************************************************************************** */
/*                              SPLIT_FILE_PATH                               */
/* ************************************************************************** */
void split_file_path_c(const char *path, char *drive, char *dir, char *fname, 
    char *ext)
{
    _splitpath(path, drive, dir, fname, ext);
}

/* ************************************************************************** */
/*                           GET_DIRECTORY_CONTENTS                           */
/* ************************************************************************** */
bool get_directory_contents_c(const char *dir, int nbuffers, int bufferSize, 
    char **fnames, int *nnames, int *nameLengths, char **dirnames, int *ndir, 
    int *dirLengths)
{
    // Windows-Specific Code
    return get_dir_contents_win32(dir, nbuffers, bufferSize, fnames, nnames, 
        nameLengths, dirnames, ndir, dirLengths);
    // End Windows-Specific Code
}

// ---------------------------- Windows Version ----------------------------- //
bool get_dir_contents_win32(const char *dir, int nbuffers, int bufferSize, 
    char **fnames, int *nnames, int *nameLengths, char **dirnames, int *ndir, 
    int *dirLengths)
{
    // Local Variables
    const size_t SPATH_SIZE = 2048;
    WIN32_FIND_DATA fdFile;
    HANDLE hFind = NULL;
    wchar_t spath[SPATH_SIZE];
    size_t n;

    // Initialize the output variables
    *nnames = 0;
    *ndir = 0;

    // Get everything in the directory
    n = strlen(dir);
    if (dir[n-1] == 92) swprintf(spath, SPATH_SIZE, L"%s*.*", dir);
    else swprintf(spath, SPATH_SIZE, L"%s\\*.*", dir);
    if ((hFind = FindFirstFile(spath, &fdFile)) == INVALID_HANDLE_VALUE) {
        // ERROR: Path not found
        return false;
    }

    do {
        // FindFirstFile always returns '.' and '..'
        size_t fdFileSize = wcslen(fdFile.cFileName);
        if (wcsncmp(fdFile.cFileName, L".", fdFileSize) != 0 && 
            wcsncmp(fdFile.cFileName, L"..", fdFileSize) != 0) 
        {
            // Build up the file path using the passed-in info
            n = strlen(dir);
            if (dir[n-1] == 92) 
                swprintf(spath, SPATH_SIZE, L"%s%ls", dir, fdFile.cFileName);
            else 
                swprintf(spath, SPATH_SIZE, L"%s\\%ls", dir, fdFile.cFileName);

            // Is the entry a file, or a folder
            if (fdFile.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
                // We've got a directory
                if (*ndir >= nbuffers) continue;
                size_t lstr = MIN(wcslen(spath), bufferSize);
                dirLengths[*ndir] = (int)wcstombs(dirnames[*ndir], spath, lstr);
                *ndir += 1;
            }
            else {
                // We've got a file
                if (*nnames >= nbuffers) continue;
                size_t lstr = MIN(wcslen(spath), bufferSize);
                nameLengths[*nnames] = 
                    (int)wcstombs(fnames[*nnames], spath, lstr);
                *nnames += 1;
            }
        }
    } while (FindNextFile(hFind, &fdFile));

    // Clean up after ourselves
    FindClose(hFind);

    // End
    return true;
}
// End Windows Version

/* ************************************************************************** */
