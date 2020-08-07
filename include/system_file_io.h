#ifndef SYSTEM_FILE_IO_H_
#define SYSTEM_FILE_IO_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

void split_file_path_c(const char *path, char *drive, char *dir, char *fname, 
    char *ext);

bool get_directory_contents_c(const char *dir, int nbuffers, int bufferSize, 
    char **fnames, int *nnames, int *nameLengths, char **dirnames, int *ndir, 
    int *dirLengths);

#ifdef __cplusplus
}
#endif
#endif // SYSTEM_FILE_IO_H_
