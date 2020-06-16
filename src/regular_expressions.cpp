// regular_expressions.cpp

#include "regular_expressions.h"
#include <regex>
#include <string>
#include <cstring>

using namespace std;

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

// References:
// - http://www.cplusplus.com/reference/regex/regex_match/


int c_regex_match(const char *src, const char *pattern, int numbuff, 
    int buffsizes, char **buffer, int *itemsizes)
{
    // Perform the match process
    cmatch cm;
    regex ex(pattern);
    regex_match(src, cm, ex);

    // Process each match
    int count = (int)cm.size();
    for (int i = 0; i < MIN(count, numbuff); ++i) 
    {
        string rst = cm[i];
        itemsizes[i] = (int)rst.size();
        strncpy(
            buffer[i], 
            rst.c_str(), 
            MIN((size_t)buffsizes, rst.size()) );
    }

    // End
    return count;
}





int c_regex_search(const char *src, const char *pattern, int numbuff,
    int buffsizes, char **buffer, int *itemsizes)
{
    // Perform the search process
    cmatch cm;
    regex ex(pattern);
    regex_search(src, cm, ex);

    // Process each match
    int count = (int)cm.size();
    for (int i = 0; i < MIN(count, numbuff); ++i) 
    {
        string rst = cm[i];
        itemsizes[i] = (int)rst.size();
        strncpy(
            buffer[i], 
            rst.c_str(), 
            MIN((size_t)buffsizes, rst.size()) );
    }

    // End
    return count;
}





void c_regex_replace(const char *src, const char *pattern, const char *rplc, 
    int buffsize, char *buffer, int *nbuff)
{
    // Replace all instances
    regex ex(pattern);
    auto rst = regex_replace(src, ex, rplc);

    // Copy the new string to the output
    *nbuff = MIN(buffsize, (int)rst.size());
    strncpy(buffer, rst.c_str(), (size_t)(*nbuff));
}
