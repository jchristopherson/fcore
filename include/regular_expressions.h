#ifndef REGULAR_EXPRESSIONS_H_
#define REGULAR_EXPRESSIONS_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Looks for sequences that match the requested pattern.
 * 
 * @param src The string to search.
 * @param pattern The pattern to match.
 * @param numbuff The number of strings that can be stored in the buffer.
 * @param buffsize The capacity of each buffer string in the buffer.
 * @param buffer The buffer to which the matched strings will be written.
 * @param itemsizes An array of size numbuff that will be used to return the
 *  actual number of characters (not including the null character) to each 
 *  buffer string.
 * 
 * @return The number of matches found.  This number may be greater than the
 * capacity of the buffer.  If so, only the first numbuff values are written to
 * the buffer.
 */
int c_regex_match(const char *src, const char *pattern, int numbuff, 
    int buffsizes, char **buffer, int *itemsizes);

#ifdef __cplusplus
}
#endif
#endif
