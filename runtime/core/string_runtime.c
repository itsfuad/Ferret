#include <limits.h>
#include <string.h>
#include "string_runtime.h"

int32_t ferret_string_len(const char* str) {
    if (!str) {
        return 0;
    }
    size_t len = strlen(str);
    if (len > INT32_MAX) {
        return INT32_MAX;
    }
    return (int32_t)len;
}

int32_t ferret_strcmp(const char* s1, const char* s2) {
    if (!s1 && !s2) {
        return 0;
    }
    if (!s1) {
        return -1;
    }
    if (!s2) {
        return 1;
    }
    return strcmp(s1, s2);
}
