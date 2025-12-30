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
