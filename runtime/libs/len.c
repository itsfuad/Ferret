// Ferret runtime: len function implementation
// Public API that calls core functions

#include "len.h"

#include "../core/array.h"
#include "../core/string_runtime.h"
#include "../core/map.h"

int32_t ferret_len_array(void* arr) {
    if (arr == NULL) {
        return 0;
    }
    ferret_array_t* array = (ferret_array_t*)arr;
    return ferret_array_len(array);
}

int32_t ferret_len_string(const char* str) {
    if (str == NULL) {
        return 0;
    }
    return ferret_string_len(str);
}

int32_t ferret_len_map(void* map) {
    if (map == NULL) {
        return 0;
    }
    ferret_map_t* m = (ferret_map_t*)map;
    return (int32_t)ferret_map_size(m);
}

