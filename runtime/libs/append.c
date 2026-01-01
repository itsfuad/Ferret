// Ferret runtime: append function implementation
// Public API that calls core functions

#include <stdint.h>
#include <stdbool.h>

#include "../core/array.h"

// Append element to array (dynamic arrays)
// Returns true on success, false on allocation failure
bool ferret_append_array(void* arr, const void* elem) {
    if (arr == NULL || elem == NULL) {
        return false;
    }
    ferret_array_t* array = (ferret_array_t*)arr;
    return ferret_array_append(array, elem);
}

