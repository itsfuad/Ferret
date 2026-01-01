// Ferret runtime: append function
// Public API for append builtin

#ifndef FERRET_APPEND_H
#define FERRET_APPEND_H

#include <stdint.h>
#include <stdbool.h>

// append for arrays (dynamic arrays)
// Returns true on success, false on allocation failure
bool ferret_append_array(void* arr, const void* elem);

#endif // FERRET_APPEND_H

