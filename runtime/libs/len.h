// Ferret runtime: len function
// Public API for len builtin

#ifndef FERRET_LEN_H
#define FERRET_LEN_H

#include <stdint.h>

// len for arrays (dynamic arrays)
int32_t ferret_len_array(void* arr);

// len for strings
int32_t ferret_len_string(const char* str);

// len for maps
int32_t ferret_len_map(void* map);

#endif // FERRET_LEN_H

