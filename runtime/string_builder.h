// Ferret runtime: String Builder Library
// Efficient string concatenation and manipulation (avoids repeated malloc)

#ifndef FERRET_STRING_H
#define FERRET_STRING_H

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

// String builder structure (similar to Go's strings.Builder)
typedef struct {
    char* data;        // String data buffer
    int32_t length;    // Current string length
    int32_t capacity;  // Buffer capacity
} ferret_string_builder_t;

// Create a new string builder with initial capacity
ferret_string_builder_t* ferret_string_builder_new(int32_t initial_capacity);

// Append a string to the builder (amortized O(1))
// Returns false on allocation failure
bool ferret_string_builder_append(ferret_string_builder_t* sb, const char* str);

// Append a single character
bool ferret_string_builder_append_char(ferret_string_builder_t* sb, char c);

// Get the built string (caller must free the result)
// Returns NULL on failure
char* ferret_string_builder_string(ferret_string_builder_t* sb);

// Get the built string without copying (builder retains ownership)
// Returns NULL if builder is empty
const char* ferret_string_builder_cstring(const ferret_string_builder_t* sb);

// Get current length
int32_t ferret_string_builder_len(const ferret_string_builder_t* sb);

// Reset builder (keeps capacity)
void ferret_string_builder_reset(ferret_string_builder_t* sb);

// Free builder memory (doesn't free the struct itself if stack-allocated)
void ferret_string_builder_free(ferret_string_builder_t* sb);

// Free builder and the struct itself
void ferret_string_builder_destroy(ferret_string_builder_t* sb);

// Efficient string concatenation (better than ferret_io_ConcatStrings for multiple strings)
// Takes variable number of strings and concatenates them
// Returns newly allocated string (caller must free)
char* ferret_string_concat_many(const char* str1, ...);

#endif // FERRET_STRING_BUILDER_H
