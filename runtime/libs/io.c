// Ferret runtime: IO functions
// Native implementations for std/io module

#define _GNU_SOURCE  // For getline on POSIX systems
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#include "../core/array.h"

// For ssize_t on non-POSIX systems
#ifdef _WIN32
typedef long long ssize_t;
#endif
// Printable union layout: [4-byte tag][12 bytes padding/data] = 16 bytes total
// Tags: 0=i8, 1=i16, 2=i32, 3=i64, 4=u8, 5=u16, 6=u32, 7=u64, 8=f32, 9=f64, 10=str, 11=byte, 12=bool

static void print_union(const void* union_ptr) {
    int32_t tag = *(int32_t*)union_ptr;
    const uint8_t* data = (const uint8_t*)union_ptr + 4;
    
    switch (tag) {
        case 0: printf("%d", *(int8_t*)data); break;      // i8
        case 1: printf("%d", *(int16_t*)data); break;     // i16
        case 2: printf("%d", *(int32_t*)data); break;     // i32
        case 3: printf("%ld", *(int64_t*)data); break;    // i64
        case 4: printf("%u", *(uint8_t*)data); break;     // u8
        case 5: printf("%u", *(uint16_t*)data); break;    // u16
        case 6: printf("%u", *(uint32_t*)data); break;    // u32
        case 7: printf("%lu", *(uint64_t*)data); break;   // u64
        case 8: printf("%.6g", *(float*)data); break;     // f32
        case 9: printf("%.15g", *(double*)data); break;   // f64
        case 10: {  // str
            const char* str = *(const char**)data;
            printf("%s", str ? str : "(null)");
            break;
        }
        case 11: printf("%c", *(uint8_t*)data); break;    // byte
        case 12: printf("%s", *(bool*)data ? "true" : "false"); break; // bool
        default: printf("<invalid union tag %d>", tag); break;
    }
}

// Naming convention: std_io_Print -> ferret_std_io_Print
// Slice layout: { void* ptr; int32_t len; int32_t cap } = 16 bytes (with padding)
void ferret_std_io_Print(void* slice_ptr) {
    if (!slice_ptr) {
        return;
    }
    
    ferret_array_t* arr = (ferret_array_t*)slice_ptr;
    uint8_t* current = (uint8_t*)arr->data;
    
    for (int32_t i = 0; i < arr->length; i++) {
        if (i > 0) printf(" ");
        print_union(current);
        current += arr->elem_size;
    }
}

void ferret_std_io_Println(void* slice_ptr) {
    ferret_std_io_Print(slice_ptr);
    printf("\n");
}

// Result type layout for str!str: [union: str (8 bytes)][tag: i32 (4 bytes)] = 12 bytes (+ padding)
// Tag: 0 = Ok (value), 1 = Err (error)
// The union holds the str pointer (8 bytes on 64-bit)

// Read a line from stdin, returns str!str
void ferret_std_io_Read(void* out) {
    if (!out) return;
    
    char* line = NULL;
    size_t len = 0;
    ssize_t read = getline(&line, &len, stdin);
    
    // Result layout: [8-byte str pointer][4-byte tag]
    char** str_ptr = (char**)out;
    int32_t* tag_ptr = (int32_t*)((char*)out + 8);
    
    if (read == -1) {
        // Error case
        *str_ptr = "failed to read input";
        *tag_ptr = 1;  // Err
        if (line) free(line);
    } else {
        // Remove trailing newline if present
        if (read > 0 && line[read - 1] == '\n') {
            line[read - 1] = '\0';
        }
        *str_ptr = line;  // Caller owns this memory now
        *tag_ptr = 0;  // Ok
    }
}

// Read an integer from stdin, returns str!i32
void ferret_std_io_ReadInt(void* out) {
    if (!out) return;
    
    char* line = NULL;
    size_t len = 0;
    ssize_t read = getline(&line, &len, stdin);
    
    // Result layout for str!i32: [4-byte i32][4-byte padding][4-byte tag] or similar
    // Actually for i32, the union is max(sizeof(str), sizeof(i32)) = 8 bytes
    // So layout is: [8-byte union][4-byte tag]
    int32_t* val_ptr = (int32_t*)out;
    int32_t* tag_ptr = (int32_t*)((char*)out + 8);
    
    if (read == -1) {
        // Store error string at the union location (as pointer)
        *(char**)out = "failed to read input";
        *tag_ptr = 1;  // Err
    } else {
        char* endptr;
        long val = strtol(line, &endptr, 10);
        
        // Check for conversion errors
        if (endptr == line || (*endptr != '\0' && *endptr != '\n')) {
            *(char**)out = "invalid integer format";
            *tag_ptr = 1;  // Err
        } else if (val < INT32_MIN || val > INT32_MAX) {
            *(char**)out = "integer out of range";
            *tag_ptr = 1;  // Err
        } else {
            *val_ptr = (int32_t)val;
            *tag_ptr = 0;  // Ok
        }
    }
    
    if (line) free(line);
}

// Read a float from stdin, returns str!f64
void ferret_std_io_ReadFloat(void* out) {
    if (!out) return;
    
    char* line = NULL;
    size_t len = 0;
    ssize_t read = getline(&line, &len, stdin);
    
    // Result layout for str!f64: [8-byte f64/str][4-byte tag]
    double* val_ptr = (double*)out;
    int32_t* tag_ptr = (int32_t*)((char*)out + 8);
    
    if (read == -1) {
        *(char**)out = "failed to read input";
        *tag_ptr = 1;  // Err
    } else {
        char* endptr;
        double val = strtod(line, &endptr);
        
        // Check for conversion errors
        if (endptr == line || (*endptr != '\0' && *endptr != '\n')) {
            *(char**)out = "invalid float format";
            *tag_ptr = 1;  // Err
        } else {
            *val_ptr = val;
            *tag_ptr = 0;  // Ok
        }
    }
    
    if (line) free(line);
}

// Enum to string conversion helper
// Used by codegen to convert enum tags to variant names
const char* ferret_enum_to_string(const char* const* table, uint32_t count, int32_t tag) {
    if (tag < 0 || (uint32_t)tag >= count) {
        return "<invalid enum tag>";
    }
    return table[tag];
}
