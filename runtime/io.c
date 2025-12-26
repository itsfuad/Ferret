// Ferret runtime: IO functions
// Native implementations for std/io module

#define _POSIX_C_SOURCE 200809L // For strdup

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include "bigint.h"
#include "io.h"

static void ferret_io_print_owned(char* msg, bool newline) {
    if (msg != NULL) {
        if (newline) {
            printf("%s\n", msg);
        } else {
            printf("%s", msg);
        }
        free(msg);
        return;
    }
    if (newline) {
        printf("\n");
    }
}

// String versions
void ferret_io_Println(const char* msg) {
    if (msg != NULL) {
        printf("%s\n", msg);
    } else {
        printf("\n");
    }
}

void ferret_io_Print(const char* msg) {
    if (msg != NULL) {
        printf("%s", msg);
    }
}

static const char* ferret_enum_invalid = "<invalid enum>";

const char* ferret_enum_to_string(const char* const* table, uint32_t count, int32_t tag) {
    if (table == NULL || count == 0) {
        return ferret_enum_invalid;
    }
    if (tag < 0 || (uint32_t)tag >= count) {
        return ferret_enum_invalid;
    }
    return table[tag];
}

// Integer versions
void ferret_io_Println_i8(int8_t value) {
    printf("%d\n", (int)value);
}

void ferret_io_Println_i16(int16_t value) {
    printf("%d\n", (int)value);
}

void ferret_io_Println_i32(int32_t value) {
    printf("%d\n", value);
}

void ferret_io_Println_i64(int64_t value) {
    printf("%ld\n", (long)value);
}

void ferret_io_Print_i8(int8_t value) {
    printf("%d", (int)value);
}

void ferret_io_Print_i16(int16_t value) {
    printf("%d", (int)value);
}

void ferret_io_Print_i32(int32_t value) {
    printf("%d", value);
}

void ferret_io_Print_i64(int64_t value) {
    printf("%ld", (long)value);
}

// Unsigned integer versions
void ferret_io_Println_u8(uint8_t value) {
    printf("%u\n", (unsigned int)value);
}

void ferret_io_Println_u16(uint16_t value) {
    printf("%u\n", (unsigned int)value);
}

void ferret_io_Println_u32(uint32_t value) {
    printf("%u\n", value);
}

void ferret_io_Println_u64(uint64_t value) {
    printf("%lu\n", (unsigned long)value);
}

void ferret_io_Println_u128(ferret_u128 value) {
    ferret_io_Println_u128_ptr(&value);
}

void ferret_io_Println_u256(ferret_u256 value) {
    ferret_io_Println_u256_ptr(&value);
}

void ferret_io_Println_byte(uint8_t value) {
    printf("%c\n", (unsigned char)value);
}

void ferret_io_Print_u8(uint8_t value) {
    printf("%u", (unsigned int)value);
}

void ferret_io_Print_u16(uint16_t value) {
    printf("%u", (unsigned int)value);
}

void ferret_io_Print_u32(uint32_t value) {
    printf("%u", value);
}

void ferret_io_Print_u64(uint64_t value) {
    printf("%lu", (unsigned long)value);
}

void ferret_io_Print_u128(ferret_u128 value) {
    ferret_io_Print_u128_ptr(&value);
}

void ferret_io_Print_u256(ferret_u256 value) {
    ferret_io_Print_u256_ptr(&value);
}

void ferret_io_Print_byte(uint8_t value) {
    printf("%c", (unsigned char)value);
}

void ferret_io_Println_i128(ferret_i128 value) {
    ferret_io_Println_i128_ptr(&value);
}

void ferret_io_Println_i256(ferret_i256 value) {
    ferret_io_Println_i256_ptr(&value);
}

void ferret_io_Print_i128(ferret_i128 value) {
    ferret_io_Print_i128_ptr(&value);
}

void ferret_io_Print_i256(ferret_i256 value) {
    ferret_io_Print_i256_ptr(&value);
}

// Float versions
void ferret_io_Println_f32(float value) {
    printf("%.6g\n", value);
}

void ferret_io_Println_f64(double value) {
    printf("%.15g\n", value);
}

void ferret_io_Println_f128(ferret_f128 value) {
    ferret_io_Println_f128_ptr(&value);
}

void ferret_io_Println_f256(ferret_f256 value) {
    ferret_io_Println_f256_ptr(&value);
}

void ferret_io_Print_f32(float value) {
    printf("%.6g", value);
}

void ferret_io_Print_f64(double value) {
    printf("%.15g", value);
}

void ferret_io_Print_f128(ferret_f128 value) {
    ferret_io_Print_f128_ptr(&value);
}

void ferret_io_Print_f256(ferret_f256 value) {
    ferret_io_Print_f256_ptr(&value);
}

// Bool version
void ferret_io_Println_bool(int value) {
    printf("%s\n", value ? "true" : "false");
}

void ferret_io_Print_bool(int value) {
    printf("%s", value ? "true" : "false");
}

void ferret_io_Println_i128_ptr(const ferret_i128* value) {
    char* msg = value ? ferret_i128_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, true);
}

void ferret_io_Println_u128_ptr(const ferret_u128* value) {
    char* msg = value ? ferret_u128_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, true);
}

void ferret_io_Println_i256_ptr(const ferret_i256* value) {
    char* msg = value ? ferret_i256_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, true);
}

void ferret_io_Println_u256_ptr(const ferret_u256* value) {
    char* msg = value ? ferret_u256_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, true);
}

void ferret_io_Println_f128_ptr(const ferret_f128* value) {
    char* msg = value ? ferret_f128_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, true);
}

void ferret_io_Println_f256_ptr(const ferret_f256* value) {
    char* msg = value ? ferret_f256_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, true);
}

void ferret_io_Print_i128_ptr(const ferret_i128* value) {
    char* msg = value ? ferret_i128_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, false);
}

void ferret_io_Print_u128_ptr(const ferret_u128* value) {
    char* msg = value ? ferret_u128_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, false);
}

void ferret_io_Print_i256_ptr(const ferret_i256* value) {
    char* msg = value ? ferret_i256_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, false);
}

void ferret_io_Print_u256_ptr(const ferret_u256* value) {
    char* msg = value ? ferret_u256_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, false);
}

void ferret_io_Print_f128_ptr(const ferret_f128* value) {
    char* msg = value ? ferret_f128_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, false);
}

void ferret_io_Print_f256_ptr(const ferret_f256* value) {
    char* msg = value ? ferret_f256_to_string_ptr(value) : NULL;
    ferret_io_print_owned(msg, false);
}

int32_t ferret_io_ReadInt(char** error) {
    int32_t value;
    if (scanf("%d", &value) == 1) {
        if (error != NULL) {
            *error = NULL;
        }
        return value;
    } else {
        if (error != NULL) {
            *error = strdup("Failed to read integer");
        }
        return 0;
    }
}

double ferret_io_ReadFloat(char** error) {
    double value;
    if (scanf("%lf", &value) == 1) {
        if (error != NULL) {
            *error = NULL;
        }
        return value;
    } else {
        if (error != NULL) {
            *error = strdup("Failed to read float");
        }
        return 0.0;
    }
}

// String concatenation helper
char* ferret_io_ConcatStrings(const char* s1, const char* s2) {
    if (s1 == NULL) s1 = "";
    if (s2 == NULL) s2 = "";
    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    char* result = (char*)malloc(len1 + len2 + 1);
    if (result == NULL) {
        return NULL;
    }
    memcpy(result, s1, len1);
    memcpy(result + len1, s2, len2);
    result[len1 + len2] = '\0';
    return result;
}
