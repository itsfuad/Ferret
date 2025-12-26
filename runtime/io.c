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
#include "string_builder.h"

static size_t ferret_align_to_size(size_t value, size_t alignment) {
    if (alignment <= 1) {
        return value;
    }
    size_t rem = value % alignment;
    if (rem == 0) {
        return value;
    }
    return value + (alignment - rem);
}

static size_t ferret_result_tag_offset(size_t ok_size, size_t ok_align, size_t err_size, size_t err_align) {
    size_t union_align = ok_align > err_align ? ok_align : err_align;
    size_t union_size = ferret_align_to_size(ok_size > err_size ? ok_size : err_size, union_align);
    return union_size;
}

static void ferret_ensure_float_decimal(char* buf, size_t size) {
    if (!buf || size == 0) {
        return;
    }

    bool has_dot = false;
    bool has_exp = false;
    for (size_t i = 0; buf[i] != '\0'; i++) {
        char c = buf[i];
        if (c == '.') {
            has_dot = true;
        } else if (c == 'e' || c == 'E') {
            has_exp = true;
        } else if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
            return;
        }
    }

    if (has_dot || has_exp) {
        return;
    }

    size_t len = strlen(buf);
    if (len + 2 >= size) {
        return;
    }
    buf[len] = '.';
    buf[len + 1] = '0';
    buf[len + 2] = '\0';
}

void ferret_format_f32(char* buf, size_t size, float value) {
    if (!buf || size == 0) {
        return;
    }
    snprintf(buf, size, "%.6g", value);
    ferret_ensure_float_decimal(buf, size);
}

void ferret_format_f64(char* buf, size_t size, double value) {
    if (!buf || size == 0) {
        return;
    }
    snprintf(buf, size, "%.15g", value);
    ferret_ensure_float_decimal(buf, size);
}

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
    char buf[64];
    ferret_format_f32(buf, sizeof(buf), value);
    printf("%s\n", buf);
}

void ferret_io_Println_f64(double value) {
    char buf[64];
    ferret_format_f64(buf, sizeof(buf), value);
    printf("%s\n", buf);
}

void ferret_io_Println_f128(ferret_f128 value) {
    ferret_io_Println_f128_ptr(&value);
}

void ferret_io_Println_f256(ferret_f256 value) {
    ferret_io_Println_f256_ptr(&value);
}

void ferret_io_Print_f32(float value) {
    char buf[64];
    ferret_format_f32(buf, sizeof(buf), value);
    printf("%s", buf);
}

void ferret_io_Print_f64(double value) {
    char buf[64];
    ferret_format_f64(buf, sizeof(buf), value);
    printf("%s", buf);
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

void ferret_io_ReadInt(void* out) {
    if (out == NULL) {
        return;
    }

    int32_t value;
    char* err = NULL;
    if (scanf("%d", &value) != 1) {
        err = strdup("Failed to read integer");
    }

    size_t tag_offset = ferret_result_tag_offset(sizeof(int32_t), sizeof(int32_t), sizeof(char*), sizeof(char*));
    if (err == NULL) {
        memcpy(out, &value, sizeof(value));
        ((uint8_t*)out)[tag_offset] = 1;
        return;
    }

    memcpy(out, &err, sizeof(err));
    ((uint8_t*)out)[tag_offset] = 0;
}

void ferret_io_ReadFloat(void* out) {
    if (out == NULL) {
        return;
    }

    double value;
    char* err = NULL;
    if (scanf("%lf", &value) != 1) {
        err = strdup("Failed to read float");
    }

    size_t tag_offset = ferret_result_tag_offset(sizeof(double), sizeof(double), sizeof(char*), sizeof(char*));
    if (err == NULL) {
        memcpy(out, &value, sizeof(value));
        ((uint8_t*)out)[tag_offset] = 1;
        return;
    }

    memcpy(out, &err, sizeof(err));
    ((uint8_t*)out)[tag_offset] = 0;
}

void ferret_io_Read(void* out) {
    if (out == NULL) {
        return;
    }

    char* err = NULL;
    char* result = NULL;
    ferret_string_builder_t* sb = ferret_string_builder_new(64);
    if (sb == NULL) {
        err = strdup("Failed to read string");
    } else {
        char buffer[256];
        bool read_any = false;
        while (err == NULL && fgets(buffer, sizeof(buffer), stdin) != NULL) {
            read_any = true;
            if (!ferret_string_builder_append(sb, buffer)) {
                err = strdup("Failed to read string");
                break;
            }
            if (strchr(buffer, '\n') != NULL) {
                break;
            }
        }

        if (err == NULL && !read_any) {
            err = strdup("Failed to read string");
        }

        if (err == NULL) {
            result = ferret_string_builder_string(sb);
            if (result == NULL) {
                err = strdup("Failed to read string");
            } else {
                size_t len = strlen(result);
                while (len > 0 && (result[len - 1] == '\n' || result[len - 1] == '\r')) {
                    result[len - 1] = '\0';
                    len--;
                }
            }
        }

        ferret_string_builder_destroy(sb);
    }

    size_t tag_offset = ferret_result_tag_offset(sizeof(char*), sizeof(char*), sizeof(char*), sizeof(char*));
    if (err == NULL) {
        memcpy(out, &result, sizeof(result));
        ((uint8_t*)out)[tag_offset] = 1;
        return;
    }

    memcpy(out, &err, sizeof(err));
    ((uint8_t*)out)[tag_offset] = 0;
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
