#ifndef FERRET_BIGINT_H
#define FERRET_BIGINT_H

#include <stdint.h>
#include <stdbool.h>
#include <math.h>

// Feature detection for 128-bit integer support
#if defined(__SIZEOF_INT128__) || defined(__GNUC__) || defined(__clang__)
    #ifndef FERRET_HAS_INT128
        #define FERRET_HAS_INT128 1
    #endif
#endif

// Feature detection for 128-bit floating point support
#if defined(__GNUC__) && (defined(__x86_64__) || defined(__i386__) || defined(__powerpc__) || defined(__powerpc64__))
    #ifndef FERRET_HAS_FLOAT128
        #define FERRET_HAS_FLOAT128 1
    #endif
#endif

// 128-bit integer types
#ifdef FERRET_HAS_INT128
    typedef __int128 ferret_i128;
    typedef unsigned __int128 ferret_u128;
#else
    // Fallback: struct-based 128-bit integer (two 64-bit words)
    typedef struct {
        uint64_t lo;  // Low 64 bits
        int64_t hi;   // High 64 bits (signed for i128)
    } ferret_i128;
    
    typedef struct {
        uint64_t lo;  // Low 64 bits
        uint64_t hi;  // High 64 bits (unsigned for u128)
    } ferret_u128;
#endif

// 256-bit integer types (always struct-based)
typedef struct {
    uint64_t words[4];  // Little-endian: words[0] is least significant
} ferret_u256;

typedef struct {
    uint64_t words[4];  // Little-endian: words[0] is least significant
    // Sign is stored in the most significant bit of words[3]
} ferret_i256;

// 128-bit integer operations
#ifdef FERRET_HAS_INT128
    // Native operations - compiler handles these
    #define ferret_i128_add(a, b) ((a) + (b))
    #define ferret_i128_sub(a, b) ((a) - (b))
    #define ferret_i128_mul(a, b) ((a) * (b))
    #define ferret_i128_div(a, b) ((a) / (b))
    #define ferret_i128_mod(a, b) ((a) % (b))
    #define ferret_i128_eq(a, b) ((a) == (b))
    #define ferret_i128_lt(a, b) ((a) < (b))
    #define ferret_i128_gt(a, b) ((a) > (b))
    
    #define ferret_u128_add(a, b) ((a) + (b))
    #define ferret_u128_sub(a, b) ((a) - (b))
    #define ferret_u128_mul(a, b) ((a) * (b))
    #define ferret_u128_div(a, b) ((a) / (b))
    #define ferret_u128_mod(a, b) ((a) % (b))
    #define ferret_u128_eq(a, b) ((a) == (b))
    #define ferret_u128_lt(a, b) ((a) < (b))
    #define ferret_u128_gt(a, b) ((a) > (b))
#else
    // Struct-based operations - implemented in bigint.c
    ferret_i128 ferret_i128_add(ferret_i128 a, ferret_i128 b);
    ferret_i128 ferret_i128_sub(ferret_i128 a, ferret_i128 b);
    ferret_i128 ferret_i128_mul(ferret_i128 a, ferret_i128 b);
    ferret_i128 ferret_i128_div(ferret_i128 a, ferret_i128 b);
    ferret_i128 ferret_i128_mod(ferret_i128 a, ferret_i128 b);
    bool ferret_i128_eq(ferret_i128 a, ferret_i128 b);
    bool ferret_i128_lt(ferret_i128 a, ferret_i128 b);
    bool ferret_i128_gt(ferret_i128 a, ferret_i128 b);
    
    ferret_u128 ferret_u128_add(ferret_u128 a, ferret_u128 b);
    ferret_u128 ferret_u128_sub(ferret_u128 a, ferret_u128 b);
    ferret_u128 ferret_u128_mul(ferret_u128 a, ferret_u128 b);
    ferret_u128 ferret_u128_div(ferret_u128 a, ferret_u128 b);
    ferret_u128 ferret_u128_mod(ferret_u128 a, ferret_u128 b);
    bool ferret_u128_eq(ferret_u128 a, ferret_u128 b);
    bool ferret_u128_lt(ferret_u128 a, ferret_u128 b);
    bool ferret_u128_gt(ferret_u128 a, ferret_u128 b);
#endif

// 256-bit integer operations (always implemented in bigint.c)
ferret_i256 ferret_i256_add(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_sub(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_mul(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_div(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_mod(ferret_i256 a, ferret_i256 b);
bool ferret_i256_eq(ferret_i256 a, ferret_i256 b);
bool ferret_i256_lt(ferret_i256 a, ferret_i256 b);
bool ferret_i256_gt(ferret_i256 a, ferret_i256 b);

ferret_u256 ferret_u256_add(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_sub(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_mul(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_div(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_mod(ferret_u256 a, ferret_u256 b);
bool ferret_u256_eq(ferret_u256 a, ferret_u256 b);
bool ferret_u256_lt(ferret_u256 a, ferret_u256 b);
bool ferret_u256_gt(ferret_u256 a, ferret_u256 b);

// Conversion functions
ferret_i128 ferret_i128_from_i64(int64_t val);
ferret_u128 ferret_u128_from_u64(uint64_t val);
ferret_i256 ferret_i256_from_i64(int64_t val);
ferret_u256 ferret_u256_from_u64(uint64_t val);

int64_t ferret_i128_to_i64(ferret_i128 val);
uint64_t ferret_u128_to_u64(ferret_u128 val);
int64_t ferret_i256_to_i64(ferret_i256 val);
uint64_t ferret_u256_to_u64(ferret_u256 val);

// 128-bit floating point types
#ifdef FERRET_HAS_FLOAT128
    typedef __float128 ferret_f128;
#else
    // Fallback: struct-based 128-bit float (IEEE 754 binary128 format)
    // 1 sign bit + 15 exponent bits + 112 mantissa bits = 128 bits
    typedef struct {
        uint64_t mantissa_lo;  // Lower 64 bits of mantissa
        uint64_t mantissa_hi;  // Upper 48 bits of mantissa + 15 exponent bits + 1 sign bit
    } ferret_f128;
#endif

// 256-bit floating point type (always struct-based)
// IEEE 754 extended format: 1 sign + 19 exponent + 236 mantissa = 256 bits
typedef struct {
    uint64_t mantissa[3];  // 192 bits of mantissa (3 × 64)
    uint64_t exp_sign;      // 19 exponent bits + 1 sign bit + remaining mantissa bits
} ferret_f256;

// 128-bit floating point operations
#ifdef FERRET_HAS_FLOAT128
    // Native operations - compiler handles these
    #define ferret_f128_add(a, b) ((a) + (b))
    #define ferret_f128_sub(a, b) ((a) - (b))
    #define ferret_f128_mul(a, b) ((a) * (b))
    #define ferret_f128_div(a, b) ((a) / (b))
    #define ferret_f128_eq(a, b) ((a) == (b))
    #define ferret_f128_lt(a, b) ((a) < (b))
    #define ferret_f128_gt(a, b) ((a) > (b))
#else
    // Struct-based operations - implemented in bigint.c
    ferret_f128 ferret_f128_add(ferret_f128 a, ferret_f128 b);
    ferret_f128 ferret_f128_sub(ferret_f128 a, ferret_f128 b);
    ferret_f128 ferret_f128_mul(ferret_f128 a, ferret_f128 b);
    ferret_f128 ferret_f128_div(ferret_f128 a, ferret_f128 b);
    bool ferret_f128_eq(ferret_f128 a, ferret_f128 b);
    bool ferret_f128_lt(ferret_f128 a, ferret_f128 b);
    bool ferret_f128_gt(ferret_f128 a, ferret_f128 b);
#endif

// 256-bit floating point operations (always implemented in bigint.c)
ferret_f256 ferret_f256_add(ferret_f256 a, ferret_f256 b);
ferret_f256 ferret_f256_sub(ferret_f256 a, ferret_f256 b);
ferret_f256 ferret_f256_mul(ferret_f256 a, ferret_f256 b);
ferret_f256 ferret_f256_div(ferret_f256 a, ferret_f256 b);
bool ferret_f256_eq(ferret_f256 a, ferret_f256 b);
bool ferret_f256_lt(ferret_f256 a, ferret_f256 b);
bool ferret_f256_gt(ferret_f256 a, ferret_f256 b);

// Conversion functions for floating point
ferret_f128 ferret_f128_from_f64(double val);
ferret_f256 ferret_f256_from_f64(double val);
double ferret_f128_to_f64(ferret_f128 val);
double ferret_f256_to_f64(ferret_f256 val);

// Bitwise operations for integers
#ifdef FERRET_HAS_INT128
    #define ferret_i128_and(a, b) ((a) & (b))
    #define ferret_i128_or(a, b) ((a) | (b))
    #define ferret_i128_xor(a, b) ((a) ^ (b))
    #define ferret_i128_not(a) (~(a))
    #define ferret_i128_shl(a, n) ((a) << (n))
    #define ferret_i128_shr(a, n) ((a) >> (n))
    
    #define ferret_u128_and(a, b) ((a) & (b))
    #define ferret_u128_or(a, b) ((a) | (b))
    #define ferret_u128_xor(a, b) ((a) ^ (b))
    #define ferret_u128_not(a) (~(a))
    #define ferret_u128_shl(a, n) ((a) << (n))
    #define ferret_u128_shr(a, n) ((a) >> (n))
#else
    ferret_i128 ferret_i128_and(ferret_i128 a, ferret_i128 b);
    ferret_i128 ferret_i128_or(ferret_i128 a, ferret_i128 b);
    ferret_i128 ferret_i128_xor(ferret_i128 a, ferret_i128 b);
    ferret_i128 ferret_i128_not(ferret_i128 a);
    ferret_i128 ferret_i128_shl(ferret_i128 a, int n);
    ferret_i128 ferret_i128_shr(ferret_i128 a, int n);
    
    ferret_u128 ferret_u128_and(ferret_u128 a, ferret_u128 b);
    ferret_u128 ferret_u128_or(ferret_u128 a, ferret_u128 b);
    ferret_u128 ferret_u128_xor(ferret_u128 a, ferret_u128 b);
    ferret_u128 ferret_u128_not(ferret_u128 a);
    ferret_u128 ferret_u128_shl(ferret_u128 a, int n);
    ferret_u128 ferret_u128_shr(ferret_u128 a, int n);
#endif

ferret_i256 ferret_i256_and(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_or(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_xor(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_not(ferret_i256 a);
ferret_i256 ferret_i256_shl(ferret_i256 a, int n);
ferret_i256 ferret_i256_shr(ferret_i256 a, int n);

ferret_u256 ferret_u256_and(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_or(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_xor(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_not(ferret_u256 a);
ferret_u256 ferret_u256_shl(ferret_u256 a, int n);
ferret_u256 ferret_u256_shr(ferret_u256 a, int n);

// String conversion functions
char* ferret_i128_to_string(ferret_i128 val);
char* ferret_u128_to_string(ferret_u128 val);
char* ferret_i256_to_string(ferret_i256 val);
char* ferret_u256_to_string(ferret_u256 val);
char* ferret_f128_to_string(ferret_f128 val);
char* ferret_f256_to_string(ferret_f256 val);

ferret_i128 ferret_i128_from_string(const char* str);
ferret_u128 ferret_u128_from_string(const char* str);
ferret_i256 ferret_i256_from_string(const char* str);
ferret_u256 ferret_u256_from_string(const char* str);
ferret_f128 ferret_f128_from_string(const char* str);
ferret_f256 ferret_f256_from_string(const char* str);

#endif // FERRET_BIGINT_H
