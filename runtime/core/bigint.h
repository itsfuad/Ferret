#ifndef FERRET_BIGINT_H
#define FERRET_BIGINT_H

#include <stdint.h>
#include <stdbool.h>
#include <math.h>

// Limb size detection for big integers (default to 32-bit if unknown)
#if defined(__SIZEOF_INT128__)
    #ifndef FERRET_HAS_WIDE128
        #define FERRET_HAS_WIDE128 1
    #endif
#endif

#if defined(UINTPTR_MAX) && UINTPTR_MAX >= 0xffffffffffffffffULL && defined(FERRET_HAS_WIDE128)
    #define FERRET_LIMB_BITS 64
#else
    #define FERRET_LIMB_BITS 32
#endif

#if FERRET_LIMB_BITS == 64
    typedef uint64_t ferret_limb_t;
#else
    typedef uint32_t ferret_limb_t;
#endif

#define FERRET_U128_LIMBS (128 / FERRET_LIMB_BITS)
#define FERRET_U256_LIMBS (256 / FERRET_LIMB_BITS)
#define FERRET_LIMB_MAX ((ferret_limb_t)~(ferret_limb_t)0)

// Feature detection for 128-bit floating point support
// Note: __float128 is NOT supported on macOS (Apple Clang), only on Linux with GCC/Clang
#if defined(__GNUC__) && !defined(__APPLE__) && (defined(__x86_64__) || defined(__i386__) || defined(__powerpc__) || defined(__powerpc64__))
    #ifndef FERRET_HAS_FLOAT128
        #define FERRET_HAS_FLOAT128 1
    #endif
#endif

// 128-bit integer types (limb-based)
typedef struct {
    ferret_limb_t words[FERRET_U128_LIMBS];  // Little-endian limbs
} ferret_u128;

typedef struct {
    ferret_limb_t words[FERRET_U128_LIMBS];  // Two's complement
} ferret_i128;

// 256-bit integer types (limb-based)
typedef struct {
    ferret_limb_t words[FERRET_U256_LIMBS];  // Little-endian limbs
} ferret_u256;

typedef struct {
    ferret_limb_t words[FERRET_U256_LIMBS];  // Two's complement
} ferret_i256;

// 128-bit integer operations (always limb-based)
ferret_i128 ferret_i128_add(ferret_i128 a, ferret_i128 b);
ferret_i128 ferret_i128_sub(ferret_i128 a, ferret_i128 b);
ferret_i128 ferret_i128_mul(ferret_i128 a, ferret_i128 b);
ferret_i128 ferret_i128_div(ferret_i128 a, ferret_i128 b);
ferret_i128 ferret_i128_mod(ferret_i128 a, ferret_i128 b);
bool ferret_i128_eq(ferret_i128 a, ferret_i128 b);
bool ferret_i128_lt(ferret_i128 a, ferret_i128 b);
bool ferret_i128_gt(ferret_i128 a, ferret_i128 b);
ferret_i128 ferret_i128_pow(ferret_i128 base, ferret_i128 exp);

ferret_u128 ferret_u128_add(ferret_u128 a, ferret_u128 b);
ferret_u128 ferret_u128_sub(ferret_u128 a, ferret_u128 b);
ferret_u128 ferret_u128_mul(ferret_u128 a, ferret_u128 b);
ferret_u128 ferret_u128_div(ferret_u128 a, ferret_u128 b);
ferret_u128 ferret_u128_mod(ferret_u128 a, ferret_u128 b);
bool ferret_u128_eq(ferret_u128 a, ferret_u128 b);
bool ferret_u128_lt(ferret_u128 a, ferret_u128 b);
bool ferret_u128_gt(ferret_u128 a, ferret_u128 b);
ferret_u128 ferret_u128_pow(ferret_u128 base, ferret_u128 exp);

// 256-bit integer operations (always implemented in bigint.c)
ferret_i256 ferret_i256_add(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_sub(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_mul(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_div(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_mod(ferret_i256 a, ferret_i256 b);
bool ferret_i256_eq(ferret_i256 a, ferret_i256 b);
bool ferret_i256_lt(ferret_i256 a, ferret_i256 b);
bool ferret_i256_gt(ferret_i256 a, ferret_i256 b);
ferret_i256 ferret_i256_pow(ferret_i256 base, ferret_i256 exp);

ferret_u256 ferret_u256_add(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_sub(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_mul(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_div(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_mod(ferret_u256 a, ferret_u256 b);
bool ferret_u256_eq(ferret_u256 a, ferret_u256 b);
bool ferret_u256_lt(ferret_u256 a, ferret_u256 b);
bool ferret_u256_gt(ferret_u256 a, ferret_u256 b);
ferret_u256 ferret_u256_pow(ferret_u256 base, ferret_u256 exp);

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

// Pointer-based helpers (for MIR/QBE lowering)
void ferret_memcpy(void* dst, const void* src, uint64_t size);

void ferret_i128_add_ptr(const ferret_i128* a, const ferret_i128* b, ferret_i128* out);
void ferret_i128_sub_ptr(const ferret_i128* a, const ferret_i128* b, ferret_i128* out);
void ferret_i128_mul_ptr(const ferret_i128* a, const ferret_i128* b, ferret_i128* out);
void ferret_i128_div_ptr(const ferret_i128* a, const ferret_i128* b, ferret_i128* out);
void ferret_i128_mod_ptr(const ferret_i128* a, const ferret_i128* b, ferret_i128* out);
bool ferret_i128_eq_ptr(const ferret_i128* a, const ferret_i128* b);
bool ferret_i128_lt_ptr(const ferret_i128* a, const ferret_i128* b);
bool ferret_i128_gt_ptr(const ferret_i128* a, const ferret_i128* b);
void ferret_i128_and_ptr(const ferret_i128* a, const ferret_i128* b, ferret_i128* out);
void ferret_i128_or_ptr(const ferret_i128* a, const ferret_i128* b, ferret_i128* out);
void ferret_i128_xor_ptr(const ferret_i128* a, const ferret_i128* b, ferret_i128* out);
void ferret_i128_pow_ptr(const ferret_i128* base, const ferret_i128* exp, ferret_i128* out);

void ferret_u128_add_ptr(const ferret_u128* a, const ferret_u128* b, ferret_u128* out);
void ferret_u128_sub_ptr(const ferret_u128* a, const ferret_u128* b, ferret_u128* out);
void ferret_u128_mul_ptr(const ferret_u128* a, const ferret_u128* b, ferret_u128* out);
void ferret_u128_div_ptr(const ferret_u128* a, const ferret_u128* b, ferret_u128* out);
void ferret_u128_mod_ptr(const ferret_u128* a, const ferret_u128* b, ferret_u128* out);
bool ferret_u128_eq_ptr(const ferret_u128* a, const ferret_u128* b);
bool ferret_u128_lt_ptr(const ferret_u128* a, const ferret_u128* b);
bool ferret_u128_gt_ptr(const ferret_u128* a, const ferret_u128* b);
void ferret_u128_and_ptr(const ferret_u128* a, const ferret_u128* b, ferret_u128* out);
void ferret_u128_or_ptr(const ferret_u128* a, const ferret_u128* b, ferret_u128* out);
void ferret_u128_xor_ptr(const ferret_u128* a, const ferret_u128* b, ferret_u128* out);
void ferret_u128_pow_ptr(const ferret_u128* base, const ferret_u128* exp, ferret_u128* out);

void ferret_i256_add_ptr(const ferret_i256* a, const ferret_i256* b, ferret_i256* out);
void ferret_i256_sub_ptr(const ferret_i256* a, const ferret_i256* b, ferret_i256* out);
void ferret_i256_mul_ptr(const ferret_i256* a, const ferret_i256* b, ferret_i256* out);
void ferret_i256_div_ptr(const ferret_i256* a, const ferret_i256* b, ferret_i256* out);
void ferret_i256_mod_ptr(const ferret_i256* a, const ferret_i256* b, ferret_i256* out);
bool ferret_i256_eq_ptr(const ferret_i256* a, const ferret_i256* b);
bool ferret_i256_lt_ptr(const ferret_i256* a, const ferret_i256* b);
bool ferret_i256_gt_ptr(const ferret_i256* a, const ferret_i256* b);
void ferret_i256_and_ptr(const ferret_i256* a, const ferret_i256* b, ferret_i256* out);
void ferret_i256_or_ptr(const ferret_i256* a, const ferret_i256* b, ferret_i256* out);
void ferret_i256_xor_ptr(const ferret_i256* a, const ferret_i256* b, ferret_i256* out);
void ferret_i256_not_ptr(const ferret_i256* a, ferret_i256* out);
void ferret_i256_pow_ptr(const ferret_i256* base, const ferret_i256* exp, ferret_i256* out);

void ferret_u256_add_ptr(const ferret_u256* a, const ferret_u256* b, ferret_u256* out);
void ferret_u256_sub_ptr(const ferret_u256* a, const ferret_u256* b, ferret_u256* out);
void ferret_u256_mul_ptr(const ferret_u256* a, const ferret_u256* b, ferret_u256* out);
void ferret_u256_div_ptr(const ferret_u256* a, const ferret_u256* b, ferret_u256* out);
void ferret_u256_mod_ptr(const ferret_u256* a, const ferret_u256* b, ferret_u256* out);
bool ferret_u256_eq_ptr(const ferret_u256* a, const ferret_u256* b);
bool ferret_u256_lt_ptr(const ferret_u256* a, const ferret_u256* b);
bool ferret_u256_gt_ptr(const ferret_u256* a, const ferret_u256* b);
void ferret_u256_and_ptr(const ferret_u256* a, const ferret_u256* b, ferret_u256* out);
void ferret_u256_or_ptr(const ferret_u256* a, const ferret_u256* b, ferret_u256* out);
void ferret_u256_xor_ptr(const ferret_u256* a, const ferret_u256* b, ferret_u256* out);
void ferret_u256_not_ptr(const ferret_u256* a, ferret_u256* out);
void ferret_u256_pow_ptr(const ferret_u256* base, const ferret_u256* exp, ferret_u256* out);

void ferret_f128_add_ptr(const ferret_f128* a, const ferret_f128* b, ferret_f128* out);
void ferret_f128_sub_ptr(const ferret_f128* a, const ferret_f128* b, ferret_f128* out);
void ferret_f128_mul_ptr(const ferret_f128* a, const ferret_f128* b, ferret_f128* out);
void ferret_f128_div_ptr(const ferret_f128* a, const ferret_f128* b, ferret_f128* out);
bool ferret_f128_eq_ptr(const ferret_f128* a, const ferret_f128* b);
bool ferret_f128_lt_ptr(const ferret_f128* a, const ferret_f128* b);
bool ferret_f128_gt_ptr(const ferret_f128* a, const ferret_f128* b);
void ferret_f128_pow_ptr(const ferret_f128* base, const ferret_f128* exp, ferret_f128* out);

void ferret_f256_add_ptr(const ferret_f256* a, const ferret_f256* b, ferret_f256* out);
void ferret_f256_sub_ptr(const ferret_f256* a, const ferret_f256* b, ferret_f256* out);
void ferret_f256_mul_ptr(const ferret_f256* a, const ferret_f256* b, ferret_f256* out);
void ferret_f256_div_ptr(const ferret_f256* a, const ferret_f256* b, ferret_f256* out);
bool ferret_f256_eq_ptr(const ferret_f256* a, const ferret_f256* b);
bool ferret_f256_lt_ptr(const ferret_f256* a, const ferret_f256* b);
bool ferret_f256_gt_ptr(const ferret_f256* a, const ferret_f256* b);
void ferret_f256_pow_ptr(const ferret_f256* base, const ferret_f256* exp, ferret_f256* out);

void ferret_i128_from_i64_ptr(int64_t val, ferret_i128* out);
void ferret_u128_from_u64_ptr(uint64_t val, ferret_u128* out);
void ferret_i256_from_i64_ptr(int64_t val, ferret_i256* out);
void ferret_u256_from_u64_ptr(uint64_t val, ferret_u256* out);
void ferret_f128_from_f64_ptr(double val, ferret_f128* out);
void ferret_f256_from_f64_ptr(double val, ferret_f256* out);

int64_t ferret_i128_to_i64_ptr(const ferret_i128* val);
uint64_t ferret_u128_to_u64_ptr(const ferret_u128* val);
int64_t ferret_i256_to_i64_ptr(const ferret_i256* val);
uint64_t ferret_u256_to_u64_ptr(const ferret_u256* val);
double ferret_f128_to_f64_ptr(const ferret_f128* val);
double ferret_f256_to_f64_ptr(const ferret_f256* val);

char* ferret_i128_to_string_ptr(const ferret_i128* val);
char* ferret_u128_to_string_ptr(const ferret_u128* val);
char* ferret_i256_to_string_ptr(const ferret_i256* val);
char* ferret_u256_to_string_ptr(const ferret_u256* val);
char* ferret_f128_to_string_ptr(const ferret_f128* val);
char* ferret_f256_to_string_ptr(const ferret_f256* val);

void ferret_i128_from_string_ptr(const char* str, ferret_i128* out);
void ferret_u128_from_string_ptr(const char* str, ferret_u128* out);
void ferret_i256_from_string_ptr(const char* str, ferret_i256* out);
void ferret_u256_from_string_ptr(const char* str, ferret_u256* out);
void ferret_f128_from_string_ptr(const char* str, ferret_f128* out);
void ferret_f256_from_string_ptr(const char* str, ferret_f256* out);

#endif // FERRET_BIGINT_H
