// Ferret runtime: IO functions header
// Native implementations for std/io module

#ifndef FERRET_IO_H
#define FERRET_IO_H

#include <stdint.h>
#include <stdbool.h>
#include "bigint.h"

// Single argument versions
// String versions
void ferret_io_Println(const char* msg);
void ferret_io_Print(const char* msg);

// Integer versions
void ferret_io_Println_i8(int8_t value);
void ferret_io_Println_i16(int16_t value);
void ferret_io_Println_i32(int32_t value);
void ferret_io_Println_i64(int64_t value);
void ferret_io_Print_i8(int8_t value);
void ferret_io_Print_i16(int16_t value);
void ferret_io_Print_i32(int32_t value);
void ferret_io_Print_i64(int64_t value);

// Unsigned integer versions
void ferret_io_Println_u8(uint8_t value);
void ferret_io_Println_u16(uint16_t value);
void ferret_io_Println_u32(uint32_t value);
void ferret_io_Println_u64(uint64_t value);
void ferret_io_Println_u128(ferret_u128 value);
void ferret_io_Println_u256(ferret_u256 value);
void ferret_io_Print_u8(uint8_t value);
void ferret_io_Print_u16(uint16_t value);
void ferret_io_Print_u32(uint32_t value);
void ferret_io_Print_u64(uint64_t value);
void ferret_io_Print_u128(ferret_u128 value);
void ferret_io_Print_u256(ferret_u256 value);

// Signed large integer versions
void ferret_io_Println_i128(ferret_i128 value);
void ferret_io_Println_i256(ferret_i256 value);
void ferret_io_Print_i128(ferret_i128 value);
void ferret_io_Print_i256(ferret_i256 value);

// Float versions
void ferret_io_Println_f32(float value);
void ferret_io_Println_f64(double value);
void ferret_io_Println_f128(ferret_f128 value);
void ferret_io_Println_f256(ferret_f256 value);
void ferret_io_Print_f32(float value);
void ferret_io_Print_f64(double value);
void ferret_io_Print_f128(ferret_f128 value);
void ferret_io_Print_f256(ferret_f256 value);

// Bool version
void ferret_io_Println_bool(int value);  // C uses int for bool
void ferret_io_Print_bool(int value);

// Pointer variants for large primitives (QBE ABI)
void ferret_io_Println_i128_ptr(const ferret_i128* value);
void ferret_io_Println_u128_ptr(const ferret_u128* value);
void ferret_io_Println_i256_ptr(const ferret_i256* value);
void ferret_io_Println_u256_ptr(const ferret_u256* value);
void ferret_io_Println_f128_ptr(const ferret_f128* value);
void ferret_io_Println_f256_ptr(const ferret_f256* value);
void ferret_io_Print_i128_ptr(const ferret_i128* value);
void ferret_io_Print_u128_ptr(const ferret_u128* value);
void ferret_io_Print_i256_ptr(const ferret_i256* value);
void ferret_io_Print_u256_ptr(const ferret_u256* value);
void ferret_io_Print_f128_ptr(const ferret_f128* value);
void ferret_io_Print_f256_ptr(const ferret_f256* value);

// Input functions
int32_t ferret_io_ReadInt(char** error);
double ferret_io_ReadFloat(char** error);

// String concatenation helper
// Returns a newly allocated string (caller must free)
char* ferret_io_ConcatStrings(const char* s1, const char* s2);

#endif // FERRET_IO_H
