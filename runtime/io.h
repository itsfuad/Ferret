// Ferret runtime: IO functions header
// Native implementations for std/io module

#ifndef FERRET_IO_H
#define FERRET_IO_H

#include <stdint.h>
#include <stdbool.h>

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
void ferret_io_Print_u8(uint8_t value);
void ferret_io_Print_u16(uint16_t value);
void ferret_io_Print_u32(uint32_t value);
void ferret_io_Print_u64(uint64_t value);

// Float versions
void ferret_io_Println_f32(float value);
void ferret_io_Println_f64(double value);
void ferret_io_Print_f32(float value);
void ferret_io_Print_f64(double value);

// Bool version
void ferret_io_Println_bool(int value);  // C uses int for bool
void ferret_io_Print_bool(int value);

// Input functions
int32_t ferret_io_ReadInt(char** error);
double ferret_io_ReadFloat(char** error);

// String concatenation helper
// Returns a newly allocated string (caller must free)
char* ferret_io_ConcatStrings(const char* s1, const char* s2);

#endif // FERRET_IO_H
