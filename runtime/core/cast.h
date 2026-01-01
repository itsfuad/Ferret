#ifndef FERRET_CAST_H
#define FERRET_CAST_H

#include <stdint.h>

float ferret_cast_u32_to_f32(uint32_t value);
double ferret_cast_u32_to_f64(uint32_t value);
float ferret_cast_u64_to_f32(uint64_t value);
double ferret_cast_u64_to_f64(uint64_t value);

uint32_t ferret_cast_f32_to_u32(float value);
uint32_t ferret_cast_f64_to_u32(double value);
uint64_t ferret_cast_f32_to_u64(float value);
uint64_t ferret_cast_f64_to_u64(double value);

#endif
