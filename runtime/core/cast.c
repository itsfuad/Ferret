// Ferret runtime: Type casting helpers

#include <stdint.h>

float ferret_cast_u32_to_f32(uint32_t value) {
    return (float)value;
}

double ferret_cast_u32_to_f64(uint32_t value) {
    return (double)value;
}

float ferret_cast_u64_to_f32(uint64_t value) {
    return (float)value;
}

double ferret_cast_u64_to_f64(uint64_t value) {
    return (double)value;
}

uint32_t ferret_cast_f32_to_u32(float value) {
    return (uint32_t)value;
}

uint32_t ferret_cast_f64_to_u32(double value) {
    return (uint32_t)value;
}

uint64_t ferret_cast_f32_to_u64(float value) {
    return (uint64_t)value;
}

uint64_t ferret_cast_f64_to_u64(double value) {
    return (uint64_t)value;
}
