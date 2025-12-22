#define _POSIX_C_SOURCE 200809L
#include "bigint.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#ifndef FERRET_HAS_INT128
// 128-bit integer operations (struct-based fallback)

ferret_i128 ferret_i128_add(ferret_i128 a, ferret_i128 b) {
    ferret_i128 result;
    uint64_t carry = 0;
    
    // Add low 64 bits
    result.lo = a.lo + b.lo;
    if (result.lo < a.lo) carry = 1;
    
    // Add high 64 bits with carry
    result.hi = a.hi + b.hi + carry;
    
    return result;
}

ferret_i128 ferret_i128_sub(ferret_i128 a, ferret_i128 b) {
    ferret_i128 result;
    uint64_t borrow = 0;
    
    // Subtract low 64 bits
    if (a.lo < b.lo) borrow = 1;
    result.lo = a.lo - b.lo;
    
    // Subtract high 64 bits with borrow
    result.hi = a.hi - b.hi - borrow;
    
    return result;
}

// Simplified multiplication (full implementation would use Karatsuba or similar)
ferret_i128 ferret_i128_mul(ferret_i128 a, ferret_i128 b) {
    // Split into 32-bit parts for multiplication
    uint64_t a_lo = a.lo & 0xFFFFFFFF;
    uint64_t a_hi = a.lo >> 32;
    uint64_t b_lo = b.lo & 0xFFFFFFFF;
    uint64_t b_hi = b.lo >> 32;
    
    uint64_t p0 = a_lo * b_lo;
    uint64_t p1 = a_lo * b_hi;
    uint64_t p2 = a_hi * b_lo;
    uint64_t p3 = a_hi * b_hi;
    
    ferret_i128 result;
    result.lo = p0 + ((p1 + p2) << 32);
    result.hi = p3 + (p1 >> 32) + (p2 >> 32) + ((p0 >> 32) ? 1 : 0);
    
    // Handle sign extension for high bits
    if ((int64_t)a.hi < 0) {
        result.hi -= b.lo;
    }
    if ((int64_t)b.hi < 0) {
        result.hi -= a.lo;
    }
    
    return result;
}

// Simplified division (full implementation would use long division)
ferret_i128 ferret_i128_div(ferret_i128 a, ferret_i128 b) {
    // For now, convert to 64-bit, divide, convert back
    // Full implementation would use binary long division
    if (b.hi == 0 && b.lo <= UINT64_MAX) {
        uint64_t divisor = b.lo;
        if (a.hi == 0 && a.lo <= UINT64_MAX) {
            ferret_i128 result;
            result.lo = a.lo / divisor;
            result.hi = 0;
            return result;
        }
    }
    // TODO: Implement full 128-bit division
    ferret_i128 zero = {0, 0};
    return zero;
}

ferret_i128 ferret_i128_mod(ferret_i128 a, ferret_i128 b) {
    ferret_i128 div = ferret_i128_div(a, b);
    ferret_i128 mul = ferret_i128_mul(div, b);
    return ferret_i128_sub(a, mul);
}

bool ferret_i128_eq(ferret_i128 a, ferret_i128 b) {
    return a.lo == b.lo && a.hi == b.hi;
}

bool ferret_i128_lt(ferret_i128 a, ferret_i128 b) {
    if (a.hi < b.hi) return true;
    if (a.hi > b.hi) return false;
    return a.lo < b.lo;
}

bool ferret_i128_gt(ferret_i128 a, ferret_i128 b) {
    return ferret_i128_lt(b, a);
}

// Similar implementations for u128...
ferret_u128 ferret_u128_add(ferret_u128 a, ferret_u128 b) {
    ferret_u128 result;
    uint64_t carry = 0;
    result.lo = a.lo + b.lo;
    if (result.lo < a.lo) carry = 1;
    result.hi = a.hi + b.hi + carry;
    return result;
}

ferret_u128 ferret_u128_sub(ferret_u128 a, ferret_u128 b) {
    ferret_u128 result;
    uint64_t borrow = 0;
    if (a.lo < b.lo) borrow = 1;
    result.lo = a.lo - b.lo;
    result.hi = a.hi - b.hi - borrow;
    return result;
}

ferret_u128 ferret_u128_mul(ferret_u128 a, ferret_u128 b) {
    // Similar to i128_mul but unsigned
    uint64_t a_lo = a.lo & 0xFFFFFFFF;
    uint64_t a_hi = a.lo >> 32;
    uint64_t b_lo = b.lo & 0xFFFFFFFF;
    uint64_t b_hi = b.lo >> 32;
    
    uint64_t p0 = a_lo * b_lo;
    uint64_t p1 = a_lo * b_hi;
    uint64_t p2 = a_hi * b_lo;
    uint64_t p3 = a_hi * b_hi;
    
    ferret_u128 result;
    result.lo = p0 + ((p1 + p2) << 32);
    result.hi = p3 + (p1 >> 32) + (p2 >> 32) + ((p0 >> 32) ? 1 : 0);
    return result;
}

ferret_u128 ferret_u128_div(ferret_u128 a, ferret_u128 b) {
    // Simplified - full implementation needed
    if (b.hi == 0 && b.lo <= UINT64_MAX) {
        uint64_t divisor = b.lo;
        if (a.hi == 0 && a.lo <= UINT64_MAX) {
            ferret_u128 result;
            result.lo = a.lo / divisor;
            result.hi = 0;
            return result;
        }
    }
    ferret_u128 zero = {0, 0};
    return zero;
}

ferret_u128 ferret_u128_mod(ferret_u128 a, ferret_u128 b) {
    ferret_u128 div = ferret_u128_div(a, b);
    ferret_u128 mul = ferret_u128_mul(div, b);
    return ferret_u128_sub(a, mul);
}

bool ferret_u128_eq(ferret_u128 a, ferret_u128 b) {
    return a.lo == b.lo && a.hi == b.hi;
}

bool ferret_u128_lt(ferret_u128 a, ferret_u128 b) {
    if (a.hi < b.hi) return true;
    if (a.hi > b.hi) return false;
    return a.lo < b.lo;
}

bool ferret_u128_gt(ferret_u128 a, ferret_u128 b) {
    return ferret_u128_lt(b, a);
}

#endif // !FERRET_HAS_INT128

// 256-bit integer operations (always struct-based)

ferret_i256 ferret_i256_add(ferret_i256 a, ferret_i256 b) {
    ferret_i256 result = {{0, 0, 0, 0}};
    uint64_t carry = 0;
    
    for (int i = 0; i < 4; i++) {
        uint64_t sum = a.words[i] + b.words[i] + carry;
        result.words[i] = sum;
        carry = (sum < a.words[i] || (carry && sum == a.words[i])) ? 1 : 0;
    }
    
    return result;
}

ferret_i256 ferret_i256_sub(ferret_i256 a, ferret_i256 b) {
    ferret_i256 result = {{0, 0, 0, 0}};
    uint64_t borrow = 0;
    
    for (int i = 0; i < 4; i++) {
        uint64_t diff = a.words[i] - b.words[i] - borrow;
        result.words[i] = diff;
        borrow = (a.words[i] < b.words[i] || (borrow && a.words[i] == b.words[i])) ? 1 : 0;
    }
    
    return result;
}

// Full 256-bit multiplication using long multiplication
ferret_i256 ferret_i256_mul(ferret_i256 a, ferret_i256 b) {
    ferret_i256 result = {{0, 0, 0, 0}};
    ferret_u256 temp = {{0, 0, 0, 0}};
    
    // Use unsigned multiplication, handle sign separately
    bool a_neg = ((int64_t)a.words[3] < 0);
    bool b_neg = ((int64_t)b.words[3] < 0);
    bool result_neg = (a_neg != b_neg);
    
    ferret_u256 a_abs = {{(uint64_t)a.words[0], (uint64_t)a.words[1], (uint64_t)a.words[2], (uint64_t)a.words[3]}};
    ferret_u256 b_abs = {{(uint64_t)b.words[0], (uint64_t)b.words[1], (uint64_t)b.words[2], (uint64_t)b.words[3]}};
    
    if (a_neg) {
        // Negate a_abs (two's complement)
        uint64_t carry = 1;
        for (int i = 0; i < 4; i++) {
            uint64_t sum = ~a_abs.words[i] + carry;
            a_abs.words[i] = sum;
            carry = (sum < ~a_abs.words[i] || (carry && sum == ~a_abs.words[i])) ? 1 : 0;
        }
    }
    if (b_neg) {
        // Negate b_abs (two's complement)
        uint64_t carry = 1;
        for (int i = 0; i < 4; i++) {
            uint64_t sum = ~b_abs.words[i] + carry;
            b_abs.words[i] = sum;
            carry = (sum < ~b_abs.words[i] || (carry && sum == ~b_abs.words[i])) ? 1 : 0;
        }
    }
    
    // Long multiplication: multiply each word of a by each word of b
    for (int i = 0; i < 4; i++) {
        uint64_t carry = 0;
        for (int j = 0; j < 4; j++) {
            if (i + j < 4) {
                // Multiply a.words[i] * b.words[j]
                // Split into high and low 32-bit parts to avoid overflow
                uint64_t a_lo = a_abs.words[i] & 0xFFFFFFFF;
                uint64_t a_hi = a_abs.words[i] >> 32;
                uint64_t b_lo = b_abs.words[j] & 0xFFFFFFFF;
                uint64_t b_hi = b_abs.words[j] >> 32;
                
                // (a_hi * 2^32 + a_lo) * (b_hi * 2^32 + b_lo)
                // = a_hi * b_hi * 2^64 + (a_hi * b_lo + a_lo * b_hi) * 2^32 + a_lo * b_lo
                uint64_t p0 = a_lo * b_lo;
                uint64_t p1 = a_lo * b_hi;
                uint64_t p2 = a_hi * b_lo;
                uint64_t p3 = a_hi * b_hi;
                
                // Add to result at position i+j
                uint64_t sum_lo = temp.words[i + j] + (p0 & 0xFFFFFFFF);
                uint64_t carry1 = (sum_lo < temp.words[i + j]) ? 1 : 0;
                temp.words[i + j] = sum_lo;
                
                uint64_t sum_mid = carry + (p0 >> 32) + (p1 & 0xFFFFFFFF) + (p2 & 0xFFFFFFFF) + carry1;
                carry = sum_mid >> 32;
                if (i + j + 1 < 4) {
                    uint64_t sum_hi = temp.words[i + j + 1] + (sum_mid & 0xFFFFFFFF);
                    carry += (sum_hi < temp.words[i + j + 1]) ? 1 : 0;
                    temp.words[i + j + 1] = sum_hi;
                }
                
                if (i + j + 2 < 4) {
                    temp.words[i + j + 2] += (p1 >> 32) + (p2 >> 32) + p3 + carry;
                }
            }
        }
    }
    
    // Copy to result
    for (int i = 0; i < 4; i++) {
        result.words[i] = (int64_t)temp.words[i];
    }
    
    // Negate if needed
    if (result_neg) {
        // Two's complement negation
        uint64_t carry = 1;
        for (int i = 0; i < 4; i++) {
            uint64_t sum = ~(uint64_t)result.words[i] + carry;
            result.words[i] = (int64_t)sum;
            carry = (sum < ~(uint64_t)result.words[i] || (carry && sum == ~(uint64_t)result.words[i])) ? 1 : 0;
        }
    }
    
    return result;
}

// Full 256-bit division using binary long division
ferret_i256 ferret_i256_div(ferret_i256 a, ferret_i256 b) {
    // Check for division by zero
    bool b_zero = true;
    for (int i = 0; i < 4; i++) {
        if (b.words[i] != 0) {
            b_zero = false;
            break;
        }
    }
    if (b_zero) {
        // Division by zero - return zero (could also set error flag)
        ferret_i256 zero = {{0, 0, 0, 0}};
        return zero;
    }
    
    // Handle signs
    bool a_neg = ((int64_t)a.words[3] < 0);
    bool b_neg = ((int64_t)b.words[3] < 0);
    bool result_neg = (a_neg != b_neg);
    
    ferret_u256 a_abs = {{(uint64_t)a.words[0], (uint64_t)a.words[1], (uint64_t)a.words[2], (uint64_t)a.words[3]}};
    ferret_u256 b_abs = {{(uint64_t)b.words[0], (uint64_t)b.words[1], (uint64_t)b.words[2], (uint64_t)b.words[3]}};
    
    if (a_neg) {
        // Negate a_abs
        uint64_t carry = 1;
        for (int i = 0; i < 4; i++) {
            uint64_t sum = ~a_abs.words[i] + carry;
            a_abs.words[i] = sum;
            carry = (sum < ~a_abs.words[i] || (carry && sum == ~a_abs.words[i])) ? 1 : 0;
        }
    }
    if (b_neg) {
        // Negate b_abs
        uint64_t carry = 1;
        for (int i = 0; i < 4; i++) {
            uint64_t sum = ~b_abs.words[i] + carry;
            b_abs.words[i] = sum;
            carry = (sum < ~b_abs.words[i] || (carry && sum == ~b_abs.words[i])) ? 1 : 0;
        }
    }
    
    // Binary long division algorithm
    ferret_u256 quotient = {{0, 0, 0, 0}};
    ferret_u256 remainder = {{0, 0, 0, 0}};
    
    // Find the most significant bit of dividend
    int msb = -1;
    for (int i = 3; i >= 0; i--) {
        if (a_abs.words[i] != 0) {
            uint64_t word = a_abs.words[i];
            for (int bit = 63; bit >= 0; bit--) {
                if (word & ((uint64_t)1 << bit)) {
                    msb = i * 64 + bit;
                    break;
                }
            }
            if (msb >= 0) break;
        }
    }
    
    if (msb < 0) {
        // Dividend is zero
        ferret_i256 zero = {{0, 0, 0, 0}};
        return zero;
    }
    
    // Perform binary long division
    for (int bit = msb; bit >= 0; bit--) {
        // Shift remainder left by 1
        uint64_t carry = 0;
        for (int i = 0; i < 4; i++) {
            uint64_t new_carry = remainder.words[i] >> 63;
            remainder.words[i] = (remainder.words[i] << 1) | carry;
            carry = new_carry;
        }
        
        // Set LSB of remainder to current bit of dividend
        int word_idx = bit / 64;
        int bit_idx = bit % 64;
        if (word_idx < 4) {
            if (a_abs.words[word_idx] & ((uint64_t)1 << bit_idx)) {
                remainder.words[0] |= 1;
            }
        }
        
        // Compare remainder with divisor
        bool ge = false;
        for (int i = 3; i >= 0; i--) {
            if (remainder.words[i] > b_abs.words[i]) {
                ge = true;
                break;
            } else if (remainder.words[i] < b_abs.words[i]) {
                break;
            }
        }
        
        if (ge || memcmp(remainder.words, b_abs.words, sizeof(remainder.words)) == 0) {
            // Subtract divisor from remainder
            uint64_t borrow = 0;
            for (int i = 0; i < 4; i++) {
                uint64_t diff = remainder.words[i] - b_abs.words[i] - borrow;
                borrow = (remainder.words[i] < b_abs.words[i] || (borrow && remainder.words[i] == b_abs.words[i])) ? 1 : 0;
                remainder.words[i] = diff;
            }
            
            // Set bit in quotient
            word_idx = bit / 64;
            bit_idx = bit % 64;
            if (word_idx < 4) {
                quotient.words[word_idx] |= ((uint64_t)1 << bit_idx);
            }
        }
    }
    
    // Convert quotient to signed result
    ferret_i256 result;
    for (int i = 0; i < 4; i++) {
        result.words[i] = (int64_t)quotient.words[i];
    }
    
    // Negate if needed
    if (result_neg) {
        uint64_t carry = 1;
        for (int i = 0; i < 4; i++) {
            uint64_t sum = ~(uint64_t)result.words[i] + carry;
            result.words[i] = (int64_t)sum;
            carry = (sum < ~(uint64_t)result.words[i] || (carry && sum == ~(uint64_t)result.words[i])) ? 1 : 0;
        }
    }
    
    return result;
}

ferret_i256 ferret_i256_mod(ferret_i256 a, ferret_i256 b) {
    ferret_i256 div = ferret_i256_div(a, b);
    ferret_i256 mul = ferret_i256_mul(div, b);
    return ferret_i256_sub(a, mul);
}

bool ferret_i256_eq(ferret_i256 a, ferret_i256 b) {
    return memcmp(a.words, b.words, sizeof(a.words)) == 0;
}

bool ferret_i256_lt(ferret_i256 a, ferret_i256 b) {
    // Compare from most significant word to least
    for (int i = 3; i >= 0; i--) {
        if (a.words[i] < b.words[i]) return true;
        if (a.words[i] > b.words[i]) return false;
    }
    return false;
}

bool ferret_i256_gt(ferret_i256 a, ferret_i256 b) {
    return ferret_i256_lt(b, a);
}

// Similar for u256...
ferret_u256 ferret_u256_add(ferret_u256 a, ferret_u256 b) {
    ferret_u256 result = {{0, 0, 0, 0}};
    uint64_t carry = 0;
    for (int i = 0; i < 4; i++) {
        uint64_t sum = a.words[i] + b.words[i] + carry;
        result.words[i] = sum;
        carry = (sum < a.words[i] || (carry && sum == a.words[i])) ? 1 : 0;
    }
    return result;
}

ferret_u256 ferret_u256_sub(ferret_u256 a, ferret_u256 b) {
    ferret_u256 result = {{0, 0, 0, 0}};
    uint64_t borrow = 0;
    for (int i = 0; i < 4; i++) {
        uint64_t diff = a.words[i] - b.words[i] - borrow;
        result.words[i] = diff;
        borrow = (a.words[i] < b.words[i] || (borrow && a.words[i] == b.words[i])) ? 1 : 0;
    }
    return result;
}

ferret_u256 ferret_u256_mul(ferret_u256 a, ferret_u256 b) {
    ferret_u256 result = {{0, 0, 0, 0}};
    
    // Long multiplication: multiply each word of a by each word of b
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            if (i + j < 4) {
                // Multiply a.words[i] * b.words[j]
                uint64_t a_lo = a.words[i] & 0xFFFFFFFF;
                uint64_t a_hi = a.words[i] >> 32;
                uint64_t b_lo = b.words[j] & 0xFFFFFFFF;
                uint64_t b_hi = b.words[j] >> 32;
                
                uint64_t p0 = a_lo * b_lo;
                uint64_t p1 = a_lo * b_hi;
                uint64_t p2 = a_hi * b_lo;
                uint64_t p3 = a_hi * b_hi;
                
                // Add to result at position i+j
                uint64_t sum_lo = result.words[i + j] + (p0 & 0xFFFFFFFF);
                uint64_t carry1 = (sum_lo < result.words[i + j]) ? 1 : 0;
                result.words[i + j] = sum_lo;
                
                if (i + j + 1 < 4) {
                    uint64_t sum_mid = (p0 >> 32) + (p1 & 0xFFFFFFFF) + (p2 & 0xFFFFFFFF) + carry1;
                    uint64_t carry2 = sum_mid >> 32;
                    uint64_t sum_hi = result.words[i + j + 1] + (sum_mid & 0xFFFFFFFF);
                    carry2 += (sum_hi < result.words[i + j + 1]) ? 1 : 0;
                    result.words[i + j + 1] = sum_hi;
                    
                    if (i + j + 2 < 4) {
                        result.words[i + j + 2] += (p1 >> 32) + (p2 >> 32) + p3 + carry2;
                    }
                }
            }
        }
    }
    
    return result;
}

ferret_u256 ferret_u256_div(ferret_u256 a, ferret_u256 b) {
    // Check for division by zero
    bool b_zero = true;
    for (int i = 0; i < 4; i++) {
        if (b.words[i] != 0) {
            b_zero = false;
            break;
        }
    }
    if (b_zero) {
        ferret_u256 zero = {{0, 0, 0, 0}};
        return zero;
    }
    
    ferret_u256 quotient = {{0, 0, 0, 0}};
    ferret_u256 remainder = {{0, 0, 0, 0}};
    
    // Find the most significant bit of dividend
    int msb = -1;
    for (int i = 3; i >= 0; i--) {
        if (a.words[i] != 0) {
            uint64_t word = a.words[i];
            for (int bit = 63; bit >= 0; bit--) {
                if (word & ((uint64_t)1 << bit)) {
                    msb = i * 64 + bit;
                    break;
                }
            }
            if (msb >= 0) break;
        }
    }
    
    if (msb < 0) {
        ferret_u256 zero = {{0, 0, 0, 0}};
        return zero;
    }
    
    // Binary long division
    for (int bit = msb; bit >= 0; bit--) {
        // Shift remainder left by 1
        uint64_t carry = 0;
        for (int i = 0; i < 4; i++) {
            uint64_t new_carry = remainder.words[i] >> 63;
            remainder.words[i] = (remainder.words[i] << 1) | carry;
            carry = new_carry;
        }
        
        // Set LSB of remainder to current bit of dividend
        int word_idx = bit / 64;
        int bit_idx = bit % 64;
        if (word_idx < 4) {
            if (a.words[word_idx] & ((uint64_t)1 << bit_idx)) {
                remainder.words[0] |= 1;
            }
        }
        
        // Compare remainder with divisor
        bool ge = false;
        for (int i = 3; i >= 0; i--) {
            if (remainder.words[i] > b.words[i]) {
                ge = true;
                break;
            } else if (remainder.words[i] < b.words[i]) {
                break;
            }
        }
        
        if (ge || memcmp(remainder.words, b.words, sizeof(remainder.words)) == 0) {
            // Subtract divisor from remainder
            uint64_t borrow = 0;
            for (int i = 0; i < 4; i++) {
                uint64_t diff = remainder.words[i] - b.words[i] - borrow;
                borrow = (remainder.words[i] < b.words[i] || (borrow && remainder.words[i] == b.words[i])) ? 1 : 0;
                remainder.words[i] = diff;
            }
            
            // Set bit in quotient
            word_idx = bit / 64;
            bit_idx = bit % 64;
            if (word_idx < 4) {
                quotient.words[word_idx] |= ((uint64_t)1 << bit_idx);
            }
        }
    }
    
    return quotient;
}

ferret_u256 ferret_u256_mod(ferret_u256 a, ferret_u256 b) {
    ferret_u256 div = ferret_u256_div(a, b);
    ferret_u256 mul = ferret_u256_mul(div, b);
    return ferret_u256_sub(a, mul);
}

bool ferret_u256_eq(ferret_u256 a, ferret_u256 b) {
    return memcmp(a.words, b.words, sizeof(a.words)) == 0;
}

bool ferret_u256_lt(ferret_u256 a, ferret_u256 b) {
    for (int i = 3; i >= 0; i--) {
        if (a.words[i] < b.words[i]) return true;
        if (a.words[i] > b.words[i]) return false;
    }
    return false;
}

bool ferret_u256_gt(ferret_u256 a, ferret_u256 b) {
    return ferret_u256_lt(b, a);
}

// Conversion functions
ferret_i128 ferret_i128_from_i64(int64_t val) {
#ifdef FERRET_HAS_INT128
    return (ferret_i128)val;
#else
    ferret_i128 result;
    result.lo = (uint64_t)val;
    result.hi = val < 0 ? -1 : 0;  // Sign extend
    return result;
#endif
}

ferret_u128 ferret_u128_from_u64(uint64_t val) {
#ifdef FERRET_HAS_INT128
    return (ferret_u128)val;
#else
    ferret_u128 result;
    result.lo = val;
    result.hi = 0;
    return result;
#endif
}

ferret_i256 ferret_i256_from_i64(int64_t val) {
    ferret_i256 result = {{0, 0, 0, 0}};
    result.words[0] = (uint64_t)val;
    if (val < 0) {
        // Sign extend
        for (int i = 1; i < 4; i++) {
            result.words[i] = UINT64_MAX;
        }
    }
    return result;
}

ferret_u256 ferret_u256_from_u64(uint64_t val) {
    ferret_u256 result = {{0, 0, 0, 0}};
    result.words[0] = val;
    return result;
}

int64_t ferret_i128_to_i64(ferret_i128 val) {
#ifdef FERRET_HAS_INT128
    return (int64_t)val;
#else
    return (int64_t)val.lo;
#endif
}

uint64_t ferret_u128_to_u64(ferret_u128 val) {
#ifdef FERRET_HAS_INT128
    return (uint64_t)val;
#else
    return val.lo;
#endif
}

int64_t ferret_i256_to_i64(ferret_i256 val) {
    return (int64_t)val.words[0];
}

uint64_t ferret_u256_to_u64(ferret_u256 val) {
    return val.words[0];
}

// 128-bit floating point operations (fallback implementation)
#ifndef FERRET_HAS_FLOAT128

// Simplified IEEE 754 binary128 operations
// Full implementation would properly handle:
// - Sign bit extraction/manipulation
// - Exponent extraction/manipulation  
// - Mantissa normalization
// - Special values (NaN, Inf, -Inf, -0)

ferret_f128 ferret_f128_add(ferret_f128 a, ferret_f128 b) {
    // For now, convert to double, add, convert back
    // Full implementation would do proper 128-bit float arithmetic
    double a_val = ferret_f128_to_f64(a);
    double b_val = ferret_f128_to_f64(b);
    return ferret_f128_from_f64(a_val + b_val);
}

ferret_f128 ferret_f128_sub(ferret_f128 a, ferret_f128 b) {
    double a_val = ferret_f128_to_f64(a);
    double b_val = ferret_f128_to_f64(b);
    return ferret_f128_from_f64(a_val - b_val);
}

ferret_f128 ferret_f128_mul(ferret_f128 a, ferret_f128 b) {
    double a_val = ferret_f128_to_f64(a);
    double b_val = ferret_f128_to_f64(b);
    return ferret_f128_from_f64(a_val * b_val);
}

ferret_f128 ferret_f128_div(ferret_f128 a, ferret_f128 b) {
    double a_val = ferret_f128_to_f64(a);
    double b_val = ferret_f128_to_f64(b);
    return ferret_f128_from_f64(a_val / b_val);
}

bool ferret_f128_eq(ferret_f128 a, ferret_f128 b) {
    return memcmp(&a, &b, sizeof(ferret_f128)) == 0;
}

bool ferret_f128_lt(ferret_f128 a, ferret_f128 b) {
    double a_val = ferret_f128_to_f64(a);
    double b_val = ferret_f128_to_f64(b);
    return a_val < b_val;
}

bool ferret_f128_gt(ferret_f128 a, ferret_f128 b) {
    return ferret_f128_lt(b, a);
}

#endif // !FERRET_HAS_FLOAT128

// 256-bit floating point operations (always struct-based)
ferret_f256 ferret_f256_add(ferret_f256 a, ferret_f256 b) {
    // Simplified: convert to double, add, convert back
    // Full implementation would do proper 256-bit float arithmetic
    double a_val = ferret_f256_to_f64(a);
    double b_val = ferret_f256_to_f64(b);
    return ferret_f256_from_f64(a_val + b_val);
}

ferret_f256 ferret_f256_sub(ferret_f256 a, ferret_f256 b) {
    double a_val = ferret_f256_to_f64(a);
    double b_val = ferret_f256_to_f64(b);
    return ferret_f256_from_f64(a_val - b_val);
}

ferret_f256 ferret_f256_mul(ferret_f256 a, ferret_f256 b) {
    double a_val = ferret_f256_to_f64(a);
    double b_val = ferret_f256_to_f64(b);
    return ferret_f256_from_f64(a_val * b_val);
}

ferret_f256 ferret_f256_div(ferret_f256 a, ferret_f256 b) {
    double a_val = ferret_f256_to_f64(a);
    double b_val = ferret_f256_to_f64(b);
    return ferret_f256_from_f64(a_val / b_val);
}

bool ferret_f256_eq(ferret_f256 a, ferret_f256 b) {
    return memcmp(a.mantissa, b.mantissa, sizeof(a.mantissa)) == 0 &&
           a.exp_sign == b.exp_sign;
}

bool ferret_f256_lt(ferret_f256 a, ferret_f256 b) {
    double a_val = ferret_f256_to_f64(a);
    double b_val = ferret_f256_to_f64(b);
    return a_val < b_val;
}

bool ferret_f256_gt(ferret_f256 a, ferret_f256 b) {
    return ferret_f256_lt(b, a);
}

// Conversion functions for floating point
ferret_f128 ferret_f128_from_f64(double val) {
#ifdef FERRET_HAS_FLOAT128
    return (ferret_f128)val;
#else
    ferret_f128 result;
    // Simplified: store as double precision
    // Full implementation would properly encode as IEEE 754 binary128
    memcpy(&result, &val, sizeof(double));
    // Zero out upper bits
    memset((char*)&result + sizeof(double), 0, sizeof(ferret_f128) - sizeof(double));
    return result;
#endif
}

ferret_f256 ferret_f256_from_f64(double val) {
    ferret_f256 result = {{0, 0, 0}, 0};
    // Simplified: store as double precision in first mantissa word
    // Full implementation would properly encode as IEEE 754 extended format
    memcpy(&result.mantissa[0], &val, sizeof(double));
    return result;
}

double ferret_f128_to_f64(ferret_f128 val) {
#ifdef FERRET_HAS_FLOAT128
    return (double)val;
#else
    double result;
    // Simplified: extract double precision
    // Full implementation would properly decode IEEE 754 binary128
    memcpy(&result, &val, sizeof(double));
    return result;
#endif
}

double ferret_f256_to_f64(ferret_f256 val) {
    double result;
    // Simplified: extract double precision from first mantissa word
    // Full implementation would properly decode IEEE 754 extended format
    memcpy(&result, &val.mantissa[0], sizeof(double));
    return result;
}

// Bitwise operations for 128-bit integers (fallback)
#ifndef FERRET_HAS_INT128

ferret_i128 ferret_i128_and(ferret_i128 a, ferret_i128 b) {
    ferret_i128 result;
    result.lo = a.lo & b.lo;
    result.hi = a.hi & b.hi;
    return result;
}

ferret_i128 ferret_i128_or(ferret_i128 a, ferret_i128 b) {
    ferret_i128 result;
    result.lo = a.lo | b.lo;
    result.hi = a.hi | b.hi;
    return result;
}

ferret_i128 ferret_i128_xor(ferret_i128 a, ferret_i128 b) {
    ferret_i128 result;
    result.lo = a.lo ^ b.lo;
    result.hi = a.hi ^ b.hi;
    return result;
}

ferret_i128 ferret_i128_not(ferret_i128 a) {
    ferret_i128 result;
    result.lo = ~a.lo;
    result.hi = ~a.hi;
    return result;
}

ferret_i128 ferret_i128_shl(ferret_i128 a, int n) {
    if (n <= 0) return a;
    if (n >= 128) {
        ferret_i128 zero = {0, 0};
        return zero;
    }
    
    ferret_i128 result;
    if (n < 64) {
        result.lo = a.lo << n;
        result.hi = (a.hi << n) | (a.lo >> (64 - n));
    } else {
        result.lo = 0;
        result.hi = a.lo << (n - 64);
    }
    return result;
}

ferret_i128 ferret_i128_shr(ferret_i128 a, int n) {
    if (n <= 0) return a;
    if (n >= 128) {
        ferret_i128 zero = {0, 0};
        return zero;
    }
    
    ferret_i128 result;
    if (n < 64) {
        result.hi = (int64_t)a.hi >> n;  // Arithmetic shift for signed
        result.lo = (a.lo >> n) | ((uint64_t)a.hi << (64 - n));
    } else {
        result.lo = (int64_t)a.hi >> (n - 64);
        result.hi = (int64_t)a.hi >> 63;  // Sign extend
    }
    return result;
}

ferret_u128 ferret_u128_and(ferret_u128 a, ferret_u128 b) {
    ferret_u128 result;
    result.lo = a.lo & b.lo;
    result.hi = a.hi & b.hi;
    return result;
}

ferret_u128 ferret_u128_or(ferret_u128 a, ferret_u128 b) {
    ferret_u128 result;
    result.lo = a.lo | b.lo;
    result.hi = a.hi | b.hi;
    return result;
}

ferret_u128 ferret_u128_xor(ferret_u128 a, ferret_u128 b) {
    ferret_u128 result;
    result.lo = a.lo ^ b.lo;
    result.hi = a.hi ^ b.hi;
    return result;
}

ferret_u128 ferret_u128_not(ferret_u128 a) {
    ferret_u128 result;
    result.lo = ~a.lo;
    result.hi = ~a.hi;
    return result;
}

ferret_u128 ferret_u128_shl(ferret_u128 a, int n) {
    if (n <= 0) return a;
    if (n >= 128) {
        ferret_u128 zero = {0, 0};
        return zero;
    }
    
    ferret_u128 result;
    if (n < 64) {
        result.lo = a.lo << n;
        result.hi = (a.hi << n) | (a.lo >> (64 - n));
    } else {
        result.lo = 0;
        result.hi = a.lo << (n - 64);
    }
    return result;
}

ferret_u128 ferret_u128_shr(ferret_u128 a, int n) {
    if (n <= 0) return a;
    if (n >= 128) {
        ferret_u128 zero = {0, 0};
        return zero;
    }
    
    ferret_u128 result;
    if (n < 64) {
        result.hi = a.hi >> n;  // Logical shift for unsigned
        result.lo = (a.lo >> n) | (a.hi << (64 - n));
    } else {
        result.lo = a.hi >> (n - 64);
        result.hi = 0;
    }
    return result;
}

#endif // !FERRET_HAS_INT128

// Bitwise operations for 256-bit integers
ferret_i256 ferret_i256_and(ferret_i256 a, ferret_i256 b) {
    ferret_i256 result;
    for (int i = 0; i < 4; i++) {
        result.words[i] = a.words[i] & b.words[i];
    }
    return result;
}

ferret_i256 ferret_i256_or(ferret_i256 a, ferret_i256 b) {
    ferret_i256 result;
    for (int i = 0; i < 4; i++) {
        result.words[i] = a.words[i] | b.words[i];
    }
    return result;
}

ferret_i256 ferret_i256_xor(ferret_i256 a, ferret_i256 b) {
    ferret_i256 result;
    for (int i = 0; i < 4; i++) {
        result.words[i] = a.words[i] ^ b.words[i];
    }
    return result;
}

ferret_i256 ferret_i256_not(ferret_i256 a) {
    ferret_i256 result;
    for (int i = 0; i < 4; i++) {
        result.words[i] = ~a.words[i];
    }
    return result;
}

ferret_i256 ferret_i256_shl(ferret_i256 a, int n) {
    if (n <= 0) return a;
    if (n >= 256) {
        ferret_i256 zero = {{0, 0, 0, 0}};
        return zero;
    }
    
    ferret_i256 result = {{0, 0, 0, 0}};
    int word_shift = n / 64;
    int bit_shift = n % 64;
    
    for (int i = 0; i < 4; i++) {
        int src_idx = i - word_shift;
        if (src_idx >= 0 && src_idx < 4) {
            if (bit_shift == 0) {
                result.words[i] = a.words[src_idx];
            } else {
                result.words[i] = a.words[src_idx] << bit_shift;
                if (src_idx > 0) {
                    result.words[i] |= a.words[src_idx - 1] >> (64 - bit_shift);
                }
            }
        }
    }
    return result;
}

ferret_i256 ferret_i256_shr(ferret_i256 a, int n) {
    if (n <= 0) return a;
    if (n >= 256) {
        ferret_i256 result = {{0, 0, 0, 0}};
        // Sign extend
        if ((int64_t)a.words[3] < 0) {
            for (int i = 0; i < 4; i++) {
                result.words[i] = -1;
            }
        }
        return result;
    }
    
    ferret_i256 result = {{0, 0, 0, 0}};
    int word_shift = n / 64;
    int bit_shift = n % 64;
    
    for (int i = 0; i < 4; i++) {
        int src_idx = i + word_shift;
        if (src_idx < 4) {
            if (bit_shift == 0) {
                result.words[i] = a.words[src_idx];
            } else {
                result.words[i] = (int64_t)a.words[src_idx] >> bit_shift;
                if (src_idx < 3) {
                    result.words[i] |= (uint64_t)a.words[src_idx + 1] << (64 - bit_shift);
                } else {
                    // Sign extend
                    if ((int64_t)a.words[3] < 0) {
                        result.words[i] |= ((uint64_t)-1) << (64 - bit_shift);
                    }
                }
            }
        } else {
            // Sign extend
            if ((int64_t)a.words[3] < 0) {
                result.words[i] = -1;
            }
        }
    }
    return result;
}

ferret_u256 ferret_u256_and(ferret_u256 a, ferret_u256 b) {
    ferret_u256 result;
    for (int i = 0; i < 4; i++) {
        result.words[i] = a.words[i] & b.words[i];
    }
    return result;
}

ferret_u256 ferret_u256_or(ferret_u256 a, ferret_u256 b) {
    ferret_u256 result;
    for (int i = 0; i < 4; i++) {
        result.words[i] = a.words[i] | b.words[i];
    }
    return result;
}

ferret_u256 ferret_u256_xor(ferret_u256 a, ferret_u256 b) {
    ferret_u256 result;
    for (int i = 0; i < 4; i++) {
        result.words[i] = a.words[i] ^ b.words[i];
    }
    return result;
}

ferret_u256 ferret_u256_not(ferret_u256 a) {
    ferret_u256 result;
    for (int i = 0; i < 4; i++) {
        result.words[i] = ~a.words[i];
    }
    return result;
}

ferret_u256 ferret_u256_shl(ferret_u256 a, int n) {
    if (n <= 0) return a;
    if (n >= 256) {
        ferret_u256 zero = {{0, 0, 0, 0}};
        return zero;
    }
    
    ferret_u256 result = {{0, 0, 0, 0}};
    int word_shift = n / 64;
    int bit_shift = n % 64;
    
    for (int i = 0; i < 4; i++) {
        int src_idx = i - word_shift;
        if (src_idx >= 0 && src_idx < 4) {
            if (bit_shift == 0) {
                result.words[i] = a.words[src_idx];
            } else {
                result.words[i] = a.words[src_idx] << bit_shift;
                if (src_idx > 0) {
                    result.words[i] |= a.words[src_idx - 1] >> (64 - bit_shift);
                }
            }
        }
    }
    return result;
}

ferret_u256 ferret_u256_shr(ferret_u256 a, int n) {
    if (n <= 0) return a;
    if (n >= 256) {
        ferret_u256 zero = {{0, 0, 0, 0}};
        return zero;
    }
    
    ferret_u256 result = {{0, 0, 0, 0}};
    int word_shift = n / 64;
    int bit_shift = n % 64;
    
    for (int i = 0; i < 4; i++) {
        int src_idx = i + word_shift;
        if (src_idx < 4) {
            if (bit_shift == 0) {
                result.words[i] = a.words[src_idx];
            } else {
                result.words[i] = a.words[src_idx] >> bit_shift;
                if (src_idx < 3) {
                    result.words[i] |= a.words[src_idx + 1] << (64 - bit_shift);
                }
            }
        }
    }
    return result;
}

// String conversion functions (simplified implementations)
char* ferret_i128_to_string(ferret_i128 val) {
    char* buf = (char*)malloc(50);  // Enough for 128-bit number
    if (!buf) return NULL;
    
#ifdef FERRET_HAS_INT128
    snprintf(buf, 50, "%lld", (long long)val);
#else
    // Convert struct-based to string
    if (val.hi == 0 && val.lo < (1ULL << 63)) {
        snprintf(buf, 50, "%llu", (unsigned long long)val.lo);
    } else if (val.hi == -1 && (int64_t)val.lo < 0) {
        snprintf(buf, 50, "-%llu", (unsigned long long)-(int64_t)val.lo);
    } else {
        // For large numbers, use hex representation
        snprintf(buf, 50, "0x%016llx%016llx", (unsigned long long)val.hi, (unsigned long long)val.lo);
    }
#endif
    return buf;
}

char* ferret_u128_to_string(ferret_u128 val) {
    char* buf = (char*)malloc(50);
    if (!buf) return NULL;
    
#ifdef FERRET_HAS_INT128
    snprintf(buf, 50, "%llu", (unsigned long long)val);
#else
    if (val.hi == 0) {
        snprintf(buf, 50, "%llu", (unsigned long long)val.lo);
    } else {
        snprintf(buf, 50, "0x%016llx%016llx", (unsigned long long)val.hi, (unsigned long long)val.lo);
    }
#endif
    return buf;
}

char* ferret_i256_to_string(ferret_i256 val) {
    char* buf = (char*)malloc(100);
    if (!buf) return NULL;
    
    // Check if all words are zero
    bool all_zero = true;
    for (int i = 0; i < 4; i++) {
        if (val.words[i] != 0) {
            all_zero = false;
            break;
        }
    }
    if (all_zero) {
        snprintf(buf, 100, "0");
        return buf;
    }
    
    // For now, use hex representation for large numbers
    snprintf(buf, 100, "0x%016llx%016llx%016llx%016llx",
             (unsigned long long)val.words[3],
             (unsigned long long)val.words[2],
             (unsigned long long)val.words[1],
             (unsigned long long)val.words[0]);
    return buf;
}

char* ferret_u256_to_string(ferret_u256 val) {
    char* buf = (char*)malloc(100);
    if (!buf) return NULL;
    
    bool all_zero = true;
    for (int i = 0; i < 4; i++) {
        if (val.words[i] != 0) {
            all_zero = false;
            break;
        }
    }
    if (all_zero) {
        snprintf(buf, 100, "0");
        return buf;
    }
    
    snprintf(buf, 100, "0x%016llx%016llx%016llx%016llx",
             (unsigned long long)val.words[3],
             (unsigned long long)val.words[2],
             (unsigned long long)val.words[1],
             (unsigned long long)val.words[0]);
    return buf;
}

char* ferret_f128_to_string(ferret_f128 val) {
    char* buf = (char*)malloc(50);
    if (!buf) return NULL;
    
#ifdef FERRET_HAS_FLOAT128
    snprintf(buf, 50, "%.15g", (double)val);
#else
    double d = ferret_f128_to_f64(val);
    snprintf(buf, 50, "%.15g", d);
#endif
    return buf;
}

char* ferret_f256_to_string(ferret_f256 val) {
    char* buf = (char*)malloc(50);
    if (!buf) return NULL;
    
    double d = ferret_f256_to_f64(val);
    snprintf(buf, 50, "%.20g", d);
    return buf;
}

// String to number conversion (simplified - only handles decimal and hex)
ferret_i128 ferret_i128_from_string(const char* str) {
    if (!str) {
#ifdef FERRET_HAS_INT128
        return 0;
#else
        ferret_i128 zero = {0, 0};
        return zero;
#endif
    }
    
#ifdef FERRET_HAS_INT128
    return (ferret_i128)strtoll(str, NULL, 0);
#else
    ferret_i128 result = {0, 0};
    int64_t val = strtoll(str, NULL, 0);
    result.lo = (uint64_t)val;
    result.hi = val < 0 ? -1 : 0;
    return result;
#endif
}

ferret_u128 ferret_u128_from_string(const char* str) {
    if (!str) {
#ifdef FERRET_HAS_INT128
        return 0;
#else
        ferret_u128 zero = {0, 0};
        return zero;
#endif
    }
    
#ifdef FERRET_HAS_INT128
    return (ferret_u128)strtoull(str, NULL, 0);
#else
    ferret_u128 result = {0, 0};
    result.lo = strtoull(str, NULL, 0);
    return result;
#endif
}

ferret_i256 ferret_i256_from_string(const char* str) {
    ferret_i256 result = {{0, 0, 0, 0}};
    if (!str) return result;
    
    int64_t val = strtoll(str, NULL, 0);
    result.words[0] = (uint64_t)val;
    if (val < 0) {
        for (int i = 1; i < 4; i++) {
            result.words[i] = UINT64_MAX;
        }
    }
    return result;
}

ferret_u256 ferret_u256_from_string(const char* str) {
    ferret_u256 result = {{0, 0, 0, 0}};
    if (!str) return result;
    
    result.words[0] = strtoull(str, NULL, 0);
    return result;
}

ferret_f128 ferret_f128_from_string(const char* str) {
    if (!str) {
#ifdef FERRET_HAS_FLOAT128
        return 0.0;
#else
        ferret_f128 zero = {0, 0};
        return zero;
#endif
    }
    
    double val = strtod(str, NULL);
    return ferret_f128_from_f64(val);
}

ferret_f256 ferret_f256_from_string(const char* str) {
    ferret_f256 result = {{0, 0, 0}, 0};
    if (!str) return result;
    
    double val = strtod(str, NULL);
    return ferret_f256_from_f64(val);
}

void ferret_memcpy(void* dst, const void* src, uint64_t size) {
    if (!dst || !src || size == 0) {
        return;
    }
    memcpy(dst, src, (size_t)size);
}

#define FERRET_PTR_BIN_OP(type, func) \
    void func##_ptr(const type* a, const type* b, type* out) { \
        if (!out || !a || !b) return; \
        *out = func(*a, *b); \
    }

#define FERRET_PTR_CMP_OP(type, func) \
    bool func##_ptr(const type* a, const type* b) { \
        if (!a || !b) return false; \
        return func(*a, *b); \
    }

#define FERRET_PTR_UNARY_OP(type, func) \
    void func##_ptr(const type* a, type* out) { \
        if (!out || !a) return; \
        *out = func(*a); \
    }

FERRET_PTR_BIN_OP(ferret_i128, ferret_i128_add)
FERRET_PTR_BIN_OP(ferret_i128, ferret_i128_sub)
FERRET_PTR_BIN_OP(ferret_i128, ferret_i128_mul)
FERRET_PTR_BIN_OP(ferret_i128, ferret_i128_div)
FERRET_PTR_BIN_OP(ferret_i128, ferret_i128_mod)
FERRET_PTR_CMP_OP(ferret_i128, ferret_i128_eq)
FERRET_PTR_CMP_OP(ferret_i128, ferret_i128_lt)
FERRET_PTR_CMP_OP(ferret_i128, ferret_i128_gt)
FERRET_PTR_BIN_OP(ferret_i128, ferret_i128_and)
FERRET_PTR_BIN_OP(ferret_i128, ferret_i128_or)
FERRET_PTR_BIN_OP(ferret_i128, ferret_i128_xor)

FERRET_PTR_BIN_OP(ferret_u128, ferret_u128_add)
FERRET_PTR_BIN_OP(ferret_u128, ferret_u128_sub)
FERRET_PTR_BIN_OP(ferret_u128, ferret_u128_mul)
FERRET_PTR_BIN_OP(ferret_u128, ferret_u128_div)
FERRET_PTR_BIN_OP(ferret_u128, ferret_u128_mod)
FERRET_PTR_CMP_OP(ferret_u128, ferret_u128_eq)
FERRET_PTR_CMP_OP(ferret_u128, ferret_u128_lt)
FERRET_PTR_CMP_OP(ferret_u128, ferret_u128_gt)
FERRET_PTR_BIN_OP(ferret_u128, ferret_u128_and)
FERRET_PTR_BIN_OP(ferret_u128, ferret_u128_or)
FERRET_PTR_BIN_OP(ferret_u128, ferret_u128_xor)

FERRET_PTR_BIN_OP(ferret_i256, ferret_i256_add)
FERRET_PTR_BIN_OP(ferret_i256, ferret_i256_sub)
FERRET_PTR_BIN_OP(ferret_i256, ferret_i256_mul)
FERRET_PTR_BIN_OP(ferret_i256, ferret_i256_div)
FERRET_PTR_BIN_OP(ferret_i256, ferret_i256_mod)
FERRET_PTR_CMP_OP(ferret_i256, ferret_i256_eq)
FERRET_PTR_CMP_OP(ferret_i256, ferret_i256_lt)
FERRET_PTR_CMP_OP(ferret_i256, ferret_i256_gt)
FERRET_PTR_BIN_OP(ferret_i256, ferret_i256_and)
FERRET_PTR_BIN_OP(ferret_i256, ferret_i256_or)
FERRET_PTR_BIN_OP(ferret_i256, ferret_i256_xor)
FERRET_PTR_UNARY_OP(ferret_i256, ferret_i256_not)

FERRET_PTR_BIN_OP(ferret_u256, ferret_u256_add)
FERRET_PTR_BIN_OP(ferret_u256, ferret_u256_sub)
FERRET_PTR_BIN_OP(ferret_u256, ferret_u256_mul)
FERRET_PTR_BIN_OP(ferret_u256, ferret_u256_div)
FERRET_PTR_BIN_OP(ferret_u256, ferret_u256_mod)
FERRET_PTR_CMP_OP(ferret_u256, ferret_u256_eq)
FERRET_PTR_CMP_OP(ferret_u256, ferret_u256_lt)
FERRET_PTR_CMP_OP(ferret_u256, ferret_u256_gt)
FERRET_PTR_BIN_OP(ferret_u256, ferret_u256_and)
FERRET_PTR_BIN_OP(ferret_u256, ferret_u256_or)
FERRET_PTR_BIN_OP(ferret_u256, ferret_u256_xor)
FERRET_PTR_UNARY_OP(ferret_u256, ferret_u256_not)

FERRET_PTR_BIN_OP(ferret_f128, ferret_f128_add)
FERRET_PTR_BIN_OP(ferret_f128, ferret_f128_sub)
FERRET_PTR_BIN_OP(ferret_f128, ferret_f128_mul)
FERRET_PTR_BIN_OP(ferret_f128, ferret_f128_div)
FERRET_PTR_CMP_OP(ferret_f128, ferret_f128_eq)
FERRET_PTR_CMP_OP(ferret_f128, ferret_f128_lt)
FERRET_PTR_CMP_OP(ferret_f128, ferret_f128_gt)

FERRET_PTR_BIN_OP(ferret_f256, ferret_f256_add)
FERRET_PTR_BIN_OP(ferret_f256, ferret_f256_sub)
FERRET_PTR_BIN_OP(ferret_f256, ferret_f256_mul)
FERRET_PTR_BIN_OP(ferret_f256, ferret_f256_div)
FERRET_PTR_CMP_OP(ferret_f256, ferret_f256_eq)
FERRET_PTR_CMP_OP(ferret_f256, ferret_f256_lt)
FERRET_PTR_CMP_OP(ferret_f256, ferret_f256_gt)

void ferret_i128_from_i64_ptr(int64_t val, ferret_i128* out) {
    if (!out) return;
    *out = ferret_i128_from_i64(val);
}

void ferret_u128_from_u64_ptr(uint64_t val, ferret_u128* out) {
    if (!out) return;
    *out = ferret_u128_from_u64(val);
}

void ferret_i256_from_i64_ptr(int64_t val, ferret_i256* out) {
    if (!out) return;
    *out = ferret_i256_from_i64(val);
}

void ferret_u256_from_u64_ptr(uint64_t val, ferret_u256* out) {
    if (!out) return;
    *out = ferret_u256_from_u64(val);
}

void ferret_f128_from_f64_ptr(double val, ferret_f128* out) {
    if (!out) return;
    *out = ferret_f128_from_f64(val);
}

void ferret_f256_from_f64_ptr(double val, ferret_f256* out) {
    if (!out) return;
    *out = ferret_f256_from_f64(val);
}

int64_t ferret_i128_to_i64_ptr(const ferret_i128* val) {
    if (!val) return 0;
    return ferret_i128_to_i64(*val);
}

uint64_t ferret_u128_to_u64_ptr(const ferret_u128* val) {
    if (!val) return 0;
    return ferret_u128_to_u64(*val);
}

int64_t ferret_i256_to_i64_ptr(const ferret_i256* val) {
    if (!val) return 0;
    return ferret_i256_to_i64(*val);
}

uint64_t ferret_u256_to_u64_ptr(const ferret_u256* val) {
    if (!val) return 0;
    return ferret_u256_to_u64(*val);
}

double ferret_f128_to_f64_ptr(const ferret_f128* val) {
    if (!val) return 0.0;
    return ferret_f128_to_f64(*val);
}

double ferret_f256_to_f64_ptr(const ferret_f256* val) {
    if (!val) return 0.0;
    return ferret_f256_to_f64(*val);
}

char* ferret_i128_to_string_ptr(const ferret_i128* val) {
    if (!val) return NULL;
    return ferret_i128_to_string(*val);
}

char* ferret_u128_to_string_ptr(const ferret_u128* val) {
    if (!val) return NULL;
    return ferret_u128_to_string(*val);
}

char* ferret_i256_to_string_ptr(const ferret_i256* val) {
    if (!val) return NULL;
    return ferret_i256_to_string(*val);
}

char* ferret_u256_to_string_ptr(const ferret_u256* val) {
    if (!val) return NULL;
    return ferret_u256_to_string(*val);
}

char* ferret_f128_to_string_ptr(const ferret_f128* val) {
    if (!val) return NULL;
    return ferret_f128_to_string(*val);
}

char* ferret_f256_to_string_ptr(const ferret_f256* val) {
    if (!val) return NULL;
    return ferret_f256_to_string(*val);
}

void ferret_i128_from_string_ptr(const char* str, ferret_i128* out) {
    if (!out) return;
    *out = ferret_i128_from_string(str);
}

void ferret_u128_from_string_ptr(const char* str, ferret_u128* out) {
    if (!out) return;
    *out = ferret_u128_from_string(str);
}

void ferret_i256_from_string_ptr(const char* str, ferret_i256* out) {
    if (!out) return;
    *out = ferret_i256_from_string(str);
}

void ferret_u256_from_string_ptr(const char* str, ferret_u256* out) {
    if (!out) return;
    *out = ferret_u256_from_string(str);
}

void ferret_f128_from_string_ptr(const char* str, ferret_f128* out) {
    if (!out) return;
    *out = ferret_f128_from_string(str);
}

void ferret_f256_from_string_ptr(const char* str, ferret_f256* out) {
    if (!out) return;
    *out = ferret_f256_from_string(str);
}

#undef FERRET_PTR_BIN_OP
#undef FERRET_PTR_CMP_OP
#undef FERRET_PTR_UNARY_OP
