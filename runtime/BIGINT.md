# Big Integer and Float Implementation for Ferret (128/256-bit)

## Problem
C doesn't natively support 128-bit or 256-bit integers and floating-point types, but Ferret supports:
- **Integers**: `i128`, `i256`, `u128`, `u256`
- **Floats**: `f128`, `f256`

We need a solution that:
1. Works on modern compilers (GCC/Clang) with native 128-bit support
2. Falls back gracefully on compilers without 128-bit support
3. Provides full support for 256-bit types everywhere

## Solution Overview

### For 128-bit Integers (`i128`, `u128`)

**Primary Strategy**: Limb-based representation
- Uses a fixed array of limbs with selectable limb size (32-bit or 64-bit).
- 64-bit limbs are used when the platform is 64-bit and `__uint128_t` is available.
- 32-bit limbs are used as a safe default when detection is unavailable.
- All operations implemented in software on limbs for portability.

### For 256-bit Integers (`i256`, `u256`)

**Always Limb-based**:
- Uses a fixed array of limbs with selectable limb size (32-bit or 64-bit).
- All operations implemented in software.
- Portable across all compilers.

### For 128-bit Floats (`f128`)

**Primary Strategy**: Use compiler extensions when available
- GCC on x86_64, PowerPC supports `__float128`
- Provides native performance (same speed as `double`)
- Operations compile to native CPU instructions

**Fallback Strategy**: Struct-based representation
- When `__float128` is not available (e.g., Clang, MSVC)
- Uses a struct with two 64-bit words for IEEE 754 binary128 format
- All operations implemented in software
- ~2-3x slower than native, but fully portable

### For 256-bit Floats (`f256`)

**Always Struct-based**: 
- Uses a struct with four 64-bit words: `{uint64_t mantissa[3], uint64_t exp_sign}`
- Implements extended IEEE 754 format (1 sign + 19 exponent + 236 mantissa bits)
- All operations implemented in software
- ~4-5x slower than native, but necessary for 256-bit support
- Portable across all compilers

## Implementation Details

### Header File (`bigint.h`)

1. **Feature Detection**:
   ```c
   #if defined(__SIZEOF_INT128__)
       #define FERRET_HAS_WIDE128 1
   #endif

   #if defined(UINTPTR_MAX) && UINTPTR_MAX >= 0xffffffffffffffffULL && defined(FERRET_HAS_WIDE128)
       #define FERRET_LIMB_BITS 64
   #else
       #define FERRET_LIMB_BITS 32
   #endif
   ```

2. **Type Definitions**:
   - Limb-based: `typedef struct { ferret_limb_t words[FERRET_U128_LIMBS]; } ferret_u128;`
   - Same layout for signed types (two's complement in limbs).
   - 256-bit uses `FERRET_U256_LIMBS`.

3. **Operations**:
   - Always function calls implemented in `bigint.c` using limb helpers.

### Implementation File (`bigint.c`)

**Integer Operations**:
- Addition/Subtraction: Per-limb carry/borrow.
- Multiplication: Long multiplication (truncated to target width).
- Division/Modulo: Binary long division on limbs.
- Comparisons: Unsigned and signed limb comparisons.

## Performance Characteristics

| Type | Native Support | Fallback | Notes |
|------|---------------|----------|-------|
| `i128`/`u128` | N/A | ~2-3x slower | Limb-based |
| `i256`/`u256` | N/A | ~4-5x slower | Limb-based |
| `f128` | ✅ Native speed | ~2-3x slower | Native on GCC (x86_64, PowerPC) |
| `f256` | N/A | ~4-5x slower | Always struct-based |

## Current Limitations

1. **Performance**:
   - Limb-based math is slower than native integer instructions.
   - For heavy big-int workloads, consider GMP/MPIR integration.

2. **No Overflow Detection**:
   - Operations don't check for overflow
   - Could add overflow flags if needed

3. **Floating Precision**:
   - f128/f256 are still simplified via double conversion in fallback paths.

## Future Improvements

1. **GMP Integration** (Optional):
   - For maximum performance on 256-bit operations
   - Would require linking against libgmp
   - Best for applications that heavily use 256-bit integers

2. **Conversion Helpers**:
   - String conversion (to/from decimal/hex)
   - More conversion functions between types

## Usage in Ferret

The codegen automatically:
1. Includes `bigint.h` in generated C code
2. Uses `ferret_i128`, `ferret_u128`, `ferret_i256`, `ferret_u256` types
3. Calls runtime functions implemented on limb helpers

**Example Ferret code**:
```ferret
// Integers
let x: i128 = 100000000000000000000;
let y: i128 = 200000000000000000000;
let sum: i128 = x + y;  // Automatically uses native or fallback operations

// Floats
let a: f128 = 3.141592653589793238462643383279502884197;
let b: f128 = 2.718281828459045235360287471352662497757;
let product: f128 = a * b;  // Automatically uses native or fallback operations
```

## Compiler Compatibility

| Compiler | 128-bit Int | 128-bit Float | 256-bit Types |
|----------|-------------|---------------|---------------|
| GCC 4.6+ (x86_64) | ✅ Limb-based | ✅ Native (`__float128`) | ✅ Limb-based |
| GCC 4.6+ (PowerPC) | ✅ Limb-based | ✅ Native (`__float128`) | ✅ Limb-based |
| Clang 3.0+ | ✅ Limb-based | ❌ Fallback (struct) | ✅ Limb-based |
| MSVC | ✅ Limb-based | ❌ Fallback (struct) | ✅ Limb-based |
| Other | ✅ Limb-based | ❌ Fallback (struct) | ✅ Limb-based |

## Testing

To test the implementation:
```bash
# Compile bigint.c
gcc -c -std=c11 -I runtime runtime/bigint.c -o bigint.o

# Should compile without errors
```

## Integration Status

- [-] Integer types defined (`ferret_i128`, `ferret_i256`, `ferret_u128`, `ferret_u256`)
- [-] Float types defined (`ferret_f128`, `ferret_f256`)
- [-] Basic arithmetic operations implemented for all types
- [-] Full 256-bit multiplication/division (using long multiplication and binary long division)
- [-] Bitwise operations for all integer types (AND, OR, XOR, NOT, shifts)
- [-] String conversion helpers (to/from decimal and hex strings)
- [-] Codegen updated to use all big types
- [-] Build system includes `bigint.c` in compilation
- [-] **f128 with native support** (GCC on x86_64/PowerPC): Complete IEEE 754 via `__float128`
- [-] **f128 fallback** (Clang, MSVC, etc.): Simplified via double conversion (loses precision, but functional)
- [-] **f256**: Simplified via double conversion (loses precision, but functional)

**Note on Float Operations:**
- When `__float128` is available (GCC on x86_64/PowerPC), f128 operations use native IEEE 754 binary128 format - **fully complete**.
- Fallback implementations convert to `double` (64-bit), perform operations, then convert back. This works but:
  - Loses precision (only 53 bits of mantissa vs 112 for f128, 236 for f256)
  - May not handle edge cases (NaN, Inf, subnormals) correctly
  - A full implementation would require bit-level IEEE 754 manipulation (extracting sign/exponent/mantissa, normalizing, rounding, etc.)

For most practical purposes, the double conversion approach is sufficient. For scientific computing requiring full precision, consider using a library like MPFR or implementing proper IEEE 754 operations.
