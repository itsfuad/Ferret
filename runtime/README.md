# Ferret Runtime Libraries

This directory contains optimized C runtime libraries used by generated Ferret code.

## Available Libraries

### 1. `io.c` / `io.h` - I/O Operations
**Status**: ✅ Required, already in use
- Basic print/println functions for all types
- Input functions (ReadInt, ReadFloat)
- String concatenation helper

**Performance**: Good for basic I/O. No changes needed.

---

### 2. `interface.c` / `interface.h` - Interface Support
**Status**: ✅ Required, already in use
- Interface value structure (`ferret_interface_t`)
- VTable-based dynamic dispatch

**Performance**: Already optimized with inline wrappers in codegen. No changes needed.

---

### 3. `array.c` / `array.h` - Dynamic Array Library
**Status**: ✅ Created, ready for use
**When to use**: When Ferret supports dynamic arrays with append/resize operations

**Features**:
- Growable arrays with exponential growth (amortized O(1) append)
- Similar to Go slices or C++ vectors
- Efficient memory management

**API**:
```c
ferret_array_t* arr = ferret_array_new(sizeof(int32_t), 4);
int32_t val = 42;
ferret_array_append(arr, &val);
int32_t* elem = (int32_t*)ferret_array_get(arr, 0);
ferret_array_free(arr);
```

**Performance**: Much faster than repeated malloc/realloc for growing arrays.

---

### 4. `string_builder.c` / `string_builder.h` - String Builder Library
**Status**: ✅ Created, ready for use
**When to use**: For efficient string concatenation (better than `ferret_io_ConcatStrings`)

**Features**:
- Efficient string concatenation with exponential growth
- Avoids repeated malloc calls
- Similar to Go's `strings.Builder`

**API**:
```c
ferret_string_builder_t* sb = ferret_string_builder_new(16);
ferret_string_builder_append(sb, "Hello");
ferret_string_builder_append(sb, " World");
char* result = ferret_string_builder_string(sb); // Caller must free
ferret_string_builder_destroy(sb);
```

**Performance**: Much faster than `ferret_io_ConcatStrings` for multiple concatenations.

---

## Current Status

### What's Working:
- ✅ `io.c` - Basic I/O (required)
- ✅ `interface.c` - Interface support (required)
- ✅ `array.c` - Dynamic arrays (optional, ready)
- ✅ `string_builder.c` - String builder (optional, ready)

### Integration Status:
- ✅ All libraries compile successfully
- ✅ Build system includes all runtime files
- ⏳ Codegen doesn't use array/string_builder yet (they're ready when needed)

### When to Use These Libraries:

**Use `array.c` when**:
- Ferret supports dynamic arrays with `append()` operation
- Arrays need to grow at runtime
- Better performance than fixed-size arrays for dynamic data

**Use `string_builder.c` when**:
- Multiple string concatenations in a loop
- Building strings dynamically
- Better performance than `ferret_io_ConcatStrings` for repeated operations

**Current code is fine for**:
- Basic I/O operations
- Interface dispatch (already optimized)
- Fixed-size arrays (current implementation)

---

## Performance Benefits

1. **Dynamic Arrays**: Exponential growth strategy means O(1) amortized append vs O(n) for naive realloc
2. **String Builder**: Single allocation for final string vs N allocations for N concatenations
3. **Interface Wrappers**: Already using `inline` for zero-overhead dispatch

These libraries are ready to use when Ferret's language features require them!
