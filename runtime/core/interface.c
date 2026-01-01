// Ferret runtime: Interface support
// VTable-based interface implementation (similar to Go)
// This file is minimal - most interface code is generated inline

#include <stdint.h>
#include <stdbool.h>

// Interface value structure
// Stores both the concrete data pointer and vtable pointer
typedef struct {
    void* data;      // Pointer to concrete type instance
    void** vtable;   // Pointer to function pointer table (vtable)
} ferret_interface_t;

// This file is mostly a placeholder
// VTable structs and instances are generated inline in the main C file
// for each interface/implementation pair
