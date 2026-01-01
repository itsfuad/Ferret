// Ferret runtime: Interface support
// VTable-based interface implementation (similar to Go)

#ifndef FERRET_INTERFACE_H
#define FERRET_INTERFACE_H

#include <stdint.h>
#include <stdbool.h>

// Interface value structure
// Stores both the concrete data pointer and vtable pointer
typedef struct {
    void* data;      // Pointer to concrete type instance
    void** vtable;   // Pointer to function pointer table (vtable)
} ferret_interface_t;

#endif // FERRET_INTERFACE_H
