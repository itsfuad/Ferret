// Ferret runtime: Optional helpers
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

// Unwrap optional into out buffer using default when none.
// Layout: value bytes followed by 1-byte is_some flag at offset val_size.
void ferret_optional_unwrap_or(const void* opt, const void* default_val, void* out, uint64_t val_size) {
    if (out == NULL) {
        return;
    }

    uint8_t* out_bytes = (uint8_t*)out;
    if (val_size == 0) {
        return;
    }

    if (opt == NULL) {
        if (default_val != NULL) {
            memcpy(out_bytes, default_val, (size_t)val_size);
        } else {
            memset(out_bytes, 0, (size_t)val_size);
        }
        return;
    }

    const uint8_t* opt_bytes = (const uint8_t*)opt;
    const uint8_t* flag_ptr = opt_bytes + val_size;
    if (*flag_ptr) {
        memcpy(out_bytes, opt_bytes, (size_t)val_size);
    } else if (default_val != NULL) {
        memcpy(out_bytes, default_val, (size_t)val_size);
    } else {
        memset(out_bytes, 0, (size_t)val_size);
    }
}
