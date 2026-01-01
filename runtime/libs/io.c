// Ferret runtime: IO functions
// Native implementations for std/io module

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include "../core/array.h"

// Printable union layout: [4-byte tag][12 bytes padding/data] = 16 bytes total
// Tags: 0=i8, 1=i16, 2=i32, 3=i64, 4=u8, 5=u16, 6=u32, 7=u64, 8=f32, 9=f64, 10=str, 11=byte, 12=bool

static void print_union(const void* union_ptr) {
    int32_t tag = *(int32_t*)union_ptr;
    const uint8_t* data = (const uint8_t*)union_ptr + 4;
    
    switch (tag) {
        case 0: printf("%d", *(int8_t*)data); break;      // i8
        case 1: printf("%d", *(int16_t*)data); break;     // i16
        case 2: printf("%d", *(int32_t*)data); break;     // i32
        case 3: printf("%ld", *(int64_t*)data); break;    // i64
        case 4: printf("%u", *(uint8_t*)data); break;     // u8
        case 5: printf("%u", *(uint16_t*)data); break;    // u16
        case 6: printf("%u", *(uint32_t*)data); break;    // u32
        case 7: printf("%lu", *(uint64_t*)data); break;   // u64
        case 8: printf("%.6g", *(float*)data); break;     // f32
        case 9: printf("%.15g", *(double*)data); break;   // f64
        case 10: {  // str
            const char* str = *(const char**)data;
            printf("%s", str ? str : "(null)");
            break;
        }
        case 11: printf("%c", *(uint8_t*)data); break;    // byte
        case 12: printf("%s", *(bool*)data ? "true" : "false"); break; // bool
        default: printf("<invalid union tag %d>", tag); break;
    }
}

// Naming convention: std_io_Print -> ferret_std_io_Print
// Slice layout: { void* ptr; int32_t len; int32_t cap } = 16 bytes (with padding)
void ferret_std_io_Print(void* slice_ptr) {
    if (!slice_ptr) {
        return;
    }
    
    ferret_array_t* arr = (ferret_array_t*)slice_ptr;
    uint8_t* current = (uint8_t*)arr->data;
    
    for (int32_t i = 0; i < arr->length; i++) {
        if (i > 0) printf(" ");
        print_union(current);
        current += arr->elem_size;
    }
}

void ferret_std_io_Println(void* slice_ptr) {
    ferret_std_io_Print(slice_ptr);
    printf("\n");
}
