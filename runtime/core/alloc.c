#include "alloc.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

void *ferret_alloc(uint64_t size) {
    return malloc((size_t)size);
}

// Union printing (placeholder)
void ferret_io_Println_union(void* u) {
    printf("<union>\n");
}
