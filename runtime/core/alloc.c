#include "alloc.h"

#include <stdlib.h>

void *ferret_alloc(uint64_t size) {
    return malloc((size_t)size);
}
