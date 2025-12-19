#ifndef FERRET_OPTIONAL_H
#define FERRET_OPTIONAL_H

#include <stdint.h>
#include <stdbool.h>

#include "interface.h"
#include "optional.h"

typedef struct {
    int32_t value;
    int is_some;
} ferret_optional_int32_t;
static const ferret_optional_int32_t none = ((ferret_optional_int32_t){.value = 0, .is_some = 0});

int main();

#endif // FERRET_OPTIONAL_H
