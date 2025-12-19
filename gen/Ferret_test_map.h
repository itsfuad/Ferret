#ifndef FERRET_TEST_MAP_H
#define FERRET_TEST_MAP_H

#include <stdint.h>
#include <stdbool.h>

#include "interface.h"
#include "optional.h"
#include "map.h"

typedef struct {
    int32_t value;
    int is_some;
} ferret_optional_int32_t;
static const ferret_optional_int32_t none = ((ferret_optional_int32_t){.value = 0, .is_some = 0});

int main();

#endif // FERRET_TEST_MAP_H
