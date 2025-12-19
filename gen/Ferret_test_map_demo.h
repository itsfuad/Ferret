#ifndef FERRET_TEST_MAP_DEMO_H
#define FERRET_TEST_MAP_DEMO_H

#include <stdint.h>
#include <stdbool.h>

#include "interface.h"
#include "optional.h"
#include "map.h"

typedef struct {
    const char* value;
    int is_some;
} ferret_optional_const_char_ptr;
static const ferret_optional_const_char_ptr none = ((ferret_optional_const_char_ptr){.value = 0, .is_some = 0});

int main();

#endif // FERRET_TEST_MAP_DEMO_H
