#ifndef FERRET_TEST_H
#define FERRET_TEST_H

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

typedef struct {
    const char* value;
    int is_some;
} ferret_optional_const_char_ptr;

typedef struct {
  int32_t X;
  int32_t Y;
} Point;

typedef struct {
  int32_t (*Area)(void*);
} Shape_vtable;

typedef struct {
  int32_t Radius;
} Circle;

typedef struct {
  int32_t Width;
  int32_t Height;
} Rectangle;

void Point_MoveBy(Point* self, int32_t dx, int32_t dy);
int32_t Circle_Area(Circle* self);
int32_t Rectangle_Area(Rectangle* self);
float add(float x, float y);
void PrintArea(ferret_interface_t s);
int main();

#endif // FERRET_TEST_H
