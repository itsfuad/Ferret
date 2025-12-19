#ifndef HELLO_HELLO_MAIN_H
#define HELLO_HELLO_MAIN_H

#include <stdint.h>
#include <stdbool.h>

#include "io.h"
#include "interface.h"
#include "map.h"
#include "bigint.h"
#include "optional.h"

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

#endif // HELLO_HELLO_MAIN_H
