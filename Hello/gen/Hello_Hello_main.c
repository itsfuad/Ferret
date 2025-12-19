// Generated C code from Ferret
// Module: Hello/Hello/main

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#include "io.h"
#include "interface.h"
#include "map.h"
#include "bigint.h"
#include "optional.h"
#include "Hello_rel.h"
#include "Hello_utils_math.h"
#include "Hello_Hello_main.h"


// Interface wrapper functions and vtable instances
static inline int32_t Circle_Area_Shape_wrapper(void* self) {
    return Circle_Area((Circle*)self);
}

static const Shape_vtable Circle_Shape_vtable_inst = {.Area = Circle_Area_Shape_wrapper};
static Shape_vtable* Circle_Shape_vtable_inst_ptr = (void*)&Circle_Shape_vtable_inst;
static inline int32_t Rectangle_Area_Shape_wrapper(void* self) {
    return Rectangle_Area((Rectangle*)self);
}

static const Shape_vtable Rectangle_Shape_vtable_inst = {.Area = Rectangle_Area_Shape_wrapper};
static Shape_vtable* Rectangle_Shape_vtable_inst_ptr = (void*)&Rectangle_Shape_vtable_inst;

void Point_MoveBy(Point* p, int32_t dx, int32_t dy) {
    char __print_buf_1[1024];
    snprintf(__print_buf_1, sizeof(__print_buf_1), "%s%d%s%d%s", "Moving point by (", dx, ", ", dy, ")");
    ferret_io_Println(__print_buf_1);
    p->X += dx;
    p->Y += dy;
}

int32_t Circle_Area(Circle* c) {
    return 3 * c->Radius * c->Radius;
}

int32_t Rectangle_Area(Rectangle* r) {
    return r->Width * r->Height;
}

float add(float x, float y) {
    return x + y;
}

void PrintArea(ferret_interface_t s) {
    int32_t area = ((Shape_vtable*)*s.vtable)->Area(s.data);
    char __print_buf_2[1024];
    snprintf(__print_buf_2, sizeof(__print_buf_2), "%s%d", "Area: ", area);
    ferret_io_Println(__print_buf_2);
}

int main() {
    for (int32_t i = 0; i <= (15 - 10); i += 1) {
        int32_t v = 10 + i;
        char __print_buf_3[1024];
        snprintf(__print_buf_3, sizeof(__print_buf_3), "%s%d%s%d", "Hello, World ", i, ": ", v);
        ferret_io_Println(__print_buf_3);
    }
    for (int32_t i = 0; i < 6; i++) {
        int32_t v = (int32_t[6]){10, 11, 12, 13, 14, 15}[i];
        char __print_buf_5[1024];
        snprintf(__print_buf_5, sizeof(__print_buf_5), "%s%d%s%d", "Hello again, World ", i, ": ", v);
        ferret_io_Println(__print_buf_5);
    }
    const char** fruits = (const char*[3]){"apple", "banana", "cherry"};
    for (int32_t __loop_idx_6 = 0; __loop_idx_6 < 3; __loop_idx_6++) {
        const char* fruit = fruits[__loop_idx_6];
        char __print_buf_8[1024];
        snprintf(__print_buf_8, sizeof(__print_buf_8), "%s%s", "I like ", fruit);
        ferret_io_Println(__print_buf_8);
    }
    for (int32_t i = 0; i < 3; i++) {
        const char* fruit = fruits[i];
        char __print_buf_10[1024];
        snprintf(__print_buf_10, sizeof(__print_buf_10), "%s%d%s%s", "Fruit ", i, ": ", fruit);
        ferret_io_Println(__print_buf_10);
    }
    int32_t* numbers = (int32_t[5]){1, 2, 3, 4, 5};
    for (int32_t i = 0; i < 5; i++) {
        int32_t num = numbers[i];
        char __print_buf_12[1024];
        snprintf(__print_buf_12, sizeof(__print_buf_12), "%s%d%s%d", "Number ", i, ": ", num);
        ferret_io_Println(__print_buf_12);
    }
    int32_t* arr = (int32_t[0]){};
    for (int32_t __loop_idx_13 = 0; __loop_idx_13 < 0; __loop_idx_13++) {
        int32_t i;
        ferret_io_Println("This will not be printed");
    }
    int32_t counter = 0;
    while (counter < 5) {
        char __print_buf_15[1024];
        snprintf(__print_buf_15, sizeof(__print_buf_15), "%s%d", "Counter is at: ", counter);
        ferret_io_Println(__print_buf_15);
        counter++;
    }
    Point p = (Point){.X = 3, .Y = 4};
    Point_MoveBy(&p, 2, 3);
    char __print_buf_16[1024];
    snprintf(__print_buf_16, sizeof(__print_buf_16), "%s%d%s%d%s", "Point coordinates: (", p.X, ", ", p.Y, ")");
    ferret_io_Println(__print_buf_16);
    int32_t n = 42;
    int32_t n2 = (int32_t)(n);
    char __print_buf_17[1024];
    snprintf(__print_buf_17, sizeof(__print_buf_17), "%s%d", "Interger n: ", n);
    ferret_io_Println(__print_buf_17);
    char __print_buf_18[1024];
    snprintf(__print_buf_18, sizeof(__print_buf_18), "%s%d", "Converted to i32 n2: ", n2);
    ferret_io_Println(__print_buf_18);
    Circle c = (Circle){.Radius = 5};
    char __print_buf_19[1024];
    snprintf(__print_buf_19, sizeof(__print_buf_19), "%s%d", "Circle area: ", Circle_Area(&c));
    ferret_io_Println(__print_buf_19);
    Rectangle r = (Rectangle){.Width = 4, .Height = 6};
    char __print_buf_20[1024];
    snprintf(__print_buf_20, sizeof(__print_buf_20), "%s%d", "Rectangle area: ", Rectangle_Area(&r));
    ferret_io_Println(__print_buf_20);
    char __print_buf_21[1024];
    snprintf(__print_buf_21, sizeof(__print_buf_21), "%s%.6g", "Sum of 3.14 and 2.71: ", add(3.14, 2.71));
    ferret_io_Println(__print_buf_21);
    ferret_interface_t shape1;
    Circle __temp_22 = c;
    shape1 = (ferret_interface_t) { .data = &__temp_22, .vtable = (void**)&Circle_Shape_vtable_inst_ptr };
    ferret_interface_t shape2;
    Rectangle __temp_23 = r;
    shape2 = (ferret_interface_t) { .data = &__temp_23, .vtable = (void**)&Rectangle_Shape_vtable_inst_ptr };
    PrintArea(shape1);
    PrintArea(shape2);
    char __print_buf_24[1024];
    snprintf(__print_buf_24, sizeof(__print_buf_24), "%s%.15g", "Value of PI from rel module: ", Hello_rel_PI);
    ferret_io_Println(__print_buf_24);
    if (Hello_rel_PI > 3.0) {
        ferret_io_Println("PI is greater than 3.0");
    } else {
        ferret_io_Println("PI is not greater than 3.0");
    }
    char __print_buf_25[1024];
    snprintf(__print_buf_25, sizeof(__print_buf_25), "%s%.6g", "2 raised to the power of 3 is: ", Hello_utils_math_Pow(2.0, 3.0));
    ferret_io_Println(__print_buf_25);
    char __print_buf_26[1024];
    snprintf(__print_buf_26, sizeof(__print_buf_26), "%s%d", "Factorial of 5 is: ", Hello_utils_math_Factorial(5));
    ferret_io_Println(__print_buf_26);
    ferret_optional_int32_t maybeVal = ((ferret_optional_int32_t){.value = 10, .is_some = 1});
    int32_t sum = (maybeVal.is_some ? maybeVal.value : 5) + 10;
    char __print_buf_27[1024];
    snprintf(__print_buf_27, sizeof(__print_buf_27), "%s%d", "Sum value: ", sum);
    ferret_io_Println(__print_buf_27);
    char __print_buf_28[1024];
    char __opt_val_28_1[64];
    if (maybeVal.is_some) {
        snprintf(__opt_val_28_1, sizeof(__opt_val_28_1), "%d", maybeVal.value);
    } else {
        strcpy(__opt_val_28_1, "none");
    }
    snprintf(__print_buf_28, sizeof(__print_buf_28), "%s%s", "Maybe value: ", __opt_val_28_1);
    ferret_io_Println(__print_buf_28);
    if (maybeVal.is_some == 0) {
        ferret_io_Println("maybeVal is none");
    } else {
        ferret_io_Println("maybeVal has a value");
        int32_t val = maybeVal.value + 100;
        char __print_buf_29[1024];
        snprintf(__print_buf_29, sizeof(__print_buf_29), "%s%d", "Value inside maybeVal plus 100: ", val);
        ferret_io_Println(__print_buf_29);
    }
    return 0;
}

