// Generated C code from Ferret
// Entry module: test_project/test_project/start

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#include "io.h"


// Functions from test_project/test_project/start
int main() {
        ferret_io_Println("Hello");
    int32_t i;
    int32_t v;
    for (int32_t i = 0; i < 10; i += 1) {
        int32_t v = i;
                char __print_buf_1[1024]; snprintf(__print_buf_1, sizeof(__print_buf_1), "%d%s%d", i, " : ", v + 1); ferret_io_Println(__print_buf_1);
    }
    const char** fruits = (const char*[3]){"apple", "banana", "cherry"};
    for (int32_t i = 0; i < 3; i++) {
        const char* fruit = fruits[i];
                char __print_buf_3[1024]; snprintf(__print_buf_3, sizeof(__print_buf_3), "%s%s%s%d", "Fruit: ", fruit, " in index: ", i); ferret_io_Println(__print_buf_3);
    }
    return 0;
}


