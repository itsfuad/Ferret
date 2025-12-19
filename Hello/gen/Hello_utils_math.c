// Generated C code from Ferret
// Module: Hello/utils/math

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
#include "Hello_utils_math.h"

int32_t Hello_utils_math_Factorial(int32_t n) {
    if (n <= 1) {
        return 1;
    } else {
        return n * Hello_utils_math_Factorial(n - 1);
    }
}

float Hello_utils_math_Pow(float base, float exponent) {
    return pow((double)(base), (double)(exponent));
}

