// Ferret runtime: Math functions
#include <math.h>

// Power function - wrapper around C's pow()
// Takes two f64 values and returns f64
double ferret_pow(double base, double exp) {
    return pow(base, exp);
}
