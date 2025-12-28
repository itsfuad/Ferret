#include "random.h"

#include <stdlib.h>
#include <time.h>

float ferret_random_Random(void) {
    static int seeded = 0;
    if (!seeded) {
        seeded = 1;
        srand((unsigned int)time(NULL));
    }
    return (float)rand() / (float)RAND_MAX;
}
