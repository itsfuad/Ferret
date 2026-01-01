// Ferret runtime: Random number generation

#include <stdlib.h>
#include <time.h>

// Generate a random float between 0.0 and 1.0
float ferret_random_Random(void) {
    static int seeded = 0;
    if (!seeded) {
        seeded = 1;
        srand((unsigned int)time(NULL));
    }
    return (float)rand() / (float)RAND_MAX;
}
