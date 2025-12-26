// Ferret runtime: Panic helper

#include "panic.h"

#include <stdio.h>
#include <stdlib.h>

void ferret_panic(const char* msg) {
    if (msg != NULL && msg[0] != '\0') {
        fprintf(stderr, "panic: %s\n", msg);
    } else {
        fputs("panic\n", stderr);
    }
    fflush(stderr);
    abort();
}
