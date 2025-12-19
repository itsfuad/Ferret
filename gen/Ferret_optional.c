// Generated C code from Ferret
// Module: Ferret/optional

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#include "io.h"
#include "interface.h"
#include "optional.h"
#include "Ferret_optional.h"

int main() {
    ferret_optional_int32_t opt = ((ferret_optional_int32_t){.value = 10, .is_some = 1});
    int32_t aa = (opt.is_some ? opt.value : 1) + 2;
    ferret_optional_int32_t a = opt;
    if (a.is_some != 0) {
                char __print_buf_1[1024]; snprintf(__print_buf_1, sizeof(__print_buf_1), "%s%d", "A is not none, a: ", a.value); ferret_io_Println(__print_buf_1);
;
    } else {
                char __print_buf_2[1024]; snprintf(__print_buf_2, sizeof(__print_buf_2), "%s%s", "A is none, a: ", "none"); ferret_io_Println(__print_buf_2);
;
    }
        char __print_buf_3[1024];     char __opt_val_3_1[64];     if (opt.is_some) { snprintf(__opt_val_3_1, sizeof(__opt_val_3_1), "%d", opt.value); } else { strcpy(__opt_val_3_1, "none"); }
snprintf(__print_buf_3, sizeof(__print_buf_3), "%s%s", "opt value: ", __opt_val_3_1); ferret_io_Println(__print_buf_3);
;
    return 0;
}

