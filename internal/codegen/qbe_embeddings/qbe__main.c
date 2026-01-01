#include "qbe_embed.h"

#include <getopt.h>
#include <setjmp.h>

static jmp_buf qbe_jmp_buf;
static int qbe_exit_code;

#define exit qbe_exit
#define main qbe_main
#include "../../../qbe/main.c"
#undef main
#undef exit

void qbe_exit(int code) {
	qbe_exit_code = code;
	if (outf) {
		fflush(outf);
		if (outf != stdout && outf != stderr) {
			fclose(outf);
		}
	}
	longjmp(qbe_jmp_buf, 1);
}

int ferret_qbe_run(int argc, char **argv) {
	qbe_exit_code = 0;
	if (setjmp(qbe_jmp_buf) != 0) {
		return qbe_exit_code;
	}
	optind = 1;
	qbe_main(argc, argv);
	return qbe_exit_code;
}

// Union printing runtime (generic placeholder)
void ferret_io_Println_union(void* u) {
    printf("<union>\n");
}

// Specific for Com union (demo)
#include <stdint.h>
typedef struct {
    int32_t tag;
    union {
        int32_t i32_val;
        char* str_val;
        _Bool bool_val;
    } data;
} ComUnion;

void ferret_io_Println_Com(void* u) {
    ComUnion* com = (ComUnion*)u;
    switch (com->tag) {
        case 0:
            printf("%d\n", com->data.i32_val);
            break;
        case 1:
            printf("%s\n", com->data.str_val);
            break;
        case 2:
            printf("%s\n", com->data.bool_val ? "true" : "false");
            break;
        default:
            printf("<unknown>\n");
    }
}
