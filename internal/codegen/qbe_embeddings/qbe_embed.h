#ifndef FERRET_QBE_EMBED_H
#define FERRET_QBE_EMBED_H

#include <setjmp.h>

void qbe_exit(int code);
int ferret_qbe_run(int argc, char **argv);

#endif
