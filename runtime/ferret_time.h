#ifndef FERRET_TIME_H
#define FERRET_TIME_H

#include <stdint.h>

char* ferret_time_Now(void);
int64_t ferret_time_NowUnix(void);
int64_t ferret_time_NowUnixMs(void);
void ferret_time_SleepMs(int32_t ms);

#endif
