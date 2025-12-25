#ifndef QBE_CONFIG_H
#define QBE_CONFIG_H

#if defined(__APPLE__)
#define Defasm Gasmacho
#define Deftgt T_amd64_sysv
#else
#define Defasm Gaself
#if defined(_WIN32)
#define Deftgt T_amd64_win64
#elif defined(__aarch64__)
#define Deftgt T_arm64
#else
#define Deftgt T_amd64_sysv
#endif
#endif

#endif
