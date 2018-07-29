#define PlatformAssert(expr) do { if (!(expr)) { \
                                                printf("Assertion failed: %s\n", #expr);\
                                                __builtin_trap(); } } while(0)

#define PlatformPrintString(...) snprintf(__VA_ARGS__)
#define PlatformBreak __asm ("int $3")