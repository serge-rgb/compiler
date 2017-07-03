
#define PRI_size "li"

int raise(int sig);

#if defined(__linux__)
#define SIGINT 5
#endif

extern int    snprintf( char * buffer, unsigned long bufsz, const char * format, ... );
extern int    vsnprintf( char * buffer, unsigned long bufsz, const char * format, va_list vlist );

#define PlatformAssert(expr) do { if (!(expr)) { __builtin_trap(); } } while(0)
#define PlatformPrintString(...) snprintf(__VA_ARGS__)
#define PlatformBreakHere __builtin_trap()
