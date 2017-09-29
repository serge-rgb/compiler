
#define PRI_size "li"

int raise(int sig);

#if defined(__linux__)
#define SIGINT 5
#define PLATFORM_FORMAT_I64 "ld"
#define PLATFORM_FORMAT_U64 "lu"
#elif defined(__MACH__)
#define PLATFORM_FORMAT_I64 "lld"
#define PLATFORM_FORMAT_U64 "llu"
#else
#error Uknown UNIX platform
asdfasd
#endif

extern int    snprintf( char * buffer, unsigned long bufsz, const char * format, ... );
extern int    vsnprintf( char * buffer, unsigned long bufsz, const char * format, va_list vlist );


#define PlatformAssert(expr) do { if (!(expr)) { __builtin_trap(); } } while(0)
#define PlatformPrintString(...) snprintf(__VA_ARGS__)
#define PlatformBreakHere __asm ("int $3")
