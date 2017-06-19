
#define PRI_size "li"

int raise(int sig);

#define SIGINT 5

extern int    snprintf( char * buffer, unsigned long bufsz, const char * format, ... );
extern int    vsnprintf( char * buffer, unsigned long bufsz, const char * format, va_list vlist );

#define PlatformAssert(expr) do { if (!(expr)) { raise(SIGINT); } } while(0)
#define PlatformPrintString(...) snprintf(__VA_ARGS__)
