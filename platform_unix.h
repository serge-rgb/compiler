

int raise(int sig);

#define SIGINT 5

extern int    snprintf( char * buffer, unsigned long bufsz, const char * format, ... );

#define PlatformAssert(expr) do { if (!(expr)) { raise(SIGINT); } } while(0)
#define PlatformPrintString(...) snprintf(__VA_ARGS__)
