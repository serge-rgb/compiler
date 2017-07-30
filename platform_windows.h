#define PRI_size "zi"

#if 0
#if !defined(__clang__)
    extern int    sprintf_s(char * buffer, rsize_t bufsz, const char * format, ...);
    extern int    vsnprintf( char * buffer, size_t bufsz, const char * format, va_list vlist );
#elif defined(__clang__)
    extern int    sprintf_s(char * buffer, unsigned long long bufsz, const char * format, ...);
//extern int    vsnprintf( char * buffer, unsigned long long bufsz, const char * format, va_list vlist );
    extern int    vsnprintf( char * buffer, unsigned long bufsz, const char * format, va_list vlist );
#endif
#endif

#define PlatformAssert(expr) do { if (!(expr)) { __debugbreak(); } }  while(0)
#define PlatformPrintString(...) sprintf_s(__VA_ARGS__)
