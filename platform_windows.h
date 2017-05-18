extern int    sprintf_s(char * buffer, rsize_t bufsz, const char * format, ...);
extern int    vsnprintf( char * buffer, size_t bufsz, const char * format, va_list vlist );

#define PlatformAssert(expr) do { if (!(expr)) { __debugbreak(); } }  while(0)
#define PlatformPrintString(...) sprintf_s(__VA_ARGS__)
