extern int    sprintf_s(char * buffer, rsize_t bufsz, const char * format, ...);

#define PlatformAssert(expr) do { if (!(expr)) { __debugbreak(); } }  while(0)
#define PlatformPrintString(...) sprintf_s(__VA_ARGS__)
