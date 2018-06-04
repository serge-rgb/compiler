#define PRI_size "zi"
#define PLATFORM_FORMAT_I64 "lld"
#define PLATFORM_FORMAT_U64 "llu"

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

#pragma warning(push, 0)
#include <Windows.h>
#pragma warning(pop)

#define PlatformDefaultTarget Config_TARGET_WIN

#define PlatformAssert(expr) do { if (!(expr)) { MessageBox(0, \
                                                            "Assertion failed: " # expr, \
                                                            "Assertion",  \
                                                            MB_OK ); \
                                                 __debugbreak(); } }  while(0)
#define PlatformPrintString(...) sprintf_s(__VA_ARGS__)

#define PlatformBreak __debugbreak()


void
winPrintError(int err_code) {
    char* msg = 0;
    char display [PathMax] = Zero;

    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                  FORMAT_MESSAGE_FROM_SYSTEM |
                  FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,
                  err_code,
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                  (LPTSTR) &msg,
                  0, NULL );

    snprintf(display, PathMax, "Error: %s", msg);

    MessageBoxA(NULL, (LPCTSTR)display, "Error", MB_OK);

    fprintf(stderr, "%s\n", display);

    LocalFree(msg);
}

int
platformCreateProcess(char* name, char** args, sz n_args) {
    STARTUPINFO startup_info = Zero;
    PROCESS_INFORMATION proc_info = Zero;

    // TODO: UTF-16 path names
    BOOL cpr = CreateProcess(/*lpApplicationName*/ name,
                             /*lpCommandLine*/ NULL,
                             /*lpProcessAttributes*/ NULL,
                             /*lpThreadAttributes*/ NULL,
                             /*bInheritHandles*/ FALSE,
                             /*dwCreationFlags*/0,
                             /*lpEnvironment*/NULL,
                             /*lpCurrentDirectory*/NULL,
                             /*lpStartupInfo*/&startup_info,
                             /*lpProcessInformation*/&proc_info);

    if (!cpr) {
        int err = GetLastError();
        winPrintError(err);
        fprintf(stderr, "CreateProcess failed: Error %d", err);
        return 1;
    }

    return 0;
}
