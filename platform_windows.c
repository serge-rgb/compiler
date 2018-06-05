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
   HANDLE stdin_write = 0;
   HANDLE stdin_read = 0;
   HANDLE stdout_write = 0;
   HANDLE stdout_read = 0;

   STARTUPINFO startup_info = {
      .cb = sizeof(STARTUPINFO),
      .hStdError = stdout_write,
      .hStdOutput = stdout_write,
      .hStdInput = stdin_read,
      .dwFlags = STARTF_USESTDHANDLES,
   };
   SECURITY_ATTRIBUTES security_attrs = {
      .nLength = sizeof(SECURITY_ATTRIBUTES),
      .bInheritHandle = TRUE,
      .lpSecurityDescriptor = NULL,
   };
   PROCESS_INFORMATION proc_info = Zero;

   if (!CreatePipe(&stdout_read, &stdout_write, &security_attrs, 0) ) {
      winPrintError(GetLastError());
   }
   else if (!CreatePipe(&stdin_read, &stdin_write, &security_attrs, 0)) {
      winPrintError(GetLastError());
   }
#if 0
   else if (!SetHandleInformation(stdout_read, HANDLE_FLAG_INHERIT, (1<<31) - 1) ) {
      winPrintError(GetLastError());
   }
   else if (!SetHandleInformation(stdin_write, HANDLE_FLAG_INHERIT, (1<<31) - 1) ) {
      winPrintError(GetLastError());
   }
#endif
   // Create the child process.
   else {
      BOOL cpr = CreateProcess(/*lpApplicationName*/ name,
                               /*lpCommandLine*/ NULL,
                               /*lpProcessAttributes*/ NULL,
                               /*lpThreadAttributes*/ NULL,
                               /*bInheritHandles*/TRUE,
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
      else {
         {
            // Read pipe
            DWORD read_bytes, dwWritten;
            CHAR buffer[1] = Zero;
            BOOL ok = FALSE;
            HANDLE hParentStdOut = GetStdHandle(STD_OUTPUT_HANDLE);

            for (;;) {
               // TODO: Why is this blocking?
               ok = ReadFile(stdout_read, buffer, ArrayCount(buffer), &read_bytes, NULL);
               if(!ok)  {
                  winPrintError(GetLastError());
                  break;
               }
               if (read_bytes == 0) break;
            }
         }
         DWORD exit_code = 0;
         GetExitCodeProcess(proc_info.hProcess, &exit_code);
         if (exit_code != 0) {
            fprintf(stderr, "NASM failed with error code %ld\n", exit_code);

         }
         else {
         }
         CloseHandle(proc_info.hProcess);
         CloseHandle(proc_info.hThread);
      }
   }
   return 0;
}
