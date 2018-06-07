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

    // MessageBoxA(NULL, (LPCTSTR)display, "Error", MB_OK);

    fprintf(stderr, "%s\n", display);

    LocalFree(msg);
}

ErrorCode
platformCreateProcess(char** args, sz n_args) {
   ErrorCode result = Fail;

   if (n_args != 0) {
      STARTUPINFO startup_info = {
         .cb = sizeof(STARTUPINFO),
         .hStdError = GetStdHandle(STD_OUTPUT_HANDLE),
         .hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE),
         .hStdInput = GetStdHandle(STD_INPUT_HANDLE),
         .dwFlags = STARTF_USESTDHANDLES,
      };
      PROCESS_INFORMATION proc_info = Zero;

      // Create the child process.
      char cmdline[1024] = Zero;

      for (int i =0; i < n_args; ++i) {
         strncat(cmdline, args[i], ArrayCount(cmdline));
         strncat(cmdline, " ", ArrayCount(cmdline));
      }

      printf("Command line: %s\n", cmdline);

      BOOL cpr = CreateProcess(/*lpApplicationName*/ NULL,  // Will grab first arg as app name.
                               /*lpCommandLine*/ cmdline,
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
         fprintf(stderr, "CreateProcess failed: Error %d\n", err);
      }
      else {
         DWORD wait_res = WaitForSingleObject(proc_info.hProcess, 2000);
         if (wait_res == WAIT_OBJECT_0) {
            DWORD exit_code = 0;
            GetExitCodeProcess(proc_info.hProcess, &exit_code);
            if (exit_code != 0) {
               fprintf(stderr, "Program failed with error code %ld\n", exit_code);
            }
            else {
               result = Ok;
            }
         }
         else  {
            fprintf(stderr, "WaitForSingleObject failed. Probably a timeout. %ld\n", wait_res);
         }

         CloseHandle(proc_info.hProcess);
         CloseHandle(proc_info.hThread);
      }
   }
   return result;
}

ErrorCode
platformCompileAndLinkAsmFile(char* outfile /*Filename without extension*/) {
   ErrorCode result = Ok;

   // TODO: Remove hardcoded path
   char asmfile[PathMax] = Zero; {
      snprintf(asmfile, ArrayCount(asmfile), "%s.asm", outfile);
   }

   char* args[] = {
      "C:\\Program Files\\NASM\\nasm.exe",
      asmfile,
      "-f win64",
      "-F cv8"
   };
   if (0 != platformCreateProcess(args, ArrayCount(args))) {
      fprintf(stderr, "Failure running nasm. %s\n", asmfile);
      result = CouldNotAssemble;
   }
   else {
      char exearg[PathMax] = Zero; {
         snprintf(exearg, ArrayCount(exearg), "/OUT:%s.exe", outfile);
      }
      char objfile[PathMax] = Zero; {
         snprintf(objfile, ArrayCount(objfile), "%s.obj", outfile);
      }
      char pdbarg[PathMax] = Zero; {
         snprintf(pdbarg, ArrayCount(pdbarg), "/PDB:%s.pdb", outfile);
      }
      char* args[] = {
         // "link.exe",
         "\"c:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.10.25017\\bin\\HostX64\\x64\\link.exe\""
                 "/PDB:out.pdb",
         objfile,
         exearg,
         "/DEBUG",
         pdbarg,
         "/ENTRY:_start",
         "/SUBSYSTEM:CONSOLE", "kernel32.lib"
      };
      if ( 0 != platformCreateProcess(args, ArrayCount(args))) {
         fprintf(stderr, "Link fail\n");
         result = CouldNotLink;
      }
   }
   return result;
}


