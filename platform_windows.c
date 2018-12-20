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
platformRunProcess(char** args, sz n_args, i32 expected_return) {
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
            if (exit_code != (DWORD)expected_return) {
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
   if (Ok != platformRunProcess(args, ArrayCount(args), 0)) {
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
         "link.exe",
         objfile,
         exearg,
         "/DEBUG",
         pdbarg,
         "/ENTRY:_start",
         "/SUBSYSTEM:CONSOLE", "kernel32.lib"
      };
      if ( Ok != platformRunProcess(args, ArrayCount(args), 0)) {
         fprintf(stderr, "Link fail\n");
         result = CouldNotLink;
      }
   }
   return result;
}

ErrorCode
platformListDirectory(char*** out_files, char* dirname, b32 (*filter)(char*)) {
   ErrorCode err = Ok;
   WIN32_FIND_DATAA data = {0};
   char glob[PathMax] = {0};
   strncat(glob, dirname, PathMax);
   strncat(glob, "\\*", PathMax);
   HANDLE h = FindFirstFileA(glob, &data);
   if (h) {
      if (!(data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
         err = NotADirectory;
      }
      else {
         while (FindNextFileA(h, &data)) {
            if (data.cFileName[0] != '.') {
               if (filter(data.cFileName)) {
                  char file[PathMax] = {0};
                  strncat(file, dirname, PathMax);
                  strncat(file, "\\", PathMax);
                  strncat(file, data.cFileName, PathMax);

                  bufPush(*out_files, getString(file));
               }
            }
         }
      }
   }
   return err;
}

void
platformPathAtBinary(char* path, sz size) {
   char* tmp = (char*)calloc(1, size);
   strcpy(tmp, path);

   // TODO: Wide char paths on Windows..
   GetModuleFileNameA(NULL, path, (DWORD)size);

   {  // Remove the exe name
     char* last_slash = path;
     for ( char* iter = path;
       *iter != '\0';
       ++iter ) {
       if ( *iter == '\\' ) {
         last_slash = iter;
       }
     }
     *(last_slash+1) = '\0';
   }

   strcat(path, tmp);
   free(tmp);
}

char*
platformOutputBinaryFilename(Arena* arena, char* fname_without_extension) {
   char* appended = appendString(&temp_arena, trimmed, ".exe");
   return appended;
}
