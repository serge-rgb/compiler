#include "unistd.h"
#include "dirent.h"

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


#define PlatformAssert(expr) do { if (!(expr)) { \
                                                printf("Assertion failed: %s\n", #expr);\
                                                __builtin_trap(); } } while(0)

#define PlatformPrintString(...) snprintf(__VA_ARGS__)
#define PlatformBreak __asm ("int $3")

ErrorCode
platformRunProcess(char** args, sz n_args, i32 expected_return) {
   ErrorCode ret = Fail;
   int pid = fork();

   int status = -1;
   if (pid == 0) {
      execve(args[0], args, NULL);
      exit(42);
   }
   else if (pid == wait(NULL) &&
            wait(&status) &&
            WIFEXITED(status)) {
      int exit_status = WEXITSTATUS(status);
      if (exit_status == expected_return) {
         ret = Ok;
      }
   }
   return ret;
}

ErrorCode
platformCompileAndLinkAsmFile(char* filename_without_extension) {
   Break;
   ErrorCode ret = Ok;
   // Call nasm from here.
   char asm_file[PathMax] = {0};
   snprintf(asm_file, PathMax, "%s.asm", filename_without_extension);
   char obj_file[PathMax] = {0};
   snprintf(obj_file, PathMax, "%s.o", filename_without_extension);
   printf("Running nasm\n");
   char* nasm_args[] = { "nasm", "-Znasm_output", "-f", "macho64", asm_file, 0 };
   if (Ok == platformRunProcess(nasm_args, ArrayCount(nasm_args), 0)) {
      printf("Running ld\n");
      char* ld_args[] = { "ld", "-arch", "x86_64", "-e", "_start", obj_file, "/usr/lib/libSystem.dylib", "-o", filename_without_extension };
      int pid = fork();
      if (pid == 0) {
            // printargs(ld_args, ArrayCount(ld_args));
         execve("ld", ld_args, NULL);
         exit(0);
      }
      else if (pid == wait(NULL)) {
         int status = 0;
         wait(&status);
         if (WIFEXITED(status) && WEXITSTATUS(status) == 0) {
            printf("ld finished successfully\n");
         }
         else {
            printf("ld failed with code %d", WEXITSTATUS(status));
         }
      }
   } else {
       fprintf(stderr, "nasm failed\n");
   }
   return ret;
}

ErrorCode
platformListDirectory(char*** s_out_files, char* dirname, b32 (*filter)(char*)) {
   ErrorCode err = Ok;
   DIR* dir = opendir(dirname);
   if (!dir) {
      err = CouldNotOpenDir;
   }
   else {
      struct dirent* entry = NULL;
      while ((entry = readdir(dir))) {
         char* name = entry->d_name;
         if (name[0] != '.') {
            if (filter(name)) {
               char file[PathMax] = Zero; {
                  strncat(file, dirname, PathMax);
                  strncat(file, "/", PathMax);
                  strncat(file, name, PathMax);
               }

               bufPush(*s_out_files, getString(file));
            }
         }
      }
      closedir(dir);
   }
   return err;
}

char*
platformOutputBinaryFilename(Arena* arena, char* fname_without_extension) {
   return fname_without_extension;
}
