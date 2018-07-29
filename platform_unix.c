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
platformCompileAndLinkAsmFile(char* filename_without_extension) {
   ErrorCode ret = Ok;
   // Call nasm from here.
   char asm_file[PathMax] = {0};
   snprintf(asm_file, PathMax, "%s.asm", filename_without_extension);
   char obj_file[PathMax] = {0};
   snprintf(obj_file, PathMax, "%s.o", filename_without_extension);
   pid_t pid = fork();
   printf("Running nasm\n");
   int nasm_status = 0;
   if (pid == 0) {
      char* nasm_args[] = { "nasm", "-f", "macho64", asm_file };
      // printargs(nasm_args, ArrayCount(nasm_args));
      execve("/usr/local/bin/nasm", nasm_args, NULL);
   }
   else if (pid == wait(&nasm_status)) {
      if (WIFEXITED(nasm_status) && WEXITSTATUS(nasm_status) == 0) {
         printf("Running ld\n");
         char* ld_args[] = { "ld", "-arch", "x86_64", "-e", "_start", obj_file, "/usr/lib/libSystem.dylib", "-o", filename_without_extension };
         pid = fork();
         if (pid == 0) {
            // printargs(ld_args, ArrayCount(ld_args));
            execve("/usr/bin/ld", ld_args, NULL);
         }
         else if (pid == wait(NULL)) {
            printf("Running %s\n", filename_without_extension);
            char* out_args[] = { filename_without_extension };
            if (fork() == 0) {
               execve(filename_without_extension, out_args, NULL);
            }
            int status = 0;
            wait(&status);
            if (WIFEXITED(status)) {
               printf("Returned status: %d\n", WEXITSTATUS(status));
               if (WEXITSTATUS(status) != 1) {
                  printf("ERROR: test failed.\n");
                  exit(1);
               }
            }
            else {
               printf("Program exited incorrectly.");
            }
         }
      } else {
         fprintf(stderr, "nasm failed\n");
      }
   }
   return ret;
}

ErrorCode
platformListDirectory(char*** out_files, char* dirname, b32 (*filter)(char*)) {
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
               bufPush(out_files, getString(name));
            }
         }
      }
      closedir(dir);
   }
   return err;
}