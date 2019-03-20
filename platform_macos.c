
#include <mach-o/dyld.h> // _NSGetExecutablePath

#define PlatformDefaultTarget Config_TARGET_MACOS


ErrorCode
platformLink(char* ld_path, char* filename_without_extension) {
   ErrorCode ret = Ok;
   printf("Running ld\n");
   char obj_file[PathMax] = {0}; {
      snprintf(obj_file, PathMax, "%s.o", filename_without_extension);
   }

   char* ld_args[] = { ld_path, "-arch", "x86_64", "-e", "_start", obj_file, "/usr/lib/libSystem.dylib", "-o", filename_without_extension, "-macosx_version_min", "10.11" };
   if (Ok == platformRunProcess(ld_args, ArrayCount(ld_args), 0)) {
   }
   else {
      fprintf(stderr, "ld failed\n");
      ret = CouldNotLink;
   }

   return ret;
}

ErrorCode
platformAssemble(char* nasm_path, char* asm_file) {
   ErrorCode ret = Ok;
   char* nasm_args[] = { nasm_path, "-Znasm_output", "-f", "macho64", asm_file };
   if (Ok == platformRunProcess(nasm_args, ArrayCount(nasm_args), 0)) {
   }
   else {
      fprintf(stderr, "nasm failed\n");
      ret = CouldNotAssemble;
   }
   return ret;
}

void
platformPathAtBinary(char* path, sz size) {
   char* buffer = calloc(1, size);
   strncpy(buffer, path, size);
   _NSGetExecutablePath(path, (u32*)&size);
   {  // Remove the executable name
       char* last_slash = path;
       for(char* iter = path;
           *iter != '\0';
           ++iter)
       {
           if (*iter == '/')
           {
               last_slash = iter;
           }
       }
       *(last_slash+1) = '\0';
   }
   strncat(path, buffer, size);
   free(buffer);
}

