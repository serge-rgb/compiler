#define PlatformDefaultTarget Config_TARGET_LINUX

void
platformPathAtBinary(char* path, sz size) {
   char tmp[PATH_MAX] = Zero;
   tmp[0] = '/';
   strncat(tmp, path, PATH_MAX);
   readlink("/proc/self/exe", path, size);

   char* last_slash = path;
   for (char* iter = path; *iter != '\0'; iter++) {
      if (*iter == '/') {
         last_slash = iter;
      }
   }
   if (*last_slash == '/') {
      *last_slash = '\0';
   }

   strncat(path, tmp, size);
}

ErrorCode
platformAssemble(char* nasm_path, char* asm_file) {
   ErrorCode ret = Ok;
   char* nasm_args[] = { nasm_path, "-Znasm_output", "-f", "elf64", asm_file };
   if (Ok == platformRunProcess(nasm_args, ArrayCount(nasm_args), 0)) {
   }
   else {
      fprintf(stderr, "nasm failed\n");
      ret = CouldNotAssemble;
   }
   return ret;
}

ErrorCode
platformLink(char* ld_path, char* filename_without_extension) {
   ErrorCode ret = Ok;
   printf("Running ld\n");
   char obj_file[PathMax] = {0}; {
      snprintf(obj_file, PathMax, "%s.o", filename_without_extension);
   }

   char* ld_args[] = { ld_path, "-arch", "x86_64", obj_file, "-o", filename_without_extension };
   if (Ok == platformRunProcess(ld_args, ArrayCount(ld_args), 0)) {
   }
   else {
      fprintf(stderr, "ld failed\n");
      ret = CouldNotLink;
   }

   return ret;
}