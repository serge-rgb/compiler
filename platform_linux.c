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