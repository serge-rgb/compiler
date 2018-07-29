
#include <mach-o/dyld.h> // _NSGetExecutablePath

#define PlatformDefaultTarget Config_TARGET_MACOS


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

