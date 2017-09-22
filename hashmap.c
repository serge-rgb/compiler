
#ifndef HashPointer
#define HashPointer(ptr) (hashStr((char*)(ptr), sizeof(*(ptr))))
#endif

u64
hashStr(char* str, size_t size) {
   u64 hash = 0;
   u64 g = 0;
   for (size_t i = 0; i < size; ++i) {
      hash = (hash << 4) + str[i];
      g = hash & 0xf000000000000000;
      if (g) {
         hash ^= g >> 24;
         hash &= ~g;
      }
   }
   return hash;
}

// Utility functions.

u64
hashStrPtr (char** str) {
   size_t len = strlen(*str);
   return hashStr(*str, len);
}


b32
compareStringKey (char* a, char* b) {
   b32 result = false;
   if (a && b) {
      size_t sa = strlen(a);
      size_t sb = strlen(b);
      if (sa == sb) {
         size_t i;
         for (i = 0; i < sa && a[i] == b[i]; ++i) {}
         if (sa == i) {
            result = true;
         }
      }
   }
   return result;
}
