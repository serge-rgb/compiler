
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

