#define STRING_HASH_BUCKET_SIZE 1024
#define STRING_CACHE_MAX_SIZE 512

typedef struct Buffer_s {
   char* current;
   char* end;  // Points to the end of the buffer.
} Buffer;

typedef struct StringList_s StringList;
struct StringList_s {
   char*         string;
   StringList*   next;
};

static Arena* g_string_arena;
static StringList* g_string_map[STRING_HASH_BUCKET_SIZE];

void
stringInit(Arena* a) {
   g_string_arena = a;
}

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

b32
stringsAreEqual(char* a, char* b) {
   b32 equal = true;
   while (*a && *b) {
      if (*a++ != *b++) {
         equal = false;
         break;
      }
   }
   if (equal && (*a != '\0' || *b != '\0')) {
      equal = false;
   }
   return equal;
}

char*
getStringFromBuffer(Buffer* buffer) {
   char* str = NULL;
   size_t size = buffer->end - buffer->current;
   if (size < STRING_CACHE_MAX_SIZE) {
      char temp_str[STRING_CACHE_MAX_SIZE] = {0};
      memcpy(temp_str, buffer->current, size);
      u64 hash = hashStr(temp_str, size) % STRING_HASH_BUCKET_SIZE;
      StringList* l = g_string_map[hash];
      while (l) {
         if (stringsAreEqual(l->string, temp_str)) {
            str = l->string;
         }
         l = l->next;
      }
      if (!str) {  // Not found. Allocate new one.
         char* new_str = allocate(g_string_arena, size+1); // Allocate one more byte for the string terminator.
         memcpy(new_str, temp_str, size+1);
         StringList* e = AllocType(g_string_arena, StringList);
         e->string = new_str;
         e->next = g_string_map[hash];
         g_string_map[hash] = e;
         str = new_str;
      }
   } else {
      // TODO: Just allocate and return.
      Assert(!"Don't know how to handle strings bigger than STRING_CACHE_MAX_SIZE");
   }
   return str;
}

char*
getString(char* orig) {
   Buffer b = {0};
   b.current = orig;
   b.end = orig + strlen(orig);
   return getStringFromBuffer(&b);
}
