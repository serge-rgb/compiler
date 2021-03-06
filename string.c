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

//
// StringMap  -- A hash map matching string to strings.
// Stores allocated strings without duplication.
//    stringMapInsert
//    stringMapGet
#define HashmapName     StringMap
#define HashmapPrefix   stringMap
#define HashmapKey      char*
#define HashmapValue    char*
#define HashFunction    hashStrPtr
#define KeyCompareFunc  compareStringKey
#include "hashmap.inl"

static StringMap g_string_map;

void
stringInit(Arena* a) {
   g_string_arena = a;
}

b32
stringsAreEqual(char* a, char* b) {
   b32 equal = true;
   if (!a || !b) {
      if (a != b) {
         equal = false;
      }
   }
   if (a && b && a != b)  {
      while (*a && *b) {
         if (*a++ != *b++) {
            equal = false;
            break;
         }
      }
      if (equal && (*a != '\0' || *b != '\0')) {
         equal = false;
      }
   }
   return equal;
}

char*
getString(char* orig) {
   char* result = NULL;
   size_t size = strlen(orig) + 1;

   char** str_in_map = NULL;
   if ((str_in_map = stringMapGet(&g_string_map, orig))) {
      result = *str_in_map;
   }
   else {  // String not in hashmap. Allocate new one and insert.
      char* new_str = allocate(g_string_arena, size);
      memcpy(new_str, orig, size);
      stringMapInsert(&g_string_map, new_str, new_str);
      result = new_str;
   }

   return result;
}

char*
cloneString(Arena* a, char* input) {
   size_t len = strlen(input) + 1;
   char* result = AllocArray(a, char, len);
   memcpy(result, input, len-1);
   return result;
}

char*
appendString(Arena* a, char* x, char* y) {
   size_t len = strlen(x) + strlen(x) + 1;
   char* result = AllocArray(a, char, len);

   char* copy = result;

   if (x) while (((*copy++ = *x++), *x)) {}
   if (y) while ((*copy++ = *y++)) {}
   return result;
}