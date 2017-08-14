/**
 * Generic hash map.
 *
 * Usage: Define HashmapKey, HashmapValue and HashmapPrefix. Then include
 * hashmap.inl to define a hash map with the corresponding key and value types.
 * You can include hashmap.inl multiple times to define different Hash maps.
 *
 * By default, the keys are compared by value with ==. Define the KeyCompareFunc
 * preprocessor macro to implement your own key comparison. There is also the
 * HashFunction define to specify a hash function. See Example 2.
 *
 * See the rest of the defines at the beginning of hashmap.inl to see what
 * knobs can be tweaked.
 *
 * This is designed to be used in a single-translation-unit build. It will need
 * modification for this technique to work on a more traditional build style.
 *
 * The generated hashmap uses the Arena structure defined in memory.c
 *
 *
 * Example 1: Hashmap mapping int to bool
 *    This will define a type called test_Hashmap.
 * --------------------------------------
 *
 * `
 * `  #define HashmapPrefix   test
 * `  #define HashmapKey      int
 * `  #define HashmapVal      bool
 * `  #include "hashmap.inl"
 * `
 * `  // Now test_Hashmap is defined. Insert elements with test_hmInsert and
 * `  // get them with test_hmGet
 * `  test_Hashmap hm = {0};
 * `  hm.arena = my_arena;
 * `  test_hmInsert(&hm, 42, TRUE);
 * `  bool v = test_hmGet(&hm, 42);  // TRUE
 * `
 *
 * Example 2:  Hashmap mapping char* to int. This will define a hash map named
 * str_Hashmap which maps char* keys to int values.
 * --------------------------------------
 *
 * `
 * `   b32 myKeyCompare(char* a, char* b) {
 * `      b32 result = false;
 * `      size_t sa = strlen(a);
 * `      size_t sb = strlen(b);
 * `      if (sa == sb) {
 * `         size_t i;
 * `         for (i = 0; i < sa && a[i] == b[i]; ++i) {}
 * `         if (sa == i) {
 * `            result = true;
 * `         }
 * `      }
 * `      return result;
 * `   }
 * `   u64 myHash (char** str) {
 * `      size_t len = strlen(*str);
 * `      return hashStr(*str, len);
 * `   }
 * `   #define HashmapPrefix str
 * `   #define HashmapKey char*
 * `   #define HashmapValue int
 * `   #define HashFunction myHash
 * `   #define KeyCompareFunc myKeyCompare
 * `   #include "hashmap.inl"
 * `
 *
 **/


#ifndef HashmapSize
#define HashmapSize 1024
#endif

#ifndef HashmapPrefix
#define HashmapPrefix HashmapKey ## _ ## HashmapValue
#endif

#ifndef HashmapKey
#define HashmapKey int
#endif

#ifndef HashmapValue
#define HashmapValue int
#endif

#ifndef HashFunction
#define HashFunction HashPointer
#endif

#define GenericExEx(name, pref) pref ## _ ## name
#define GenericEx(name, pref) GenericExEx(name, pref)
#define Generic(name) GenericEx(name, HashmapPrefix)


typedef struct Generic(HashmapKeyVal_s) Generic(HashmapKeyVal);

struct Generic(HashmapKeyVal_s) {
   HashmapKey              key;
   HashmapValue            val;
   Generic(HashmapKeyVal)* next;
};

typedef struct {
   Arena*                  arena;
   Generic(HashmapKeyVal)  keyvals[HashmapSize];
} Generic(Hashmap);

void
Generic(hmInsert) (
                   Generic(Hashmap)* hm,
                   HashmapKey key,
                   HashmapValue val) {
   BreakHere;
   u64 hash = HashFunction(&key);
   Generic(HashmapKeyVal) *kv = &hm->keyvals[hash % HashmapSize];
   // Using the first element as a sentinel.
   while(kv->next) {
      kv = kv->next;
   }
   kv->next = AllocType(hm->arena, Generic(HashmapKeyVal));
   kv = kv->next;
   kv->key = key;
   kv->val = val;
}

HashmapValue*
Generic(hmGet) (
                Generic(Hashmap)* hm,
                HashmapKey        key) {
   u64 hash = HashFunction(&key);
   Generic(HashmapKeyVal)* kv = hm->keyvals[hash % HashmapSize].next;
   HashmapValue* result = NULL;
   while (kv) {
#if defined(KeyCompareFunc)
      if (KeyCompareFunc(key, kv->key)) {
#else
      if (key == kv->key) {
#endif
         result = &kv->val;
         break;
      }
      kv = kv->next;
   }
   return result;
}

#undef Generic
#undef GenericEx
#undef GenericExEx
#undef HashmapSize
#undef HashmapPrefix
