/**
 * Generic hash map.
 *
 * Usage: Define HashmapKey, HashmapValue and HashmapPrefix. Then include
 * hashmap.inl to define a hash map with the corresponding key and value types.
 * You can include hashmap.inl multiple times to define different Hash maps.
 *
 * If HashmapName is not defined, the type name will be [prefix]Hashmap
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
 * `  #define HashmapName     MyMap
 * `  #define HashmapPrefix   test
 * `  #define HashmapKey      int
 * `  #define HashmapVal      bool
 * `  #include "hashmap.inl"
 * `
 * `  // Now test_Hashmap is defined. Insert elements with test_hmInsert and
 * `  // get them with test_hmGet
 * `  MyMap hm = {0};
 * `  testInsert(&hm, 42, TRUE);
 * `  bool v = testGet(&hm, 42);  // TRUE
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
 * `   #define HashmapPrefix  str
 * `   #define HashmapKey     char*
 * `   #define HashmapValue   int
 * `   #define HashFunction   myHash
 * `   #define KeyCompareFunc myKeyCompare
 * `   #include "hashmap.inl"
 * `
 * `   int example_func() {
 * `     str_Hashmap hm = {0};
*  `     hm.arena = my_arena;  // You can specify the arena to control how the allocation is done.
 * `     strInsert(&hm, "hello world", 42);
 * `   }
 *
 *
 *
 *
 *
 *
 **/


#ifndef HashmapInitSize
#define HashmapInitSize 1024
#endif

#ifndef HashmapPrefix
#define HashmapPrefix HashmapKey ## _ ## HashmapValue
#endif

#ifndef HashmapKey
#warning "HashmapKey not defined. Default to int."
#define HashmapKey int
#endif

#ifndef HashmapValue
#warning "HashmapValue not defined. Default to int."
#define HashmapValue int
#endif

// Default hash functions.
u64 hashStr(char* str, size_t size);
u64 hashStrPtr (char** str);
b32 compareStringKey (char* a, char* b);

#ifndef HashPointer
#define HashPointer(ptr) (hashStr((char*)(ptr), sizeof(*(ptr))))
#endif

#ifndef HashFunction
#define HashFunction HashPointer
#endif

#if !defined(HashmapName)
#define HashmapName Generic(Hashmap)
#endif

// #if !defined(HashmapIterName)
#define HashmapIterName(postfix) HashmapIterNameEx(HashmapName, postfix)
#define HashmapIterNameEx(hmName, postfix) GenericExEx(hmName, postfix)
#define HashmapIterNameExEx(hmName, postfix) hmName ## postfix
//#undef HashmapIterNameEx
// #endif

#if !defined(HashmapError)
#define HashmapError(msg) Assert(!(msg))
#endif

#define Generic(name)            GenericEx(HashmapPrefix, name)
#define GenericEx(pref, name)    GenericExEx(pref, name)
#define GenericExEx(pref, name)  pref ## name


typedef struct Generic(HashmapKeyVal_s) Generic(HashmapKeyVal);

struct Generic(HashmapKeyVal_s) {
   HashmapKey              key;
   HashmapValue            val;
   Generic(HashmapKeyVal)* next;
};

typedef struct {
   Arena                   arena;
   Generic(HashmapKeyVal)  *keyvals;
   u32                     n_keyvals;
} HashmapName;

typedef struct {
   int foo;
} HashmapIterName(KeyIter);

void
Generic(__maybeInit) (HashmapName* hm) {
   if (!hm->keyvals) {
      hm->n_keyvals = HashmapInitSize;
      hm->keyvals = AllocArray(&hm->arena, Generic(HashmapKeyVal), hm->n_keyvals);
   }
}

HashmapValue*
Generic(Insert) (HashmapName* hm, HashmapKey key, HashmapValue val) {
   Generic(__maybeInit(hm));

   u64 hash = HashFunction(&key);
   Generic(HashmapKeyVal) *kv = &hm->keyvals[hash % hm->n_keyvals];
   while (true) {
   // Using the first element as a sentinel.
#if defined(KeyCompareFunc)
      if (KeyCompareFunc(key, kv->key)) {
#else
      if (key == kv->key) {
#endif
         HashmapError("Duplicate key!");
      }
      if (kv->next) {
         kv = kv->next;
      }
      else {
         break;
      }
   } while(kv->next);
   kv->next = AllocType(&hm->arena, Generic(HashmapKeyVal));
   kv = kv->next;
   kv->key = key;
   kv->val = val;

   return &kv->val;
}

HashmapValue*
Generic(Get) (HashmapName* hm, HashmapKey   key) {
   Generic(__maybeInit(hm));
   u64 hash = HashFunction(&key);
   Generic(HashmapKeyVal)* kv = hm->keyvals[hash % hm->n_keyvals].next;
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

void
Generic(KeyIterBegin) (HashmapName*hm, HashmapIterName(KeyIter)* iter) {

}

#ifdef KeyCompareFunc
  #undef KeyCompareFunc
#endif

#undef Generic
#undef GenericEx
#undef GenericExEx
#undef HashmapName
#undef HashmapKey
#undef HashmapPrefix
#undef HashmapInitSize
#undef HashmapValue
#undef HashFunction
#undef HashmapIterName
#undef HashmapIterNameEx
#undef HashmapIterNameExEx
