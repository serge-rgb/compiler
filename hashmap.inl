/**
 * Generic hash map.
 *
 **/


#ifndef HashmapSize
#define HashmapSize 1024
#endif

#ifndef HashmapPrefix
#define HashmapPrefix int_int
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
