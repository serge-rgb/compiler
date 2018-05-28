// Good old stb-style stretchy buffer.

typedef struct BufHdr {
   sz capacity;
   sz used;
} BufHdr;

#define bufHdr(ptr) ((ptr) ? (BufHdr*)ptr - 1 : NULL)

void
bufMaybeResize(void** ptr, sz count, sz element_size) {
   // TODO(Out of mem)
   if (!*ptr) {
      *ptr = calloc(1, element_size*count + sizeof(BufHdr));
      ((BufHdr*)*ptr)->capacity = count;
      *ptr += sizeof(BufHdr);
   }
   else if (bufHdr(*ptr)->capacity - bufHdr(*ptr)->used < count) {
      *ptr = realloc(bufHdr(*ptr), sizeof(BufHdr) + (bufHdr(*ptr)->capacity *= 2)*element_size);
      *ptr += sizeof(BufHdr);
      // TODO(Zero-out new area)
   }
}

#define bufCount(ptr) \
        ((ptr) ? bufHdr(ptr)->used : 0)

#define bufPush(ptr, e) \
        (bufMaybeResize((void**)&ptr, 1, sizeof(e)), (ptr)[bufHdr(ptr)->used++] = (e))

#define bufPop(ptr)          \
        (_bufCrashIfZero(ptr), \
         (ptr)[--bufHdr(ptr)->used])

#define _bufCrashIfZero(ptr) \
        (bufCount(ptr) > 0  ? \
         0 : (*(volatile u8*)0 = 0))
