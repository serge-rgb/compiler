#define ARENA_DEFAULT_BLOCK_SIZE Megabytes(1)

#define AllocType(arena, type) allocate(arena, sizeof(type))

typedef struct Arena_s Arena;
struct Arena_s {
   u8*     block;
   size_t  used;
   size_t  size;

   Arena* next;
};

typedef struct ArenaHeader_s {
   u8* previous;  // Pointer to the memory block of the previous arena.
} ArenaHeader;

void*
allocate(Arena* a, size_t num_bytes) {
   u8* ptr = NULL;
   if (a->block != NULL && num_bytes < (a->size - a->used)) {
      ptr = a->block + a->used;
      a->used += num_bytes;
   }
   else {
      size_t next_size = Max(ARENA_DEFAULT_BLOCK_SIZE, num_bytes);
      u8* next = calloc(1, next_size + sizeof(ArenaHeader));
      if (next) {
         ArenaHeader* h = (ArenaHeader*)next;
         h->previous = a->block;

         next += sizeof(ArenaHeader);
         a->block = next;
         a->size = next_size;
         a->used = num_bytes;
         ptr = a->block;  // Returning the start of the block.
      }
      else {
         // TODO: Fail gracefully.
      }
   }
   return ptr;
}

void
deallocate(Arena* a) {
   u8* block = a->block;
   if (!block) {
      fprintf(stderr, "WARNING: deallocating an arena with NULL block. (Double free?)\n");
   }
   while (block) {
      ArenaHeader* h = (ArenaHeader*)block-1;
      u8* p = h->previous;
      free(h);
      block = p;
   }
   *a = (Arena){0};
}
