typedef struct ArenaHeader_s {
   u8* previous;  // Pointer to the memory block of the previous arena.
} ArenaHeader;


u8*
arenaBlock(size_t* num_bytes) {
   size_t size = Max(ARENA_DEFAULT_BLOCK_SIZE, *num_bytes);
   *num_bytes = size;
   u8* chunk = calloc(1, *num_bytes + sizeof(ArenaHeader));
   return chunk;
}

void*
allocate(Arena* a, size_t num_bytes) {
   u8* ptr = NULL;
   if (a->block != NULL && num_bytes < (a->size - a->used)) {
      ptr = a->block + a->used;
      a->used += num_bytes;
   }
   else {
      size_t next_size = num_bytes;
      u8* next = arenaBlock(&next_size);
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

   *a = (Arena){0};
   while (block) {
      ArenaHeader* h = (ArenaHeader*)block-1;
      u8* p = h->previous;
      free(h);
      block = p;
   }
}

// Helper functions.

b32
arenaIsEmpty(Arena* a) {
   b32 empty = a->block == NULL;
   Assert(!empty || !a->next);
   return empty;
}
