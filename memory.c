#define ARENA_DEFAULT_BLOCK_SIZE MEGABYTES(1)

struct Arena {
    u8*     block;
    size_t  used;
    size_t  size;

    struct Arena* next;
};

struct ArenaHeader {
    u8* previous;  // Pointer to the memory block of the previous arena.
};

void*
allocate(struct Arena* a, size_t num_bytes) {
    u8* ptr = NULL;
    if (a->block != NULL && num_bytes < (a->size - a->used)) {
        ptr = a->block + a->used;
        a->used += num_bytes;
    }
    else {
        size_t next_size = MAX(ARENA_DEFAULT_BLOCK_SIZE, num_bytes);
        u8* next = calloc(1, next_size);
        if (next) {
            struct ArenaHeader* h = (struct ArenaHeader*)next;
            next += sizeof(struct ArenaHeader);
            h->previous = a->block;

            a->block = next;
            a->used = num_bytes;
            a->size = next_size;
            ptr = a->block;
        }
        else {
            // TODO: Fail gracefully.
        }
    }
    return ptr;
}

void
deallocate(struct Arena* a) {
    u8* block = a->block;
    while (block) {
        struct ArenaHeader* h = (struct ArenaHeader*)block-1;
        u8* p = h->previous;
        free(h);
        block = p;
    }
    *a = (struct Arena){0};
}