int
main(void) {
    Arena a = {0};
    u8* bytes = allocate(&a, 1024);
    if (bytes) {
        printf("The variable of bytes is %p\n", bytes);
    }
    u8* more = allocate(&a, ARENA_DEFAULT_BLOCK_SIZE * 2);
    if (more) {
        printf("Got more\n");
    }
    u8* last_one = allocate(&a, 1);
    if (last_one) {
        printf("One for the road.\n");
    }
    deallocate(&a);

    u8* eight = allocate(&a, 8);
    if (eight) {
        // See if application verifier catches an overflow.
        for (int i = 0; i < 9; ++i) {
            eight[i] = i;  // Overflow on i=8,9
        }
    }
    deallocate(&a);

    printf("Hello world!\n");
    return 0;
}