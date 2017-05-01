int
main(void) {
    Arena a = {0};

    FILE* fd = fopen("test.c", "r");
    if (fd) {
#define NUM_BYTES 1024
        size_t num_bytes = NUM_BYTES;
        char buffer[NUM_BYTES] = {0};        
#undef NUM_BYTES
        
        size_t read = fread(buffer, 1, num_bytes, fd);
        printf("Read %li bytes\n", read);
        tokenize(&a, buffer, read);
    }
        
    return 0;
}

