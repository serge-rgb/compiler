#define NUM_BYTES 1024


int
main(void) {
    Arena a = {0};
    codegenInit();
    FILE* fd = fopen("test.c", "r");

    if (fd) {
        size_t num_bytes = NUM_BYTES;
        char buffer[NUM_BYTES] = {0};

        size_t read = fread(buffer, 1, num_bytes, fd);
        printf("Read %" PRI_size " bytes\n", read);
        Token* tokens = tokenize(&a, buffer, read);
        for (Token* t = tokens; t != NULL; t = t->next) {
            tokenPrint(t);
        }
        AstNode* tree = parseExpression(tokens);
        codegenEmit(tree);
    }

    codegenFinish();

    return 0;
}

#undef NUM_BYTES
