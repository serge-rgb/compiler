static FILE* g_asm;

void
codegenInit(void) {
    g_asm = fopen("out.asm", "w");
    char* prelude =
        "extern ExitProcess\n"
        "section .text\n"
        "global _start\n"
        "_start:\n";
    fwrite(prelude, 1, strlen(prelude), g_asm);
}

void
codegenEmit(char* asm_line) {
    fwrite(asm_line, 1, strlen(asm_line), g_asm);
}

void
codegenFinish(void) {
    char* end = "call ExitProcess\n";
    fwrite(end, 1, strlen(end), g_asm);
}
