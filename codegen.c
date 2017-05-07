static FILE* g_asm;

void
codegenInit(void) {
    g_asm = fopen("out.asm", "w");
    char* prelude =
        //"extern ExitProcess\n"
        "extern exit\n"
        "section .text\n"
        "global _start\n"
        "_start:\n"
        "push rbp\n"
        "mov rbp, rsp\n"
        ;
    fwrite(prelude, 1, strlen(prelude), g_asm);
}

static
void
emitInstruction(char* asm_line) {
    // fwrite(asm_line, 1, strlen(asm_line), g_asm);
    printf("%s", asm_line);

}

void
codegenEmit(AstNode* expr) {
    if (expr->val == Ast_MUL || expr->val == Ast_ADD) {
        // MUL has two children.
        AstNode* child0 = expr->child;
        AstNode* child1 = child0->sibling;
        codegenEmit(child0);  // Result of expression is in eax
        emitInstruction("mov ecx, eax\n");
        codegenEmit(child1);
    }

    if (expr->val == Ast_ADD) {
        emitInstruction("add eax, ecx\n");
    }
    else if (expr->val == Ast_MUL) {
        emitInstruction("mul eax, ecx\n");
    }
    else if (expr->val == Ast_NUMBER) {
        char asm_line[LINE_MAX] = {0};
        PrintString(asm_line, LINE_MAX, "mov eax, %d\n", expr->tok->value.integer);
        emitInstruction(asm_line);
    }
}

void
codegenFinish(void) {
    //char* end = "call ExitProcess\n";
    char* end =
        "call exit\n"
        "ret\n";
    fwrite(end, 1, strlen(end), g_asm);
    fclose(g_asm);
}
