static FILE* g_asm;

typedef struct RegisterValue_s {
    char* reg;
    AstNode* bound;  // Non-NULL when an intermediate value is bound to this register.
} RegisterValue;

// NOTE: All registers are 64 bit at the moment. We will

// Must be the same as g_register.
enum RegisterEnum {
    Reg_RAX,
    Reg_RCX,
    Reg_RDX,
    Reg_RBX,
    Reg_RSI,
    Reg_RDI,

    Reg_Count,
};

static
RegisterValue g_registers[] = {
    {.reg = "rax"},
    {.reg = "rcx"},
    {.reg = "rdx"},
    {.reg = "rbx"},
    {.reg = "rsi"},
    {.reg = "rdi"},
};

RegisterValue*
allocateRegister(AstNode* n) {
    RegisterValue* result = NULL;

    for (size_t i = Reg_RCX; i < Reg_Count; ++i) {
        RegisterValue* r = &g_registers[i];
        if (!r->bound) {
            r->bound = n;
            result = r;
            break;
        }
    }

    Assert(result != NULL);
    return result;
}

void
freeRegister(RegisterValue* reg) {
    reg->bound = NULL;
}

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
emitInstruction(char* asm_line, ...) {
    va_list args;
    va_start(args, asm_line);
    // vprintf(asm_line, args);
    char out_asm[LINE_MAX] = {0};
    vsnprintf(out_asm, LINE_MAX, asm_line, args);
    // fwrite(out_asm, 1, strlen(out_asm), g_asm);
    printf("%s", out_asm);
    va_end(args);
}

RegisterValue*
codegenEmit(AstNode* expr) {
    RegisterValue* result = NULL;
    if (expr->val == Ast_MUL || expr->val == Ast_DIV || expr->val == Ast_ADD || expr->val == Ast_SUB) {
        AstNode* child0 = expr->child;
        AstNode* child1 = child0->sibling;
        RegisterValue* r0 = codegenEmit(child0);
        RegisterValue* r1 = codegenEmit(child1);

        if (expr->val == Ast_ADD) {
            emitInstruction("add %s, %s\n", r0->reg, r1->reg);
            result = r0;
            freeRegister(r1);
        }
        else if (expr->val == Ast_SUB) {
            emitInstruction("sub %s, %s\n", r0->reg, r1->reg);
            result = r0;
            freeRegister(r1);
        }
        else if (expr->val == Ast_MUL || expr->val == Ast_DIV) {
            char* op = expr->val == Ast_MUL ? "mul" : "div";
            if (r0 != &g_registers[Reg_RAX]) {
                emitInstruction("mov rax, %s\n", r0->reg);
            }

            emitInstruction("%s %s\n", op, r1->reg);
            result = allocateRegister(expr);
            emitInstruction("mov %s, rax\n", result->reg);
            freeRegister(r0);
            freeRegister(r1);
        }
    }
    else if (expr->val == Ast_NUMBER) {
        char asm_line[LINE_MAX] = {0};
        RegisterValue* r = allocateRegister(expr);
        PrintString(asm_line, LINE_MAX, "mov %s, %d\n", r->reg, expr->tok->value.integer);
        emitInstruction(asm_line);
        result = r;
    }
    return result;
}

void
codegenFinish(void) {
    //char* end = "call ExitProcess\n";
    char* end =
        //"call exit\n"
        "ret\n";
    fwrite(end, 1, strlen(end), g_asm);
    fclose(g_asm);
}
