static FILE* g_asm;

typedef struct RegisterValue_s {
   char* reg;
   AstNode* bound;  // Non-NULL when an intermediate value is bound to this register.
} RegisterValue;

// Must be the same as g_registers.
enum RegisterEnum {
   Reg_RAX,
   Reg_RCX,
   Reg_RDX,
   Reg_RBX,
   Reg_RSI,
   Reg_RDI,

   Reg_Count,
};

#define SCOPE_HASH_SIZE 1024

typedef struct Scope_s Scope;
struct Scope_s {
   // Hash map from string variable to offset in the stack.
   int offsets[SCOPE_HASH_SIZE];

   Scope* prev;
};

typedef struct Codegen_s {
   Arena* arena;
   Scope* scope;
} Codegen;

static
RegisterValue g_registers[] = {
   {.reg = "rax"},
   {.reg = "rcx"},
   {.reg = "rdx"},
   {.reg = "rbx"},
   {.reg = "rsi"},
   {.reg = "rdi"},
};

// Forward declaration for recursive calls.
RegisterValue* codegenEmit(Codegen* c, AstNode* node);

void
codegenError(char* msg) {
   fprintf(stderr, "Codegen error: %s\n", msg);
   exit(1);
}


int
offsetForName(Codegen* c, char* name) {
   u64 hash = hash_str(name, strlen(name));
   int offset = c->scope->offsets[hash % SCOPE_HASH_SIZE];
   if (!offset) {
      char buf[1024] = {0};
      snprintf(buf, 1024, "Tying to use variable name that is not bounded. %s\n", name);
      codegenError(buf);
   }
   return offset;
}

// Keep track of register values per identifier.

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

void
pushScope(Codegen* c) {
   Scope* prev_scope = c->scope;
   c->scope = allocate(c->arena, sizeof(*c->scope));
   c->scope->prev = prev_scope;
}

void
popScope(Codegen* c) {
   c->scope = c->scope->prev;
}

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
emitExpression(Codegen* c, AstNode* node) {
   RegisterValue* result = NULL;
   if (node->val == Ast_MUL || node->val == Ast_DIV || node->val == Ast_ADD || node->val == Ast_SUB) {
      AstNode* child0 = node->child;
      AstNode* child1 = child0->sibling;
      RegisterValue* r0 = codegenEmit(c, child0);
      RegisterValue* r1 = codegenEmit(c, child1);

      if (node->val == Ast_ADD) {
         emitInstruction("add %s, %s\n", r0->reg, r1->reg);
         result = r0;
         freeRegister(r1);
      }
      else if (node->val == Ast_SUB) {
         emitInstruction("sub %s, %s\n", r0->reg, r1->reg);
         result = r0;
         freeRegister(r1);
      }
      else if (node->val == Ast_MUL || node->val == Ast_DIV) {
         char* op = node->val == Ast_MUL ? "mul" : "div";
         if (r0 != &g_registers[Reg_RAX]) {
            emitInstruction("mov rax, %s\n", r0->reg);
         }

         emitInstruction("%s %s\n", op, r1->reg);
         result = allocateRegister(node);
         emitInstruction("mov %s, rax\n", result->reg);
         freeRegister(r0);
         freeRegister(r1);
      }
   }
   else if (node->val == Ast_NUMBER) {
      char asm_line[LINE_MAX] = {0};
      RegisterValue* r = allocateRegister(node);
      PrintString(asm_line, LINE_MAX, "mov %s, %d\n", r->reg, node->tok->value.integer);
      emitInstruction(asm_line);
      result = r;
   }
   else if (node->val == Ast_ID) {
      int offset = offsetForName(c, node->tok->value.string);
      char asm_line[LINE_MAX] = {0};
      PrintString(asm_line, LINE_MAX, "mov eax, QWORD PTR[rbp - %x]\n", offset);
      emitInstruction(asm_line);
   }
   return result;
}

void
emitStatement(Codegen* c, AstNode* stmt) {
   switch (stmt->val) {
      case Ast_RETURN: {
         // Emit code for the expression and move it to rax.
         if (stmt->child) {
            emitExpression(c, stmt->child);
         }
      } break;
      case Ast_DECLARATION: {
         char* id_str = stmt->child->tok->value.string;
         u64 hash = hash_str(id_str, strlen(id_str));
         if (c->scope->offsets[hash % SCOPE_HASH_SIZE]) {
            Assert(!"Hash map collision handling not implemented.");
         }
         int value = stmt->child->sibling->tok->value.integer;
         c->scope->offsets[hash % SCOPE_HASH_SIZE] = 4;
         char asm_line[LINE_MAX] = {0};
         PrintString(asm_line, LINE_MAX, "mov QWORD PTR[rbp-%x], %x\n", 4, value);
         emitInstruction(asm_line);
      } break;
      default: {
         // Not handled
      } break;
   }
}

void
emitFunctionDefinition(Codegen* c, AstNode* node) {
   // Type
   AstNode* type = node->child;
   AstNode* id = type->sibling;
   AstNode* compound = id->sibling;
   if (type && id && compound) {
      emitInstruction("%s:\n", id->tok->value.string);

      Assert(compound->child->val == Ast_STACK_REQ);
      AstNode* stack_req = compound->child;
      // Emit first pass of stack req.
      AstNode* stmt = stack_req->sibling;

      // Push
      pushScope(c);

      // Emit function call prelude. Push stack
      while (stmt) {
         emitStatement(c, stmt);
         stmt = stmt->sibling;
      }

      popScope(c);
   }
}

RegisterValue*
codegenEmit(Codegen* c, AstNode* node) {
   RegisterValue* result = NULL;
   if (node->val == Ast_FUNCDEF) {
      emitFunctionDefinition(c, node);
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
