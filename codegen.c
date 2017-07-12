static FILE* g_asm;

typedef enum RegisterValueType_n {
   RegisterValueType_REGISTER,
   RegisterValueType_IMMEDIATE,
   RegisterValueType_STACK,
} RegisterValueType;

typedef struct RegisterValue_s {
   RegisterValueType type;
   union {
      // REGISTER
      struct {
         char* reg;
         char* reg_32;
         b8    bound;
      };
      // IMMEDIATE
      struct {
         u64 immediate_value;  // Cast to appropriate value based on token type.
      };
      // STACK
      struct {
         u64 offset;
      };
   };

} RegisterValue;


// Must be the same as g_registers.
enum RegisterEnum {
   Reg_RAX,
   Reg_RBX,
   Reg_RCX,
   Reg_RDX,
   Reg_RSI,
   Reg_RDI,
   Reg_R8,
   Reg_R9,
   Reg_R10,
   Reg_R11,
   Reg_R12,
   Reg_R13,
   Reg_R14,
   Reg_R15,

   Reg_Count,
};

#define SCOPE_HASH_SIZE 1024

typedef struct Scope_s Scope;
struct Scope_s {
   // Hash map from string variable to offset in the stack.
   int offsets[SCOPE_HASH_SIZE];
   int stack_size;

   Scope* prev;
};

#define CODEGEN_QUEUE_SIZE 1024

typedef struct Codegen_s {
   Arena* arena;
   Scope* scope;
   char*  waiting;
   char*  queue[CODEGEN_QUEUE_SIZE];
   int    n_queue;              // Size of the queue.
} Codegen;

static
RegisterValue g_registers[] = {
   {
      .reg    = "rax",
      .reg_32 = "eax",
   },
   {
      .reg    = "rbx",
      .reg_32 = "ebx",
   },
   {
      .reg    = "rcx",
      .reg_32 = "ecx",
   },
   {
      .reg    = "rdx",
      .reg_32 = "rdx",
   },
   {
      .reg    = "rsi",
      .reg_32 = "esi",
   },
   {
      .reg    = "rdi",
      .reg_32 = "edi",
   },
   {
      .reg    = "r8",
      .reg_32 = "r8d",
   },
   {
      .reg    = "r9",
      .reg_32 = "r9d",
   },
   {
      .reg    = "r10",
      .reg_32 = "r10d",
   },
   {
      .reg    = "r11",
      .reg_32 = "r11d",
   },
   {
      .reg    = "r12",
      .reg_32 = "r12d",
   },
   {
      .reg    = "r13",
      .reg_32 = "r13d",
   },
   {
      .reg    = "r14",
      .reg_32 = "r14d",
   },
   {
      .reg    = "r15",
      .reg_32 = "r15d",
   },

};

// Forward declaration for recursive calls.
RegisterValue* codegenEmit(Codegen* c, AstNode* node);

char*
registerStringWithBitness(RegisterValue* r, int bitness) {
   char *res = NULL;
   switch(r->type) {
      case RegisterValueType_REGISTER: {
         if (bitness == 64) {
            res = r->reg;
         }
         else if (bitness == 32) {
            res = r->reg_32;
         }
      } break;
      case RegisterValueType_IMMEDIATE: {
         // TODO: Allocate to current scope.
         res = calloc(1, 128);
         snprintf(res, 128, "%d", (int)r->immediate_value);
      } break;
      case RegisterValueType_STACK: {
         // TODO: Allocate to current scope.
         res = calloc(1, 128);
         if (bitness == 32)
            snprintf(res, 128, "DWORD [ rbp - 0x%x ]", (int)r->offset);
         else if (bitness == 64)
            snprintf(res, 128, "QWORD [ rbp - 0x%x ]", (int)r->offset);
      } break;
   }
   return res;
}

char *
registerString(RegisterValue* r) {
   return registerStringWithBitness(r, 64);
}

char *
registerString32(RegisterValue* r) {
   return registerStringWithBitness(r, 32);
}

void
codegenError(char* msg) {
   fprintf(stderr, "Codegen error: %s\n", msg);
   exit(1);
}

int
offsetForName(Codegen* c, char* name) {
   u64 hash = hashStr(name, strlen(name));
   int offset = c->scope->offsets[hash % SCOPE_HASH_SIZE];
   if (!offset) {
      char buf[1024] = {0};
      snprintf(buf, 1024, "Tying to use variable name that is not bounded. %s\n", name);
      codegenError(buf);
   }
   return offset;
}

b32
fitsInRegister(AstNode* node) {
   b32 result = false;
   if (node->type == Ast_NUMBER ||
       node->type == Ast_ID) {
      result = true;
   }
   else {
      codegenError("I don't know whether this node fits inside a register.");
   }
   return result;
}

// Keep track of register values per identifier.

RegisterValue*
allocateRegister(void) {
   RegisterValue* result = NULL;

   for (size_t i = Reg_RCX; i < Reg_Count; ++i) {
      RegisterValue* r = &g_registers[i];
      if (!r->bound) {
         r->bound = true;
         result = r;
         break;
      }
   }

   Assert(result != NULL);
   return result;
}

void
freeRegister(RegisterValue* reg) {
   if (reg->type == RegisterValueType_REGISTER) {
      reg->bound = false;
   }
}

void
codegenInit(void) {
   g_asm = fopen("out.asm", "w");
   char* prelude =
      //"extern ExitProcess\n"
      "extern _exit\n"
      "section .text\n"
      "global _start\n"
      "_start:\n"
      "mov rbp, rsp\n"
      "push rbp\n"
      "call main\n"
      // Linux exit syscall.
#if defined(__linux__)
      "mov ebx, eax\n"
      "mov eax, 0x1\n"
      "int 0x80\n"
#else
      // libc cleanup
      "mov edi, eax\n"
      "call _exit\n"
#endif
      ;

   fwrite(prelude, 1, strlen(prelude), g_asm);
}

void
emitInstruction(Codegen* c, char* asm_line, ...) {
   va_list args;
   va_start(args, asm_line);
   // vprintf(asm_line, args);
   char* out_asm = NULL;
   char stack_asm[LINE_MAX] = {0};
   if (c->waiting) {
      out_asm = allocate(c->arena, LINE_MAX);
   }
   else {
      out_asm = stack_asm;
   }

   int written = vsnprintf(out_asm, LINE_MAX, asm_line, args);
   if (written >= LINE_MAX - 2) {  // Counting two extra character for the new line and 0 terminator.
      codegenError("LINE_MAX is not sufficient for instruction length.");
   }
   if (out_asm[written-1] != '\n') {
      //out_asm[written] = '\n';
      *(out_asm + written) = '\n';
   }
   // If we are waiting on an instruction being modified, queue up.
   if (c->waiting) {
      c->queue[c->n_queue++] = out_asm;
   }
   else {
      fwrite(out_asm, 1, strlen(out_asm), g_asm);
      printf("%s", out_asm);
      va_end(args);
   }
}

void
needsRegister(Codegen* c, RegisterValue** r) {
   RegisterValue* old_r = *r;
   if (old_r->type != RegisterValueType_REGISTER) {
      *r = allocateRegister();
      emitInstruction(c, "mov %s, %s", registerString32(*r), registerString32(old_r));
   }
}

void
queueInstruction(Codegen* c, char* waiting) {
   c->waiting = waiting;
}

void
finishInstruction(Codegen* c, int val) {
   // Modify waiting. instruction.
   // Emit queued instructions.

   char* waiting = c->waiting;

   char newasm[LINE_MAX] = {0};

   snprintf(newasm, LINE_MAX, "%s %d", waiting, val);

   c->waiting = NULL;

   emitInstruction(c, newasm);
   for (int i = 0; i < c->n_queue; ++i) {
      emitInstruction(c, c->queue[i]);
   }
   c->n_queue = 0;
}


void
pushScope(Codegen* c) {
   Scope* prev_scope = c->scope;
   c->scope = allocate(c->arena, sizeof(*c->scope));
   c->scope->prev = prev_scope;
   emitInstruction(c, "mov rdx, QWORD [rbp-4]");
}

void
popScope(Codegen* c) {
   // TODO: Scope lifespan arena.

   // NOTE: We are currently leaking this scope but it won't be a
   // problem if we allocate it within a scope-based arena.
   c->scope = c->scope->prev;
}

RegisterValue*
emitExpression(Codegen* c, AstNode* node) {
   RegisterValue* result = NULL;
   if (node->type == Ast_MUL || node->type == Ast_DIV || node->type == Ast_ADD || node->type == Ast_SUB) {
      AstNode* child0 = node->child;
      AstNode* child1 = child0->sibling;
      RegisterValue* r0 = codegenEmit(c, child0);
      RegisterValue* r1 = codegenEmit(c, child1);

      if (node->type == Ast_ADD) {
         needsRegister(c, &r0);
         emitInstruction(c, "add %s, %s", registerString32(r0), registerString32(r1));
         result = r0;
         freeRegister(r1);
      }
      else if (node->type == Ast_SUB) {
         needsRegister(c, &r0);
         emitInstruction(c, "sub %s, %s", registerString32(r0), registerString32(r1));
         result = r0;
         freeRegister(r1);
      }
      else if (node->type == Ast_MUL || node->type == Ast_DIV) {
         char* op = node->type == Ast_MUL ? "mul" : "div";
         if (r0 != &g_registers[Reg_RAX]) {
            emitInstruction(c, "mov rax, %s", registerString32(r0));
         }

         emitInstruction(c, "%s %s", op, registerString32(r1));
         result = allocateRegister();
         emitInstruction(c, "mov %s, rax", registerString32(result));
         freeRegister(r0);
         freeRegister(r1);
      }
   }
   else if (node->type == Ast_NUMBER ||
            node->type == Ast_ID) {
      codegenEmit(c, node);
      char asm_line[LINE_MAX] = {0};
      RegisterValue* r = allocateRegister();
      PrintString(asm_line, LINE_MAX, "mov %s, %d", registerString32(r), node->tok->value.integer);
      emitInstruction(c, asm_line);
      result = r;
   }
   else if (node->type == Ast_ID) {
      int offset = offsetForName(c, node->tok->value.string);
      char asm_line[LINE_MAX] = {0};
      RegisterValue* r = allocateRegister();
      PrintString(asm_line, LINE_MAX, "mov %s, DWORD [rbp - 0x%x]", registerString32(r), offset);
      emitInstruction(c, asm_line);
      result = r;
   }
   return result;
}

void
emitStatement(Codegen* c, AstNode* stmt) {
   switch (stmt->type) {
      case Ast_RETURN: {
         // Emit code for the expression and move it to rax.
         if (stmt->child) {
            RegisterValue* r = emitExpression(c, stmt->child);
            if (r != &g_registers[Reg_RAX]) {
               emitInstruction(c, "mov rax, %s", registerString(r));
            }
         }
      } break;
      case Ast_DECLARATION: {
         char* id_str = stmt->child->tok->value.string;
         u64 hash = hashStr(id_str, strlen(id_str));
         if (c->scope->offsets[hash % SCOPE_HASH_SIZE]) {
            // TODO: Finish implementing hash map...
            Assert(!"Hash map collision handling not implemented.");
         }
         int value = stmt->child->sibling->tok->value.integer;
         // TODO: A value different than four for different kinds of types.
         int offset = c->scope->stack_size + 4;
         c->scope->offsets[hash % SCOPE_HASH_SIZE] = offset;
         c->scope->stack_size += 4;
         char asm_line[LINE_MAX] = {0};
         PrintString(asm_line, LINE_MAX, "mov DWORD [rbp-0x%x], 0x%x", offset, value);
         emitInstruction(c, asm_line);
      } break;
      default: {
         // Not handled
         Assert(!"This type of statement is not handled.");
      } break;
   }
}

void
emitCompoundStatement(Codegen* c, AstNode* compound) {
   if (compound->type != Ast_COMPOUND_STMT) {
      codegenError("Expected a compound statement.");
   }
   AstNode* stmt = compound->child;

   // Emit function call prelude. Push stack
   while (stmt) {
      emitStatement(c, stmt);
      stmt = stmt->sibling;
   }
}

void
emitFunctionDefinition(Codegen* c, AstNode* node) {
   AstNode* type = node->child;
   AstNode* id = type->sibling;
   AstNode* compound = id->sibling;

   if (type && id && compound) {
      emitInstruction(c, "global %s", id->tok->value.string);
      emitInstruction(c, "%s:", id->tok->value.string);
      emitInstruction(c, "push rbp");
      emitInstruction(c, "mov rbp, rsp");
      queueInstruction(c, "sub rsp, ");

      // Push
      pushScope(c);

      emitCompoundStatement(c, compound);

      int stack = c->scope->stack_size;
      popScope(c);

      finishInstruction(c, stack);

      emitInstruction(c, "add rsp, %d", stack);
      emitInstruction(c, "pop rbp");
      emitInstruction(c, "ret");
   }
   else {
      codegenError("Funcdef: Invalid node in the tree.");
   }
}

RegisterValue*
codegenEmit(Codegen* c, AstNode* node) {
   RegisterValue* result = NULL;
   if (node->type == Ast_FUNCDEF) {
      emitFunctionDefinition(c, node);
   }
   else if (node->type == Ast_NUMBER) {

      // Allocate a register with this number.
      // result = allocateRegister();
      // emitInstruction(c, "mov %s, %d", registerString32(result), node->tok->value.integer);
      // TODO: Scope allocation.
      result = allocate(c->arena, sizeof(RegisterValue));
      result->type = RegisterValueType_IMMEDIATE;
      result->immediate_value = node->tok->value.integer;
   }
   else if (node->type == Ast_ID) {
      if (fitsInRegister(node)) {
         result = allocate(c->arena, sizeof(RegisterValue));

         result->type = RegisterValueType_STACK;
         result->offset = offsetForName(c, node->tok->value.string);
      }
      else {
         result = allocateRegister();
         Assert(!"ID codegen not implemented.");
      }
   }
   else {
      Assert(!"Codegen emit for this kind of node is not implemented.");
   }
   return result;
}

void
codegenFinish(void) {
   char* end =
      "int 3\n"
      "ret\n";
   fwrite(end, 1, strlen(end), g_asm);
   fclose(g_asm);
}
