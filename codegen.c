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
         char* reg_8;
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
   Arena* arena;

   int if_count;
   Scope* prev;
};

#define CODEGEN_QUEUE_SIZE 1024

typedef struct Codegen_s {
   Arena* arena;
   Scope* scope;
   char*  waiting;
   char*  queue[CODEGEN_QUEUE_SIZE];
   u64    queue_lines[CODEGEN_QUEUE_SIZE];
   int    n_queue;              // Size of the queue.
   Html*  html;
   char*  file_name;
   u64    last_line_number;
} Codegen;

static
RegisterValue g_registers[] = {
   {
      .reg    = "rax",
      .reg_32 = "eax",
      .reg_8  = "al",
   },
   {
      .reg    = "rbx",
      .reg_32 = "ebx",
      .reg_8  = "bl",
   },
   {
      .reg    = "rcx",
      .reg_32 = "ecx",
      .reg_8  = "cl",
   },
   {
      .reg    = "rdx",
      .reg_32 = "edx",
      .reg_8  = "dl",
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
registerStringWithBitness(Codegen* c, RegisterValue* r, int bitness) {
   char *res = NULL;
   switch(r->type) {
      case RegisterValueType_REGISTER: {
         if (bitness == 64) {
            res = r->reg;
         }
         else if (bitness == 32) {
            res = r->reg_32;
         }
         else if (bitness == 8) {
            res = r->reg_8;
         }
      }
      break;
      case RegisterValueType_IMMEDIATE: {
         res = allocate(c->scope->arena, 128);
         snprintf(res, 128, "0x%x", (int)r->immediate_value);
      } break;
      case RegisterValueType_STACK: {
         res = allocate(c->scope->arena, 128);
         if (bitness == 32)
            snprintf(res, 128, "DWORD [ rbp - 0x%x ]", (int)r->offset);
         else if (bitness == 64)
            snprintf(res, 128, "QWORD [ rbp - 0x%x ]", (int)r->offset);
         else if (bitness == 8)
            snprintf(res, 128, "BYTE [ rbp - 0x%x ]", (int)r->offset);
      } break;
   }
   return res;
}

char *
registerString(Codegen* c, RegisterValue* r) {
   return registerStringWithBitness(c, r, 64);
}

char *
registerString32(Codegen* c, RegisterValue* r) {
   return registerStringWithBitness(c, r, 32);
}

void
codegenError(char* msg, ...) {
   va_list args;
   va_start(args, msg);
   char buffer[LINE_MAX] = {0};
   vsnprintf(buffer, LINE_MAX, msg, args);
   fprintf(stderr, "Codegen error: %s\n", buffer);
   va_end(args);

   BreakHere;

   exit(-1);
}

int
offsetForName(Codegen* c, char* name) {
   u64 hash = hashStr(name, strlen(name));
   int offset = c->scope->offsets[hash % SCOPE_HASH_SIZE];
   if (!offset) {
      codegenError("Tying to use variable name that is not bounded. %s\n", name);
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
#ifdef _WIN32
      "extern ExitProcess\n"
#else
      "extern _exit\n"
#endif
      "section .text\n"
      "global _start\n"
      "_start:\n"
      "mov rbp, rsp\n"
      "push rbp\n"
      "call main\n"
#if defined(__linux__)
      "mov ebx, eax\n"
      "mov eax, 0x1\n"
      "int 0x80\n"  // Linux exit syscall.
#elif defined(_WIN32)
      "call ExitProcess\n";
#else  // macos
      // libc cleanup
      "mov edi, eax\n"
      "call _exit\n"
#endif
      ;

   fwrite(prelude, 1, strlen(prelude), g_asm);
}

char*
codegenHtmlHidden(Codegen* c, u64 line_number) {
   char* hidden = allocate(c->arena, LINE_MAX);
   snprintf(hidden, LINE_MAX, "%s: %llu", c->file_name, line_number);
   return hidden;
}

void
emitInstruction(Codegen* c, u64 line_number, char* asm_line, ...) {
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

   if (!line_number) {
      line_number = c->last_line_number;
   } else {
      c->last_line_number = line_number;
   }
   int written = vsnprintf(out_asm, LINE_MAX, asm_line, args);
   if (written >= LINE_MAX - 2) {  // Counting two extra character for the new line and 0 terminator.
      codegenError("LINE_MAX is not sufficient for instruction length.");
   }
   if (out_asm[written-1] != '\n') {
      out_asm[written] = '\n';
   }
   // If we are waiting on an instruction being modified, queue up.
   if (c->waiting) {
      if (c->n_queue < CODEGEN_QUEUE_SIZE) {
         c->queue_lines[c->n_queue] = line_number;
         c->queue[c->n_queue++] = out_asm;
      }
      else {
         codegenError("Queue overflow.");
      }
   }
   else {
      fwrite(out_asm, 1, strlen(out_asm), g_asm);
      printf("%s", out_asm);
      htmlEmit(c->html, out_asm, codegenHtmlHidden(c, line_number));
      va_end(args);
   }
}

void
needsRegister(Codegen* c, RegisterValue** r) {
   RegisterValue* old_r = *r;
   if (old_r->type != RegisterValueType_REGISTER) {
      *r = allocateRegister();
      emitInstruction(c, 0, "mov %s, %s", registerString32(c, *r), registerString32(c, old_r));
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

   emitInstruction(c, 0, newasm);
   for (int i = 0; i < c->n_queue; ++i) {
      emitInstruction(c, c->queue_lines[i], c->queue[i]);
   }
   c->n_queue = 0;
}

void
pushScope(Codegen* c) {
   Scope* prev_scope = c->scope;
   c->scope = allocate(c->arena, sizeof(*c->scope));
   ArenaBootstrap(c->scope, arena);
   c->scope->prev = prev_scope;
   emitInstruction(c, 0, "mov rdx, QWORD [rbp-4]");
}

void
popScope(Codegen* c) {
   deallocate(c->scope->arena);
   c->scope = c->scope->prev;
}

b32
nodeIsExpression(AstNode* node) {
   b32 result = false;
   if (node->type == Ast_MUL || node->type == Ast_DIV ||
       node->type == Ast_ADD || node->type == Ast_SUB ||
       node->type == Ast_EQUALS || node->type == Ast_LESS ||
       node->type == Ast_GREATER || node->type == Ast_LEQ ||
       node->type == Ast_GEQ) {
      result = true;
   }
   return result;
}

RegisterValue*
emitExpression(Codegen* c, AstNode* node) {
   RegisterValue* result = NULL;
   if (nodeIsExpression(node)) {
      AstNode* child0 = node->child;
      AstNode* child1 = child0->sibling;
      RegisterValue* r0 = codegenEmit(c, child0);
      RegisterValue* r1 = codegenEmit(c, child1);

      if (node->type == Ast_ADD) {
         needsRegister(c, &r0);
         emitInstruction(c, child0->line_number, "add %s, %s", registerString32(c, r0), registerString32(c, r1));
         result = r0;
         freeRegister(r1);
      }
      else if (node->type == Ast_SUB) {
         needsRegister(c, &r0);
         emitInstruction(c, child0->line_number, "sub %s, %s", registerString32(c, r0), registerString32(c, r1));
         result = r0;
         freeRegister(r1);
      }
      else if (node->type == Ast_MUL || node->type == Ast_DIV) {
         char* op = node->type == Ast_MUL ? "mul" : "div";
         if (r0 != &g_registers[Reg_RAX]) {
            emitInstruction(c, child0->line_number, "mov rax, %s", registerString32(c, r0));
         }

         emitInstruction(c, node->line_number, "%s %s", op, registerString32(c, r1));
         result = allocateRegister();
         emitInstruction(c, node->line_number, "mov %s, rax", registerString32(c, result));
         freeRegister(r0);
         freeRegister(r1);
      }
      else if (node->type == Ast_EQUALS ||
               node->type == Ast_LESS ||
               node->type == Ast_GREATER ||
               node->type == Ast_GEQ ||
               node->type == Ast_LEQ) {
         RegisterValue* left = codegenEmit(c, node->child);
         RegisterValue* right = codegenEmit(c, node->child->sibling);
         if (node->type == Ast_EQUALS)
            emitInstruction(c, node->line_number, "test %s, %s", registerString32(c, left), registerString(c, right));
         else
            emitInstruction(c, node->line_number, "cmp %s, %s", registerString32(c, left), registerString(c, right));
         result = allocateRegister();
         if (node->type == Ast_GREATER) {
            emitInstruction(c, node->line_number, "setg al");
            emitInstruction(c, node->line_number, "and al, 0x1", registerString(c, result));
            emitInstruction(c, node->line_number, "movzx %s, al", registerString32(c, result));
         } else {
            Assert(!"Implement this");
         }
      }
   }
   else if (node->type == Ast_NUMBER ||
            node->type == Ast_ID) {
      codegenEmit(c, node);
      char asm_line[LINE_MAX] = {0};
      RegisterValue* r = allocateRegister();
      PrintString(asm_line, LINE_MAX, "mov %s, %d", registerString32(c, r), node->tok->value.integer);
      emitInstruction(c, node->line_number, asm_line);
      result = r;
   }
   else if (node->type == Ast_ID) {
      int offset = offsetForName(c, node->tok->value.string);
      char asm_line[LINE_MAX] = {0};
      RegisterValue* r = allocateRegister();
      PrintString(asm_line, LINE_MAX, "mov %s, DWORD [rbp - 0x%x]", registerString32(c, r), offset);
      emitInstruction(c, node->line_number, asm_line);
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
               emitInstruction(c, stmt->line_number, "mov rax, %s", registerString(c, r));
               emitInstruction(c, stmt->line_number, "jmp .func_end");
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
         emitInstruction(c, stmt->line_number, "mov DWORD [rbp-0x%x], 0x%x", offset, value);
      } break;
      case Ast_IF: {
         AstNode* cond = stmt->child;
         AstNode* then = cond->sibling;
         RegisterValue* cv = codegenEmit(c, cond);
         needsRegister(c, &cv);
         // TODO: Treat <,<=,>,>=,== comparisons differently?
         emitInstruction(c, stmt->line_number, "cmp %s, 0x0", registerString32(c, cv));
         char else_label[256] = {0};
         snprintf(else_label, 256, ".else%d", c->scope->if_count++);
         emitInstruction(c, stmt->line_number, "je %s", else_label);
         codegenEmit(c, then);
         emitInstruction(c, stmt->line_number, "%s:", else_label);
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
   AstNode* type     = node->child;
   AstNode* id       = type->sibling;
   AstNode* compound = id->sibling;

   if (type && id && compound) {
      emitInstruction(c, node->line_number, "global %s", id->tok->value.string);
      emitInstruction(c, node->line_number, "%s:", id->tok->value.string);
      emitInstruction(c, node->line_number, "push rbp");
      emitInstruction(c, node->line_number, "mov rbp, rsp");
      queueInstruction(c, "sub rsp, ");

      // Push
      pushScope(c);

      emitCompoundStatement(c, compound);

      int stack = c->scope->stack_size;
      popScope(c);

      finishInstruction(c, stack);

      emitInstruction(c, 0, ".func_end:");
      emitInstruction(c, 0, "add rsp, %d", stack);
      emitInstruction(c, 0, "pop rbp");
      emitInstruction(c, 0, "ret");
   }
   else {
      codegenError("Funcdef: Invalid node in the tree.");
   }
}

void
codegenTranslationUnit(Codegen* c, AstNode* node) {
   while (node) {
      codegenEmit(c, node);
      node = node->sibling;
   }
}

RegisterValue*
codegenEmit(Codegen* c, AstNode* node) {
   RegisterValue* result = NULL;
   if (node->type == Ast_FUNCDEF) {
      emitFunctionDefinition(c, node);
   }
   else if (node->type == Ast_NUMBER) {
      // Allocate an immediate value with this number.
      result = allocate(c->scope->arena, sizeof(RegisterValue));
      result->type = RegisterValueType_IMMEDIATE;
      result->immediate_value = node->tok->value.integer;
   }
   else if (node->type == Ast_ID) {
      if (fitsInRegister(node)) {
         result = allocate(c->scope->arena, sizeof(RegisterValue));

         result->type = RegisterValueType_STACK;
         result->offset = offsetForName(c, node->tok->value.string);
      }
      else {
         result = allocateRegister();
         Assert(!"ID codegen not implemented.");
      }
   }
   else if (nodeIsExpression(node)) {
      result = emitExpression(c, node);
   }
   else if (node->type == Ast_COMPOUND_STMT) {
      emitCompoundStatement(c, node);
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
