
static FILE* g_asm;
typedef enum RegisterValueType_n {
   RegisterValueType_REGISTER,
   RegisterValueType_IMMEDIATE,
   RegisterValueType_STACK,
} RegisterValueType;

typedef struct RegisterValue_s {
   RegisterValueType type;
   int bits;
   union {
      // REGISTER
      struct {
         char* reg;
         char* reg_32;
         char* reg_8;
         b8    bound;
         b8    is_volatile;
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
   i64      stack_size;
   Arena*   arena;

   int      if_count;
   Scope*   prev;
   SymTable symbol_table;
};


typedef enum CodegenConfigFlags_n {
   Config_TARGET_MACOS = (1<<0),
   Config_TARGET_LINUX = (1<<1),
   Config_TARGET_WIN   = (1<<2),
} CodegenConfigFlags;

#define CODEGEN_QUEUE_SIZE 1024


typedef struct Codegen_s {
   Arena*      arena;
   Scope*      scope;
   char*       waiting;
   char*       queue[CODEGEN_QUEUE_SIZE];
   u64         queue_lines[CODEGEN_QUEUE_SIZE];
   int         n_queue;              // Size of the queue.
   Html*       html;
   char*       file_name;
   u64         last_line_number;
   u32         config;   // CodegenConfigFlags enum
   SymTable*   symbol_table;
} Codegen;

static
RegisterValue g_registers[] = {
   // TODO(large):  It's time to write a real register allocator.
   // This array is wrong. Some 32 and 64-bit registers are aliased to two 8
   // bit registers, which the current "solution" does not handle. We can do
   // another shitty solution which can handle the multi-register aliasing or
   // we can start doing a proper register allocator.
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
      .reg_8 = "ah",
   },
   {
      .reg    = "rdi",
      .reg_32 = "edi",
      .reg_8 = "bh"
   },
   {
      .reg    = "r8",
      .reg_32 = "r8d",
      .reg_8 = "ch",
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

void
setupVolatility(Codegen* c) {
   int* volatile_regs = NULL;
   if (   (c->config & Config_TARGET_MACOS)
       || (c->config & Config_TARGET_LINUX)) {
      // These are the non-volatile registers in macos
      static int volatile_regs_systemv[] = {
         Reg_RBX,
         Reg_RSI,
         Reg_RDI,
         Reg_R12,
         Reg_R13,
         Reg_R14,
         Reg_R15,
      };
      volatile_regs = volatile_regs_systemv;
   }
   else if (c->config & Config_TARGET_WIN) {
      static int volatile_regs_win64[] = {
         Reg_RAX, Reg_RCX, Reg_RDX, Reg_R8, Reg_R9, Reg_R10, Reg_R11
                 // TODO(medium): Floating point registers volatility.
      };
      volatile_regs = volatile_regs_win64;
   }
   for (int i = volatile_regs[0]; i < ArrayCount(volatile_regs); ++i) {
      g_registers[i].is_volatile = true;
   }
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


char*
registerString(Codegen* c, RegisterValue* r) {
   char *res = NULL;
   switch(r->type) {
      case RegisterValueType_REGISTER: {
         if (r->bits == 64) {
            res = r->reg;
         }
         else if (r->bits == 32) {
            res = r->reg_32;
         }
         else if (r->bits == 8) {
            res = r->reg_8;
         }
         else {
            codegenError("RegisterValue bits not set!");
         }
      }
      break;
      case RegisterValueType_IMMEDIATE: {
         res = allocate(c->scope->arena, 128);
         snprintf(res, 128, "0x%x", (int)r->immediate_value);
      } break;
      case RegisterValueType_STACK: {
         res = allocate(c->scope->arena, 128);
         if (r->bits == 8)
            snprintf(res, 128, "BYTE [ rbp - 0x%x ]", (int)r->offset);
         else if (r->bits == 32)
            snprintf(res, 128, "DWORD [ rbp - 0x%x ]", (int)r->offset);
         else if (r->bits == 64)
            snprintf(res, 128, "QWORD [ rbp - 0x%x ]", (int)r->offset);
      } break;
      default: {
         // WTF
         BreakHere;
      } break;
   }
   Assert(res);
   return res;
}

int
codegenPointerSize(Codegen* c) {
   return 8;  // 8 bytes.
}

RegisterValue*
allocateStackRegister(Codegen* c, int num_bits) {
   RegisterValue* result = allocate(c->scope->arena, sizeof(RegisterValue));

   result->type = RegisterValueType_STACK;
   result->offset = c->scope->stack_size;
   result->bits = num_bits;
   c->scope->stack_size += num_bits;

   return result;
}

RegisterValue*
allocateRegister(Codegen* c, int bits) {
   Assert(bits);
   RegisterValue* result = NULL;

   for (size_t i = Reg_RCX; i < Reg_Count; ++i) {
      RegisterValue* r = &g_registers[i];
      // TODO(small): Select register corresponding to bitness.
      if (!r->bound) {
         r->bound = true;
         r->bits = bits;
         result = r;
         break;
      }
   }

   // TODO(long): Implement a graph coloring algorithm for register allocation.

   if (!result) {
      result = allocateStackRegister(c, 64);
   }
   return result;
}
void
freeRegister(RegisterValue* reg) {
   if (reg->type == RegisterValueType_REGISTER) {
      reg->bound = false;
   }
}

void
codegenInit(Codegen* c) {
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
      "mov rcx, rax\n"
      "call ExitProcess\n"
#else  // macos
      // libc cleanup
      "mov edi, eax\n"
      "call _exit\n"
#endif
      ;

   fwrite(prelude, 1, strlen(prelude), g_asm);

   setupVolatility(c);
}

char*
codegenHtmlHidden(Codegen* c, u64 line_number) {
   char* hidden = allocate(c->arena, LINE_MAX);
   snprintf(hidden, LINE_MAX, "%s: %" FORMAT_I64 , c->file_name, line_number);
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
      Assert((*r)->bits);
      *r = allocateRegister(c, (*r)->bits);
      emitInstruction(c, 0, "mov %s, %s", registerString(c, *r), registerString(c, old_r));
   }
}

// Put a register in the stack.
RegisterValue*
stashRegister(Codegen* c, RegisterValue* r) {
   RegisterValue* stack = allocateStackRegister(c, 64);
   emitInstruction(c, 0, "mov %s, %s", registerString(c, stack), registerString(c, r));
   r->bound = false;
   return stack;
}

void
stashPopRegister(Codegen* c, RegisterValue* r, RegisterValue* stack) {
   emitInstruction(c, 0, "mov %s, %s", registerString(c, r), registerString(c, stack));
   r->bound = true;
}

void
incompleteInstruction(Codegen* c, char* waiting) {
   c->waiting = waiting;
}

void
finishInstruction(Codegen* c, i64 val) {
   // Modify waiting. instruction.
   // Emit queued instructions.

   char* waiting = c->waiting;

   char newasm[LINE_MAX] = {0};

   snprintf(newasm, LINE_MAX, "%s %" PLATFORM_FORMAT_I64, waiting, val);

   c->waiting = NULL;

   emitInstruction(c, 0, newasm);
   for (int i = 0; i < c->n_queue; ++i) {
      emitInstruction(c, c->queue_lines[i], c->queue[i]);
   }
   c->n_queue = 0;
}

void
pushScope(Codegen* c) {
   // TODO(medium): Different kinds of scope (6.2.1)
   Scope* prev_scope = c->scope;
   c->scope = allocate(c->arena, sizeof(*c->scope));
   ArenaBootstrap(c->scope, arena);
   c->scope->prev = prev_scope;
   emitInstruction(c, 0, "mov rdx, QWORD [rbp - 0x4]");
}

void
popScope(Codegen* c) {
   deallocate(c->scope->arena);
   if (!arenaIsEmpty(&c->scope->symbol_table.arena)) {
      deallocate(&c->scope->symbol_table.arena);
   }
   c->scope = c->scope->prev;
}

b32
nodeIsExpression(AstNode* node) {
   b32 result = false;
   if (node->type == Ast_MUL || node->type == Ast_DIV ||
       node->type == Ast_ADD || node->type == Ast_SUB ||
       node->type == Ast_EQUALS || node->type == Ast_LESS ||
       node->type == Ast_GREATER || node->type == Ast_LEQ ||
       node->type == Ast_GEQ ||
       node->type == Ast_FUNCCALL ||
       node->type == Ast_NUMBER || node->type == Ast_ID) {
      result = true;
   }
   return result;
}

void
targetPushParameter(Codegen* c, u64 n_param, AstNode* param) {
   if ((c->config & Config_TARGET_LINUX) || (c->config & Config_TARGET_MACOS)) {
      RegisterValue* r = &g_registers[Reg_RDI];
      RegisterValue* p = codegenEmit(c, param);
      switch (n_param) {
         case 0: {
            // r is already RDI
         } break;
         case 1: {
            r = &g_registers[Reg_RSI];
         } break;
         case 2: {
            r = &g_registers[Reg_RDX];
         } break;
         case 3: {
            r = &g_registers[Reg_RCX];
         } break;
         case 4: {
            r = &g_registers[Reg_R8];
         } break;
         case 5: {
            r = &g_registers[Reg_R9];
         } break;
      }
      r->bits = p->bits;
      emitInstruction(c, 0, "mov %s, %s", registerString(c, r), registerString(c, p));
   }
   else {
      Assert(!"Need to implement params on Windows.");
   }
}

RegisterValue*
targetPopParameter(Codegen* c, u64 n_param) {
   RegisterValue* r = &g_registers[Reg_RDI];
   if ((c->config & Config_TARGET_MACOS) || (c->config & Config_TARGET_LINUX)) {
      switch (n_param) {
         case 0: {
            // r is already RDI
         } break;
         case 1: {
            r = &g_registers[Reg_RSI];
         } break;
         case 2: {
            r = &g_registers[Reg_RDX];
         } break;
         case 3: {
            r = &g_registers[Reg_RCX];
         } break;
         case 4: {
            r = &g_registers[Reg_R8];
         } break;
         case 5: {
            r = &g_registers[Reg_R9];
         } break;
      }
   }
   else {
      Assert (!"Implement parameter pop in Windows.");
   }
   return r;
}


RegisterValue*
emitExpression(Codegen* c, AstNode* node) {
   RegisterValue* result = NULL;
   if (nodeIsExpression(node)) {
      AstNode* child0 = node->child;
      RegisterValue* r0 = NULL;
      RegisterValue* r1 = NULL;

      if (node->type == Ast_FUNCCALL) {
         AstNode* func = node->child;
         AstNode* params = func->sibling;
         char* label = func->tok->value.string;
         // Keep track of the volatile registers that were bound before we called the function
         RegisterValue* stored[Reg_Count] = {0};
         for (size_t i = 0; i < Reg_Count; ++i) {
            RegisterValue* v = &g_registers[i];
            if (v->bound && v->is_volatile) {
               stored[i] = stashRegister(c, v);
            }
         }
         // Put the parameters in registers and/or the stack.
         u64 n_param = 0;
         for (AstNode* param = params;
              param != NULL;
              param = param->sibling) {

            targetPushParameter(c, n_param++, param);
         }
         emitInstruction(c, node->line_number, "call %s", label);
         // Restore the volatile registers we saved.
         for (size_t i = 0; i < Reg_Count; ++i) {
            RegisterValue* v = &g_registers[i];
            if (stored[i]) {
               stashPopRegister(c, v, stored[i]);
            }
         }
         result = &g_registers[Reg_RAX];
      }
      else if (node->type == Ast_NUMBER) {
         result = allocate(c->scope->arena, sizeof(RegisterValue));
         result->type = RegisterValueType_IMMEDIATE;
         result->immediate_value = node->tok->value.integer;
         // TODO(large): Support non 32-bit integers.
         result->bits = 32;
      }
      else if (node->type == Ast_ID) {
         SymEntry* entry = symGet(&c->scope->symbol_table, node->tok->value.string);

         result = entry->regval;
      } else {
         r0 = codegenEmit(c, child0);
         AstNode* child1 = child0->sibling;
         r1 = codegenEmit(c, child1);
         if (r0->bits != r1->bits) {
            // TODO(large): promotion rules for non-integer types.
            // If both are integer types with the same sign, the one with the lower rank gets promoted.
            needsRegister(c, &r0);
            needsRegister(c, &r1);
            RegisterValue* smaller = r1;
            RegisterValue* larger = r0;

            if (r0->bits < r1->bits) {
               smaller = r0;
               larger = r1;
            }

            char* smaller_str = registerString(c, smaller);

            smaller->bits = larger->bits;
            emitInstruction(c, node->line_number, "movzx %s, %s", registerString(c, smaller), smaller_str);
         }
      }

      // Expressions that use two children register values.

      if (node->type == Ast_ADD) {
         needsRegister(c, &r0);
         emitInstruction(c, child0->line_number, "add %s, %s", registerString(c, r0), registerString(c, r1));
         result = r0;
         freeRegister(r1);
      }
      else if (node->type == Ast_SUB) {
         needsRegister(c, &r0);
         emitInstruction(c, child0->line_number, "sub %s, %s", registerString(c, r0), registerString(c, r1));
         result = r0;
         freeRegister(r1);
      }
      else if (node->type == Ast_MUL || node->type == Ast_DIV) {
         char* op = node->type == Ast_MUL ? "mul" : "div";
         if (r0 != &g_registers[Reg_RAX]) {
            emitInstruction(c, child0->line_number, "mov rax, %s", registerString(c, r0));
         }

         emitInstruction(c, node->line_number, "%s %s", op, registerString(c, r1));
         // TODO(medium): Missing type
         result = allocateRegister(c, 32);
         emitInstruction(c, node->line_number, "mov %s, rax", registerString(c, result));
         freeRegister(r0);
         freeRegister(r1);
      }
      else if (node->type == Ast_EQUALS ||
               node->type == Ast_LESS ||
               node->type == Ast_GREATER ||
               node->type == Ast_GEQ ||
               node->type == Ast_LEQ) {
         if (r0->bits != r1->bits) {
            codegenError("Expected same number of bits in expression.");
         }
         if (node->type == Ast_EQUALS) {
            emitInstruction(c, node->line_number, "test %s, %s", registerString(c, r0), registerString(c, r1));
         }
         else {
            needsRegister(c, &r0);
            emitInstruction(c, node->line_number, "cmp %s, %s", registerString(c, r0), registerString(c, r1));
         }
         freeRegister(r0);
         freeRegister(r1);
         result = allocateRegister(c, r0->bits);
         if (node->type == Ast_GREATER) {
            emitInstruction(c, node->line_number, "setg al");
            emitInstruction(c, node->line_number, "and al, 0x1", registerString(c, result));
            emitInstruction(c, node->line_number, "movzx %s, al", registerString(c, result));
         }
         else if (node->type == Ast_LESS) {
            emitInstruction(c, node->line_number, "setl al");
            emitInstruction(c, node->line_number, "and al, 0x1", registerString(c, result));
            emitInstruction(c, node->line_number, "movzx %s, al", registerString(c, result));
         }
         else if (node->type == Ast_GEQ) {
            emitInstruction(c, node->line_number, "setge al");
            emitInstruction(c, node->line_number, "and al, 0x1", registerString(c, result));
            emitInstruction(c, node->line_number, "movzx %s, al", registerString(c, result));
         }
         else if (node->type == Ast_LEQ) {
            emitInstruction(c, node->line_number, "setle al");
            emitInstruction(c, node->line_number, "and al, 0x1", registerString(c, result));
            emitInstruction(c, node->line_number, "movzx %s, al", registerString(c, result));
         }
         else {
            Assert(!"Implement this");
         }
      }
   }
   else {
      Assert (!"Not an expression");
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
               g_registers[Reg_RAX].bits = r->bits;
               emitInstruction(c, stmt->line_number, "mov %s, %s",
                               registerString(c, &g_registers[Reg_RAX]), registerString(c, r));
            }
            emitInstruction(c, stmt->line_number, "jmp .func_end");
         }
      } break;
      case Ast_DECLARATION: {
         AstNode* ast_type = stmt->child;
         AstNode* ast_id = ast_type->sibling;
         char* id_str = ast_id->tok->value.string;

         int value = ast_id->sibling->tok->value.integer;

         Assert (symGet(&c->scope->symbol_table, id_str) == NULL);
         int bits = 0;
         switch (ast_type->ctype->type) {
            case Type_INT: {
               bits = 32;
            } break;
            case Type_CHAR: {
               bits = 8;
            } break;
            default: {
               Assert(!"Unknown size for type.");
            } break;
         }
         RegisterValue* s = allocateStackRegister(c, bits);
         symInsert(&c->scope->symbol_table, id_str, (SymEntry){.ctype = *ast_type->ctype, .regval = s});

         char* reg = registerString(c, s);
         emitInstruction(c, stmt->line_number, "mov %s, 0x%x", reg, value);
      } break;
      case Ast_IF: {
         AstNode* cond = stmt->child;
         AstNode* then = cond->sibling;
         AstNode* els = then ? then->sibling : NULL;
         RegisterValue* cv = codegenEmit(c, cond);
         needsRegister(c, &cv);
         // TODO(long): Treat <,<=,>,>=,== comparisons differently?
         emitInstruction(c, stmt->line_number, "cmp %s, 0x0", registerString(c, cv));
         char else_label[256] = {0};
         snprintf(else_label, 256, ".else%d", c->scope->if_count++);
         emitInstruction(c, stmt->line_number, "je %s", else_label);
         codegenEmit(c, then);
         emitInstruction(c, stmt->line_number, "%s:", else_label);
         if (els) {
            codegenEmit(c, els);
         }
      } break;
      default: {
         // Expression statements
         if (nodeIsExpression(stmt)) {
            emitExpression(c, stmt);
         }
         else {
            Assert(!"This type of statement is not handled.");
         }
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
   AstNode* type        = node->child;
   AstNode* declarator  = type->sibling;
   AstNode* compound    = declarator->sibling;

   if (type && declarator && compound) {
      emitInstruction(c, node->line_number, "global %s", declarator->tok->value.string);
      emitInstruction(c, node->line_number, "%s:", declarator->tok->value.string);
      emitInstruction(c, node->line_number, "push rbp");
      emitInstruction(c, node->line_number, "mov rbp, rsp");
      incompleteInstruction(c, "sub rsp, ");

      // Push
      pushScope(c);

      AstNode* params = declarator->child;

      if (params) {
         AstNode* p = params;
         u64 n_param = 0;
         while (p) {
            Assert (p->type == Ast_PARAMETER);
            AstNode* type_spec = p->child;
            AstNode* declarator = type_spec->sibling;

            Assert (type_spec && type_spec->type == Ast_TYPE_SPECIFIER);
            Assert (declarator && declarator->type == Ast_ID);

            RegisterValue* reg = targetPopParameter(c, n_param++);

            reg->bits = 8 * numBytesForType(type_spec->ctype);
            char* id_str = declarator->tok->value.string;

            symInsert(&c->scope->symbol_table, id_str,
               (SymEntry){.ctype = *type_spec->ctype, .regval = reg});

            p = p->sibling;
         }
      }

      emitCompoundStatement(c, compound);

      i64 stack = c->scope->stack_size;

      stack = AlignPow2(stack, 16);
      // TODO(short): On mac OS, the stack needs to be aligned to 32 or 64 byte boundaries when m256 or m512 values are passed on the stack.

      popScope(c);

      finishInstruction(c, stack);

      emitInstruction(c, 0, ".func_end:");

      // Restore non-volatile registers.

      emitInstruction(c, 0, "add rsp, %d", stack);
      emitInstruction(c, 0, "pop rbp");
      emitInstruction(c, 0, "ret");
   }
   else {
      codegenError("Funcdef: Invalid node in the tree.");
   }
}

void
emitFunctionCall(Codegen* c, AstNode* node) {
   // Save all volatile registers which are bound.
   // Call the function.
   // Restore all volatile registers which are bound.
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
   else if (nodeIsExpression(node)) {
      result = emitExpression(c, node);
   }
   else if (node->type == Ast_COMPOUND_STMT) {
      emitCompoundStatement(c, node);
   }
   else {
      emitStatement(c, node);
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
