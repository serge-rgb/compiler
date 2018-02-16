
static FILE* g_asm;
typedef enum RegisterValueType_n {
   RegisterValueType_REGISTER,
   RegisterValueType_IMMEDIATE,
   RegisterValueType_STACK,
} RegisterValueType;


typedef enum EmitTarget_n {
   Target_TMP, // Using while I port to DDCG

   Target_NONE,
   Target_ACCUM,
   Target_STACK,
} EmitTarget;

typedef struct RegisterValue_s {
   RegisterValueType type;
   union {
      // REGISTER
      struct {
         char* reg;
         char* reg_32;
         char* reg_8;
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
typedef enum RegisterEnum_n {
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
} RegisterEnum;

typedef struct ExprType_s {
   int   bits;
   Ctype ctype;
} ExprType;

typedef struct SymEntry_s {
   ExprType expr_type;
   u64 offset;
} SymEntry;

#define HashmapName     SymTable
#define HashmapPrefix   sym
#define HashmapKey      char*
#define HashmapValue    SymEntry
#define HashFunction    hashStrPtr
#define KeyCompareFunc  compareStringKey
#include "hashmap.inl"

enum {
   Stack_OFFSET,
   Stack_QWORD,
};
typedef struct StackValue_s {
   unsigned int offset  : 6;
   unsigned int type : 2;
} StackValue;

#define SCOPE_HASH_SIZE 1024
typedef struct Scope_s Scope;
struct Scope_s {
   i64         stack_size;
   Arena*      arena;

   int         if_count;
   Scope*      prev;
   SymTable    symbol_table;
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

   u64         stack_offset;
   u64         n_stack;
   StackValue  stack[1024];
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
void codegenEmit(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target);

SymEntry*
findSymbol(Codegen* c, char* name) {

   SymEntry* entry = NULL;
   Scope* scope = c->scope;
   while (scope) {
      entry = symGet(&scope->symbol_table, name);
      if (entry) break;
      else scope = scope->prev;
   }

   return entry;

}

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

   Assert (!"Codegen error");

   exit(-1);
}


char*
registerString(Codegen* c, RegisterValue* r, int bits) {
   char *res = NULL;
   switch(r->type) {
      case RegisterValueType_REGISTER: {
         if (bits == 64) {
            res = r->reg;
         }
         else if (bits == 32) {
            res = r->reg_32;
         }
         else if (bits == 8) {
            res = r->reg_8;
         }
         else {
            codegenError("No size information.");
         }
      }
      break;
      case RegisterValueType_IMMEDIATE: {
         res = allocate(c->scope->arena, 128);
         snprintf(res, 128, "0x%x", (int)r->immediate_value);
      } break;
      case RegisterValueType_STACK: {
         res = allocate(c->scope->arena, 128);
         if (bits == 8)
            snprintf(res, 128, "BYTE [ rsp + 0x%x ]", (int)r->offset);
         else if (bits == 32)
            snprintf(res, 128, "DWORD [ rsp + 0x%x ]", (int)r->offset);
         else if (bits == 64)
            snprintf(res, 128, "QWORD [ rsp + 0x%x ]", (int)r->offset);
      } break;
      default: {
         // WTF
         InvalidCodePath;
      } break;
   }
   Assert(res);
   return res;
}

int
codegenPointerSize(Codegen* c) {
   return 8;  // 8 bytes.
}

void
codegenInit(Codegen* c, char* outfile) {
   char asmfile [PATH_MAX] = {};
   snprintf(asmfile, PATH_MAX, "%s.asm", outfile);
   g_asm = fopen(asmfile, "w");
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
instructionPrintf(Codegen* c, u64 line_number, char* asm_line, ...) {
   va_list args;
   va_start(args, asm_line);

   char out_asm[LINE_MAX] = {0};

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
   fwrite(out_asm, 1, strlen(out_asm), g_asm);

   printf("%s", out_asm);
   htmlEmit(c->html, out_asm, codegenHtmlHidden(c, line_number));
   va_end(args);
}

void
instructionReg(Codegen* c, u64 line_number, char* asm_line, int bits, ...) {
#define MaxRegs 2
   va_list args;
   va_start(args, bits);
   RegisterEnum reg = Reg_RAX;

   RegisterEnum regs[MaxRegs];
   int n_regs = 0;

   for (char* c = asm_line; *c != '\0'; ++c) {
      if (*c == '%') {
         if (n_regs < MaxRegs) {
            reg = va_arg(args, RegisterEnum);
            regs[n_regs++] = reg;
         }
         else {
            codegenError("Register string has more registers than supported.");
         }
      }
   }
   switch (n_regs) {
      case 0: {
         instructionPrintf(c, line_number, asm_line);
      } break;
      case 1: {
         instructionPrintf(c, line_number, asm_line,
                           registerString(c, &g_registers[regs[0]], bits));
      } break;
      case 2: {
         instructionPrintf(c, line_number, asm_line,
                           registerString(c, &g_registers[regs[0]], bits),
                           registerString(c, &g_registers[regs[1]], bits));
      } break;

   }
#undef MaxRegs
}

void
stackPushReg(Codegen* c, RegisterEnum reg) {
   instructionPrintf(c, 0, "push %s", registerString(c, &g_registers[reg], 64));
   c->stack_offset += 8;
   c->stack[c->n_stack++] = (StackValue){ .type = Stack_QWORD };
}

void
stackPushImm(Codegen* c, i64 val) {
   instructionPrintf(c, 0, "push %d", val);
   c->stack_offset += 8;
   c->stack[c->n_stack++] = (StackValue){ .type = Stack_QWORD };
}

void
stackPushOffset(Codegen* c, u64 bytes) {
   instructionPrintf(c, 0, "sub rsp, %d", bytes);
   c->stack_offset += bytes;
   c->stack[c->n_stack++] = (StackValue) { .type = Stack_OFFSET, .offset = bytes };
}

void
stackPop(Codegen* c, RegisterEnum reg) {
   StackValue s = c->stack[--c->n_stack];
   switch (s.type) {
      case Stack_QWORD: {
         instructionReg(c, 0, "pop %s", 64, reg);
         c->stack_offset -= 8;
      } break;
      case Stack_OFFSET: {
         c->stack_offset -= s.offset;
         instructionPrintf(c, 0, "add rsp, %d", s.offset);
      }
   }
}

void
pushScope(Codegen* c) {
   // TODO(medium): Different kinds of scope (6.2.1)
   Scope* prev_scope = c->scope;
   c->scope = allocate(c->arena, sizeof(*c->scope));
   ArenaBootstrap(c->scope, arena);
   c->scope->prev = prev_scope;
}

void
popScope(Codegen* c) {
   deallocate(c->scope->arena);
   if (!arenaIsEmpty(&c->scope->symbol_table.arena)) {
      deallocate(&c->scope->symbol_table.arena);
   }
   c->scope = c->scope->prev;
}

void
targetPushParameter(Codegen* c, u64 n_param, AstNode* param) {
   if ((c->config & Config_TARGET_LINUX) || (c->config & Config_TARGET_MACOS)) {
      RegisterEnum r = Reg_RDI;
      ExprType type = {};
      codegenEmit(c, param, &type, Target_ACCUM);
      switch (n_param) {
         case 0: {
            // r is already RDI
         } break;
         case 1: {
            r = Reg_RSI;
         } break;
         case 2: {
            r = Reg_RDX;
         } break;
         case 3: {
            r = Reg_RCX;
         } break;
         case 4: {
            r = Reg_R8;
         } break;
         case 5: {
            r = Reg_R9;
         } break;
      }
      instructionReg(c, 0, "mov %s, %s", type.bits, r, Reg_RAX);
   }
   else {
      Assert(!"Need to implement params on Windows.");
   }
}

void
targetPopParameter(Codegen* c, u64 n_param, EmitTarget target) {
   RegisterEnum r = Reg_RDI;
   if ((c->config & Config_TARGET_MACOS) || (c->config & Config_TARGET_LINUX)) {
      switch (n_param) {
         case 0: {
            // r is already RDI
         } break;
         case 1: {
            r = Reg_RSI;
         } break;
         case 2: {
            r = Reg_RDX;
         } break;
         case 3: {
            r = Reg_RCX;
         } break;
         case 4: {
            r = Reg_R8;
         } break;
         case 5: {
            r = Reg_R9;
         } break;
      }
      if (target == Target_STACK) {
         stackPushReg(c, r);
      }
      else {
         instructionReg(c, 0, "mov %s, %s", 64, Reg_RAX, r);
      }
   }
   else {
      Assert (!"Implement parameter pop in Windows.");
   }
}


void
emitExpression(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   if (nodeIsExpression(node)) {
      AstNode* child0 = node->child;

      if (node->type == Ast_FUNCCALL) {
         AstNode* func = node->child;
         char* label = func->tok->cast.string;

         AstNode* params = func->sibling;
         // Put the parameters in registers and/or the stack.
         u64 n_param = 0;
         for (AstNode* param = params;
              param != NULL;
              param = param->sibling) {

            targetPushParameter(c, n_param++, param);
         }

         instructionPrintf(c, node->line_number, "call %s", label);
         if (target == Target_STACK) {
            stackPushReg(c, Reg_RAX);
         }
      }
      else if (node->type == Ast_NUMBER) {
         int bits = 32;
         switch (target) {
            case Target_ACCUM: {
               instructionPrintf(
                  c,
                  node->line_number,
                  "mov %s, %d",
                  registerString(c, &g_registers[Reg_RAX], bits),
                  node->tok->cast.integer);
            } break;
            case Target_STACK: {
               stackPushImm(c, node->tok->value);
            } break;
            case Target_NONE: { /* Nothing */ } break;
            case Target_TMP: {
               InvalidCodePath;  // Will remove this case later.
            } break;
         }
         if (expr_type) {
            expr_type->bits = bits;
            expr_type->ctype = Type_INT;
         }
      }
      else if (node->type == Ast_ID) {
         SymEntry* entry = findSymbol(c, node->tok->cast.string);

         if (!entry) {
            codegenError("Use of undeclared identifier %s", node->tok->cast.string);
         }

         u64 rsp_relative = c->stack_offset - entry->offset;

         char* size_str = NULL;

         switch (entry->expr_type.bits) {
            case 32: {
               size_str = "DWORD";
            } break;
            case 16: {
               size_str = "WORD";
            } break;
            case 8: {
               size_str = "BYTE";
            } break;
            default: {
               Assert(!"Can't handle this size.");
            } break;
         }
         if (entry->expr_type.bits <= 16) {
            instructionPrintf(c, 0, "xor %s, %s",
                              registerString(c, &g_registers[Reg_RAX], 64),
                              registerString(c, &g_registers[Reg_RAX], 64));
         }
         instructionPrintf(c, 0, "mov %s, %s [ rsp + %d ]",
                           registerString(c, &g_registers[Reg_RAX], entry->expr_type.bits),
                           size_str,
                           rsp_relative);
         if (target == Target_STACK) {
            stackPushReg(c, Reg_RAX);
         }

         if (expr_type) {
            *expr_type = entry->expr_type;
         }
      }
      else {
         AstNode* child1 = child0->sibling;
         if (node->type == Ast_ADD ||
             node->type == Ast_SUB ||
             node->type == Ast_MUL ||
             node->type == Ast_DIV) {
            ExprType type_left = {0};
            ExprType type_right = {0};

            codegenEmit(c, child1, &type_right, Target_STACK);
            codegenEmit(c, child0, &type_left, Target_ACCUM);

            if ( !isArithmeticType(type_left.ctype) ) {
               codegenError("Left operator in binary expression is not arithmetic type.");
            }
            else if ( !isArithmeticType(type_right.ctype) ) {
               codegenError("Left operator in expression is not arithmetic type.");
            }

            stackPop(c, Reg_RBX);

            if (type_left.bits != type_right.bits ||
                type_left.ctype != type_right.ctype) {
               // If both are integer types, then apply integer promotion rules.
               if (isIntegerType(type_left.ctype) && isIntegerType(type_right.ctype)) {
                  ExprType* smaller = type_left.bits < type_right.bits ? &type_left  : &type_right;
                  ExprType* bigger  = type_left.bits < type_right.bits ? &type_right : &type_left;


                  smaller->bits = bigger->bits;
               }
               //
               // If one of them is floating point... do floating point conversion.
               // TODO(long): Implement floating point conversion rules.
            }

            if (expr_type) {
               *expr_type = type_left;
            }

            int bits = type_left.bits;
            switch (node->type) {
               case Ast_ADD: { instructionReg(c, 0, "add %s, %s", bits, Reg_RAX, Reg_RBX); } break;
               case Ast_SUB: { instructionReg(c, 0, "sub %s, %s", bits, Reg_RAX, Reg_RBX); } break;
               case Ast_MUL: { instructionReg(c, 0, "imul %s", bits, Reg_RBX); } break;
               case Ast_DIV: { instructionReg(c, 0, "idiv %s", bits, Reg_RBX); } break;
               default: break;
            }

            if (target == Target_STACK) {
               stackPushReg(c, Reg_RAX);
            }
         }
         else {
            codegenError("PORT ( expression )");
         }
      }
   }
   else {
      Assert (!"Not an expression");
   }
}

void
emitCondition(Codegen* c, AstNode* cond, char* then, char* els) {
   ExprType expr_type = {0};
   // codegenEmit(c, cond, &expr_type, Target_ACCUM);
   switch (cond->type) {
      case Ast_LESS:
      case Ast_LEQ:
      case Ast_GREATER:
      case Ast_GEQ:
      case Ast_EQUALS: {
         AstNode* left = cond->child;
         AstNode* right = cond->child->sibling;
         ExprType left_type = {0};
         ExprType right_type = {0};
         codegenEmit(c, right, &right_type, Target_STACK);
         codegenEmit(c, left, &left_type, Target_ACCUM);
         stackPop(c, Reg_RBX);
         if (left_type.bits != right_type.bits) {
            Assert(!"promotion!");
         }
         instructionReg(c, cond->line_number, "cmp %s, %s", left_type.bits, Reg_RAX, Reg_RBX);
         switch(cond->type) {
            case Ast_EQUALS: { instructionPrintf(c, 0, "je %s", then); } break;
            case Ast_LESS: { instructionPrintf(c, 0, "jl %s", then); } break;
            case Ast_LEQ: { instructionPrintf(c, 0, "jle %s", then); } break;
            case Ast_GREATER: { instructionPrintf(c, 0, "jg %s", then); } break;
            case Ast_GEQ: { instructionPrintf(c, 0, "jge %s", then); } break;
            default: break;
         }
         instructionPrintf(c, 0, "jmp %s", els);
      } break;
      default: {
         codegenEmit(c, cond, &expr_type, Target_ACCUM);
         instructionReg(c, cond->line_number, "cmp %s, %s", expr_type.bits, Reg_RAX, Reg_RBX);
         instructionPrintf(c, 0, "jne %s", then);
         instructionPrintf(c, 0, "jmp %s", els);
      } break;
   }
}

// forward decl
void emitCompoundStatement(Codegen* c, AstNode* compound, EmitTarget target);

void
emitStatement(Codegen* c, AstNode* stmt, EmitTarget target) {
   switch (stmt->type) {
      case Ast_COMPOUND_STMT : {
         emitCompoundStatement(c, stmt, target);
      } break;
      case Ast_RETURN: {
         // Emit code for the expression and move it to rax.
         if (stmt->child) {
            emitExpression(c, stmt->child, NULL, Target_ACCUM);
            instructionPrintf(c, stmt->line_number, "jmp .func_end");
         }
      } break;
      case Ast_DECLARATION: {
         AstNode* ast_type = stmt->child;
         AstNode* ast_id = ast_type->sibling;
         char* id_str = ast_id->tok->cast.string;
         AstNode* rhs = ast_id->sibling;

         Assert (symGet(&c->scope->symbol_table, id_str) == NULL);
         int bits = 8 * numBytesForType(ast_type->ctype);

         stackPushOffset(c, bits/8);

         SymEntry entry = {
            .expr_type = (ExprType) {
               .bits = bits,
               .ctype = ast_type->ctype
            },
            .offset = c->stack_offset
         };

         // Push ESP this number of bytes, but store the current offset.
         // Save this to sym table.
         symInsert(&c->scope->symbol_table, id_str, entry);

         if (isLiteral(rhs)) {
            // TODO: Non-integer values.
            int value = rhs->tok->value;

            switch (bits) {
               case 32: {
                  instructionPrintf(c, stmt->line_number, "mov DWORD [ rsp ], 0x%x", value);
               } break;
               case 8: {
                  instructionPrintf(c, stmt->line_number, "mov BYTE [ rsp ], 0x%x", value);
               } break;
               default: {
                  Assert(!"IMPL");
               } break;
            }
         }
         else /* right-hand-side is not a literal*/ {
            ExprType type = {};
            codegenEmit(c, rhs, &type, Target_ACCUM );
            instructionReg(c, rhs->line_number, "mov QWORD [ rsp ], %s", 64, Reg_RAX);
         }
      } break;
      case Ast_IF: {
         AstNode* cond = stmt->child;
         AstNode* then = cond->sibling;
         AstNode* els = then ? then->sibling : NULL;
         char then_label[1024] = {0};
         char else_label[1024] = {0};
         snprintf(then_label, 1024, ".then%d", c->scope->if_count);
         snprintf(else_label, 1024, ".else%d", c->scope->if_count++);
         emitCondition(c, cond, then_label, else_label);
         instructionPrintf(c, then ? then->line_number : 0, "%s:", then_label);
         if (then) {
            codegenEmit(c, then, NULL, Target_NONE);
         }
         else {
            codegenError("No then after if");
         }
         //instructionPrintf(then_label);
         instructionPrintf(c, els? els->line_number : 0, "%s:", else_label);
         if (els) {
            codegenEmit(c, els, NULL, Target_NONE);
         }
      } break;
      case Ast_ITERATION: {
         AstNode* decl = stmt->child;
         AstNode* control = decl->sibling;
         AstNode* after = control->sibling;
         AstNode* body = after->sibling;
         pushScope(c); {
            char loop_label[1024] = {0}; {
               snprintf(loop_label, sizeof(loop_label), ".body%d", c->scope->if_count);
            }
            char end_label[1024] = {0}; {
               snprintf(end_label, sizeof(end_label), ".end%d", c->scope->if_count++);
            }
            b32 after_is_control = (control->type == Ast_NONE);
            if (decl->type != Ast_NONE) { emitStatement(c, decl, Target_ACCUM); }

            instructionPrintf(c, 0, "%s:", loop_label);
            if (control->type != Ast_NONE) {
               emitStatement(c, control, Target_ACCUM);
               instructionReg(c, 0, "test %s, %s", 64, Reg_RAX, Reg_RAX);
               instructionPrintf(c, 0, "jz %s", end_label);
            }
            if (body->type != Ast_NONE) {
               emitStatement(c, body, Target_ACCUM);
            }
            if (after->type != Ast_NONE) {
               emitStatement(c, after, Target_ACCUM);
            }
            instructionPrintf(c, 0, "jmp %s", loop_label);
            instructionPrintf(c, 0, "%s:", end_label);
         } popScope(c);
      } break;
      default: {
         // Expression statements
         if (nodeIsExpression(stmt)) {
            emitExpression(c, stmt, NULL, target);
         }
         else {
            Assert(!"This type of statement is not handled.");
         }
      } break;
   }
}

void
emitCompoundStatement(Codegen* c, AstNode* compound, EmitTarget target) {

   pushScope(c);

   if (compound->type != Ast_COMPOUND_STMT) {
      codegenError("Expected a compound statement.");
   }
   AstNode* stmt = compound->child;

   // Emit function call prelude. Push stack
   while (stmt) {
      b32 is_last = stmt->sibling == NULL;
      emitStatement(c, stmt, is_last ? target : Target_NONE);
      stmt = stmt->sibling;
   }

   popScope(c);
}


void
emitFunctionDefinition(Codegen* c, AstNode* node, EmitTarget target) {
   AstNode* type        = node->child;
   AstNode* declarator  = type->sibling;
   AstNode* compound    = declarator->sibling;

   if (type && declarator && compound) {
      char *func_name = declarator->child->tok->cast.string;
      instructionPrintf(c, node->line_number, "global %s", func_name);
      instructionPrintf(c, node->line_number, "%s:", func_name);
      instructionPrintf(c, node->line_number, "push rbp");
      instructionPrintf(c, node->line_number, "mov rbp, rsp");

      // Helper when running in a debugger. Break on function entry.
      // instructionPrintf(c, 0, "int 3");

      // Push
      pushScope(c);

      AstNode* params = declarator->child->sibling;
      if (params) {
         AstNode* p = params;
         u64 n_param = 0;
         while (p) {
            Assert (p->type == Ast_PARAMETER);
            AstNode* param_type_spec = p->child;
            AstNode* param_declarator = param_type_spec->sibling;
            char* id_str = param_declarator->tok->cast.string;

            Assert (param_type_spec && param_type_spec->type == Ast_TYPE_SPECIFIER);
            Assert (param_declarator && param_declarator->type == Ast_ID);

            targetPopParameter(c, n_param++, Target_STACK);
            int bits = (int)(8 * numBytesForType(param_type_spec->ctype));

            SymEntry entry = {
               .expr_type = { .bits = bits, .ctype = param_type_spec->ctype },
               .offset = c->stack_offset,
            };
            symInsert(&c->scope->symbol_table, id_str, entry);

            p = p->sibling;

         }
      }

      emitCompoundStatement(c, compound, Target_ACCUM);

      //i64 stack = c->scope->stack_size;

      // TODO(short): Align the stack again.
      // stack = AlignPow2(stack, 16);
      // TODO(short): On mac OS, the stack needs to be aligned to 32 or 64 byte boundaries when m256 or m512 values are passed on the stack.

      instructionPrintf(c, 0, ".func_end:");

      while (c->n_stack > 0)  {
         stackPop(c, Reg_RBX);
      }

      popScope(c);

      // finishInstruction(c, stack);


      // Restore non-volatile registers.

      //instruction(c, 0, "add rsp, %d", stack);
      instructionPrintf(c, 0, "pop rbp");
      instructionPrintf(c, 0, "ret");
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
      if (node->type == Ast_FUNCDEF) {
         codegenEmit(c, node, NULL, Target_NONE);
      }
      else {
         Assert (!"Implement top level declarations.");
      }

      node = node->sibling;
   }
}

void
codegenEmit(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   if (node->type == Ast_FUNCDEF) {
      emitFunctionDefinition(c, node, target);
   }
   else if (nodeIsExpression(node)) {
      if (expr_type == NULL) {
         codegenError("expr_type is NULL when generating code for expression.");
      }
      emitExpression(c, node, expr_type, target);
   }
   else if (node->type == Ast_COMPOUND_STMT) {
      emitCompoundStatement(c, node, target);
   }
   else {
      emitStatement(c, node, Target_TMP);
   }
}

void
codegenFinish(void) {
   char* end =
      "int 3\n"
      "ret\n";
   fwrite(end, 1, strlen(end), g_asm);
   fclose(g_asm);
}
