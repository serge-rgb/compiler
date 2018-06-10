
static FILE* g_asm;


typedef enum EmitTarget_n {
   Target_TMP, // Using while I port to DDCG

   Target_NONE,
   Target_ACCUM,
   Target_STACK,
} EmitTarget;

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
} typedef RegisterEnum;

// TODO: Make Location fit in a register and use a hack to fit 64-bit values.
struct Location {
   enum {
      Location_INVALID,

      Location_IMMEDIATE,
      Location_REGISTER,
      Location_STACK,
      Location_POINTER,
   } type;
   union {
      // REGISTER
      struct {
         RegisterEnum reg;
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
} typedef Location;

struct Register {
   char* reg;
   char* reg_32;
   char* reg_8;
   b8    is_volatile;
} typedef Register;

struct ExprType {
   Ctype    c;
   Location location;
} typedef ExprType;


#define HashmapName     SymTable
#define HashmapPrefix   sym
#define HashmapKey      char*
#define HashmapValue    ExprType
#define HashFunction    hashStrPtr
#define KeyCompareFunc  compareStringKey
#include "hashmap.inl"

typedef struct StackValue_s {
   unsigned int offset  : 6;
   enum {
      Stack_OFFSET,
      Stack_QWORD,
   } type : 2;
} StackValue;

#define SCOPE_HASH_SIZE 1024
typedef struct Scope_s Scope;
struct Scope_s {
   Arena*      arena;

   int         if_count;
   Scope*      prev;
   SymTable    label_table;
   SymTable    tag_table;
   SymTable    symbol_table;
};

typedef enum CodegenConfigFlags_n {
   Config_TARGET_MACOS = (1<<0),
   Config_TARGET_LINUX = (1<<1),
   Config_TARGET_WIN   = (1<<2),
} CodegenConfigFlags;

#define CODEGEN_QUEUE_SIZE 1024


typedef struct Codegen_s {
   StackValue* stack;  // Stack allocation / de-allocation is done on a per-function basis.

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

   u64         stack_offset;  // # Bytes from the bottom of the stack to RSP.

   // Constants
   AstNode* one;
} Codegen;

static
Register g_registers[] = {
   { .reg = "rax", .reg_32 = "eax" , .reg_8 = "al" },
   { .reg = "rbx", .reg_32 = "ebx" , .reg_8 = "bl" },
   { .reg = "rcx", .reg_32 = "ecx" , .reg_8 = "cl" },
   { .reg = "rdx", .reg_32 = "edx" , .reg_8 = "dl" },
   { .reg = "rsi", .reg_32 = "esi" , .reg_8 = "ah" },
   { .reg = "rdi", .reg_32 = "edi" , .reg_8 = "bh" },
   { .reg = "r8" , .reg_32 = "r8d" , .reg_8 = "ch" },
   { .reg = "r9" , .reg_32 = "r9d" , .reg_8 = NULL },
   { .reg = "r10", .reg_32 = "r10d", .reg_8 = NULL },
   { .reg = "r11", .reg_32 = "r11d", .reg_8 = NULL },
   { .reg = "r12", .reg_32 = "r12d", .reg_8 = NULL },
   { .reg = "r13", .reg_32 = "r13d", .reg_8 = NULL },
   { .reg = "r14", .reg_32 = "r14d", .reg_8 = NULL },
   { .reg = "r15", .reg_32 = "r15d", .reg_8 = NULL },
};

// Forward declaration for recursive calls.
void codegenEmit(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target);

ExprType*
findTag(Codegen* c, char* name) {
   ExprType* entry = NULL;
   Scope* scope = c->scope;
   while (scope) {
      entry = symGet(&scope->tag_table, name);
      if (entry) break;
      else scope = scope->prev;
   }

   return entry;
}

ExprType*
findSymbol(Codegen* c, char* name) {
   ExprType* entry = NULL;
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
                 // TODO: Floating point registers volatility.
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
   char buffer[LineMax] = {0};
   vsnprintf(buffer, LineMax, msg, args);
   fprintf(stderr, "Codegen error: %s\n", buffer);
   va_end(args);

   Assert (!"Codegen error");

   exit(-1);
}


char*
locationString(Codegen* c, Location r, int bits) {
   char *res = NULL;
   switch(r.type) {
      case Location_REGISTER: {
         if (bits == 64) {
            res = g_registers[r.reg].reg;
         }
         else if (bits == 32) {
            res = g_registers[r.reg].reg_32;
         }
         else if (bits == 8) {
            res = g_registers[r.reg].reg_8;
         }
         else {
            codegenError("No size information.");
         }
      }
      break;
      case Location_IMMEDIATE: {
         res = allocate(c->scope->arena, 128);
         snprintf(res, 128, "0x%x", (int)r.immediate_value);
      } break;
      case Location_STACK: {
         u64 rsp_relative = c->stack_offset - r.offset;

#define ResultSize 64
         res = allocate(c->scope->arena, ResultSize);
         if (bits == 8)
            snprintf(res, ResultSize, "BYTE [ rsp + 0x%x ]", (int)rsp_relative);
         else if (bits == 16)
            snprintf(res, ResultSize, "WORD [ rsp + 0x%x ]", (int)rsp_relative);
         else if (bits == 32)
            snprintf(res, ResultSize, "DWORD [ rsp + 0x%x ]", (int)rsp_relative);
         else if (bits == 64)
            snprintf(res, ResultSize, "QWORD [ rsp + 0x%x ]", (int)rsp_relative);
      } break;
      default: {
         // WTF
         InvalidCodePath;
      } break;
   }
   Assert(res);
   return res;
}

void
codegenInit(Codegen* c, char* outfile) {
   char asmfile [PathMax] = Zero;
   snprintf(asmfile, PathMax, "%s.asm", outfile);
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

   // Constants
   c->one = makeAstNode(c->arena, Ast_NUMBER, 0,0);
   Token* one_tok = AllocType(c->arena, Token);
   one_tok->value = 1;
   c->one->tok = one_tok;
}

char*
codegenHtmlHidden(Codegen* c, u64 line_number) {
   char* hidden = allocate(c->arena, LineMax);
   snprintf(hidden, LineMax, "%s: %" FORMAT_I64 , c->file_name, line_number);
   return hidden;
}

void
instructionPrintf(Codegen* c, u64 line_number, char* asm_line, ...) {
   va_list args;
   va_start(args, asm_line);

   char out_asm[LineMax] = {0};

   if (!line_number) {
      line_number = c->last_line_number;
   } else {
      c->last_line_number = line_number;
   }
   int written = vsnprintf(out_asm, LineMax, asm_line, args);
   if (written >= LineMax - 2) {  // Counting two extra character for the new line and 0 terminator.
      codegenError("LineMax is not sufficient for instruction length.");
   }
   if (out_asm[written-1] != '\n') {
      out_asm[written] = '\n';
   }
   fwrite(out_asm, 1, strlen(out_asm), g_asm);

   printf("%s", out_asm);
   htmlEmit(c->html, out_asm, codegenHtmlHidden(c, line_number));
   va_end(args);
}

Location
registerLocation(RegisterEnum reg) {
   Location loc = { .type = Location_REGISTER, .reg = reg };
   return loc;
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
                           locationString(c, registerLocation(regs[0]), bits));
      } break;
      case 2: {
         instructionPrintf(c, line_number, asm_line,
                           locationString(c, registerLocation(regs[0]), bits),
                           locationString(c, registerLocation(regs[1]), bits));
      } break;
      default: {
         InvalidCodePath;
      } break;
   }
#undef MaxRegs
}

void
stackPushReg(Codegen* c, RegisterEnum reg) {
   instructionPrintf(c, 0, "push %s", locationString(c, registerLocation(reg), 64));
   c->stack_offset += 8;
   bufPush(c->stack, (StackValue){ .type = Stack_QWORD });
}

void
stackPushImm(Codegen* c, i64 val) {
   instructionPrintf(c, 0, "push %d", val);
   c->stack_offset += 8;
   bufPush(c->stack, (StackValue){ .type = Stack_QWORD });
}

void
stackPushOffset(Codegen* c, u64 bytes) {
   Assert(bytes);
   instructionPrintf(c, 0, "sub rsp, %d", bytes);
   c->stack_offset += bytes;
   StackValue val = { .type = Stack_OFFSET, .offset = bytes };
   bufPush(c->stack, val);
}

Location
locationFromId(Codegen* c, char* id) {
   ExprType* entry = findSymbol(c, id);
   if (!entry) {
      codegenError("Use of undeclared identifier %s", id);
   }
   Location var = entry->location;
   return var;
}

void
stackPop(Codegen* c, RegisterEnum reg) {
   StackValue s = bufPop(c->stack);
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
movOrCopy(Codegen* c, u64 line_number, Location out, Location in, int bits) {
   if (bits <= 64) {
      instructionPrintf(c, line_number, "mov %s, %s",
                        locationString(c, out, bits),
                        locationString(c, in, bits));
   }
   else {
      if (out.type == Location_REGISTER) {
         NotImplemented("Big copy - into register.");
      }
      else if (out.type == Location_STACK) {
         int bytes = bits/8;
         if (in.type == Location_REGISTER) {
            instructionReg(c, line_number, "mov rsi, %s", 64, in);
         }
         else if (in.type == Location_STACK) {
            instructionPrintf(c, line_number, "mov rsi, rsp");
            if (in.offset != c->stack_offset) {          // TODO lea
               instructionPrintf(c, line_number, "add rsi, %d", c->stack_offset - in.offset);
            }
         }
         instructionPrintf(c, line_number, "mov rdi, rsp");
         if (out.offset != c->stack_offset) {
            instructionPrintf(c, line_number, "add rdi, %d", c->stack_offset - out.offset);
         }
         instructionPrintf(c, line_number, "mov rcx, 0x%x", bytes);
         instructionPrintf(c, line_number, "rep movsb");
      }
      else {
         InvalidCodePath;
      }
   }
}

void
pushScope(Codegen* c) {
   Scope* prev_scope = c->scope;
   c->scope = allocate(c->arena, sizeof(*c->scope));
   ArenaBootstrap(c->scope, arena);
   c->scope->prev = prev_scope;
}

void
popScope(Codegen* c) {
   deallocate(c->scope->arena);

   // We might have pointers to objects declared in this scope
   // We can't deallocate, but we can go through all of our symbols and mark them as invalid.

   for (sz i = 0;
        i < c->scope->symbol_table.n_keyvals;
        ++i) {
      c->scope->symbol_table.keyvals[i] = (symHashmapKeyVal)Zero;
   }

   c->scope = c->scope->prev;
}

void
pushParameter(Codegen* c, u64 n_param, ExprType* etype) {
   if (isRealType(&etype->c)) {
      NotImplemented("Floating parameters.");
   }

   if ((c->config & Config_TARGET_LINUX) || (c->config & Config_TARGET_MACOS)) {
      RegisterEnum r = Reg_RDI;
      if (n_param < 6) {
         if (typeBits(&etype->c) <= 64) {
            switch (n_param) {
               case 0: { } break;
               case 1: { r = Reg_RSI; } break;
               case 2: { r = Reg_RDX; } break;
               case 3: { r = Reg_RCX; } break;
               case 4: { r = Reg_R8;  } break;
               case 5: { r = Reg_R9;  } break;
            }
         }
         movOrCopy(c, 0,  registerLocation(r), registerLocation(Reg_RAX), typeBits(&etype->c));
      }
      else {
         NotImplemented("More params.");
      }
   }
   else {
      // Windows x64 calling convention:
      // First 4 integers (left-to-right): RCX, RDX, R8, and R9
      // First 4 floats (left-to-right): XMM0-3
      // Items 5 an higher on the stack.
      // Items larger than 16 bytes passed by reference.
      Location loc = {0};
      if (typeBits(&etype->c) <= 64) {
         loc.type = Location_REGISTER;
         if (n_param < 4) {
            if (isIntegerType(&etype->c)) {
               switch(n_param) {
                  case 0: { loc.reg = Reg_RCX; } break;
                  case 1: { loc.reg = Reg_RDX; } break;
                  case 2: { loc.reg = Reg_R8;  } break;
                  case 3: { loc.reg = Reg_R9;  } break;
               }
            }
            else {
               NotImplemented("Float");
            }
         }
         else {
            NotImplemented("more than 4 params");
         }
      }
      else {
         stackPushOffset(c, typeBits(&etype->c) / 8);
         loc.type = Location_STACK;
         loc.offset = c->stack_offset;
      }
      movOrCopy(c, 0, loc, etype->location, typeBits(&etype->c));
   }
}

Location
popParameter(Codegen* c, Ctype* ctype, u64 n_param, u64* offset) {
   Location loc = Zero;

   if (isRealType(ctype)) {
      NotImplemented("float params");
   }

   if (typeBits(ctype) <= 64) {
      if ((c->config & Config_TARGET_MACOS) || (c->config & Config_TARGET_LINUX)) {
         if (n_param < 6) {
            loc.type = Location_REGISTER;
            if (isIntegerType(ctype)) {
               switch (n_param) {
                  case 0: { loc.reg = Reg_RDI; } break;
                  case 1: { loc.reg = Reg_RSI; } break;
                  case 2: { loc.reg = Reg_RDX; } break;
                  case 3: { loc.reg = Reg_RCX; } break;
                  case 4: { loc.reg = Reg_R8;  } break;
                  case 5: { loc.reg = Reg_R9;  } break;
               }
            }
         }
         else {
            loc = (Location){ .type = Location_STACK, .offset = c->stack_offset - *offset };
            *offset += typeBits(ctype);
         }
      }
      else if (c->config & Config_TARGET_WIN){
         if (n_param < 4) {
            if (isIntegerType(ctype)) {
               loc.type = Location_REGISTER;
               switch(n_param) {
                  case 0: { loc.reg = Reg_RCX; } break;
                  case 1: { loc.reg = Reg_RDX; } break;
                  case 2: { loc.reg = Reg_R8;  } break;
                  case 3: { loc.reg = Reg_R9;  } break;
               }
            }
         }
         else {
            loc = (Location){ .type = Location_STACK, .offset = c->stack_offset - *offset };
            *offset += typeBits(ctype);
         }
      }
   }
   else {
      loc = (Location){ .type = Location_STACK, .offset = c->stack_offset - *offset };
      *offset += typeBits(ctype);
   }
   return loc;
}

void
emitArithBinaryExpr(Codegen* c, AstType type, ExprType* expr_type,
                    AstNode* left, AstNode* right, EmitTarget target) {
   ExprType tleft = {0};
   ExprType tright = {0};

   codegenEmit(c, right, &tright, Target_STACK);
   codegenEmit(c, left, &tleft, Target_ACCUM);

   if ( !isArithmeticType(tleft.c) ) {
      codegenError("Left operator in binary expression is not arithmetic type.");
   }
   else if ( !isArithmeticType(tright.c) ) {
      codegenError("Left operator in expression is not arithmetic type.");
   }

   stackPop(c, Reg_RBX);

   if (typeBits(&tleft.c) != typeBits(&tright.c) ||
       tleft.c.type != tright.c.type) {
      // If both are integer types, then apply integer promotion rules.
      if (isIntegerType(&tleft.c) && isIntegerType(&tright.c)) {
         // ExprType* smaller = typeBits(&tleft.c) < typeBits(&tright.c) ? &tleft  : &tright;
         // ExprType* bigger  = typeBits(&tleft.c) < typeBits(&tright.c) ? &tright : &tleft;

         NotImplemented("Int promotion");
      }
      //
      // If one of them is floating point... do floating point conversion.
      // TODO: Implement floating point conversion rules.
   }

   if (expr_type) {
      *expr_type = tleft;
   }

   int bits = typeBits(&tleft.c);
   switch (type) {
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

void
emitIdentifier(Codegen*c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   char* id_str = node->tok->cast.string;
   ExprType* entry = findSymbol(c, id_str);
   Location loc = locationFromId(c, id_str);

   if (!entry) {
      codegenError("Use of undeclared identifier %s", node->tok->cast.string);
   }

   char* size_str = NULL;

   if (typeBits(&entry->c) > 64) {
      Assert(entry->location.type == Location_STACK);
      if (target != Target_NONE) {
         instructionPrintf(c, node->line_number, "mov rax, rsp");
         // TODO: LEA
         instructionPrintf(c, 0, "add rax, %d", c->stack_offset - entry->location.offset);
         if (target == Target_STACK) {
            stackPushReg(c, Reg_RAX);
         }
      }

   }
   else {
      switch (typeBits(&entry->c)) {
         case 64: {
            size_str = "QWORD";
         } break;
         case 32: {
            size_str = "DWORD";
         } break;
         case 16: {
            size_str = "WORD";
         } break;
         case 8: {
            size_str = "BYTE";
         } break;
         case 0:{
            InvalidCodePath;
         }
      }

      if (target != Target_NONE) {
         if (typeBits(&entry->c) <= 16) {
            instructionPrintf(c, 0, "xor %s, %s",
                              locationString(c, registerLocation(Reg_RAX), 64),
                              locationString(c, registerLocation(Reg_RAX), 64));
         }
         movOrCopy(c, 0, registerLocation(Reg_RAX), loc, typeBits(&entry->c));

         if (target == Target_STACK) {
            stackPushReg(c, Reg_RAX);
         }
      }
   }

   if (expr_type) {
      expr_type->c = entry->c;
      expr_type->location = loc;
   }

}

void
emitStructMemberAccess(Codegen*c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   char* struct_str = node->child->tok->cast.string;
   char* field_str = node->child->next->tok->cast.string;
   ExprType* symbol_entry = findSymbol(c, struct_str);
   if (!symbol_entry) {
      codegenError("%s undeclared.", struct_str);
   }
   char* tag_str = symbol_entry->c.aggr.tag;
   ExprType* struct_entry = findTag(c, tag_str);
   if (!struct_entry) {
      codegenError("No struct named %s", tag_str);
   }
   struct StructMember* members = struct_entry->c.aggr.members;

   u64 member_idx = MaxU64;
   for (u64 i = 0; i < bufCount(members); ++i) {
      if (!strcmp(members[i].id, field_str)) {
         member_idx = i;
         break;
      }
   }
   if (member_idx == MaxU64) {
      codegenError("Struct %s does not have %s member", tag_str, field_str);
   }

   struct StructMember* member = members + member_idx;

   if (symbol_entry->location.type == Location_STACK) {
      u64 member_offset = member->offset;

      Location loc = {
         .type = Location_STACK,
         .offset = symbol_entry->location.offset - member_offset,
      };

      if (expr_type) {
         expr_type->c = *(member->ctype);
         expr_type->location = loc;
      }
      int bits = typeBits(member->ctype);

      if (target != Target_NONE) {
         movOrCopy(c, node->line_number, registerLocation(Reg_RAX), loc, bits);

         if (target == Target_STACK) {
            stackPushReg(c, Reg_RAX);
         }
      }
   }
   else if (symbol_entry->location.type == Location_REGISTER) {
      if (target == Target_ACCUM) {
         instructionReg(c, node->line_number, "mov %s, %s", 64, Reg_RAX, symbol_entry->location.reg);
      }
      else if (target == Target_STACK) {
         stackPushReg(c, symbol_entry->location.reg);
      }
   }
}

void emitExpression(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target);  // forward decl

void
emitFunctionCall(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   AstNode* func = node->child;
   char* label = func->tok->cast.string;

   ExprType* sym = findSymbol(c, label);
   if (!sym) {
      codegenError("Call to undefined function. %s", label);
   }
   Ctype* type = &sym->c;
   if (type->type != Type_FUNC) {
      codegenError("%s is not a function.", label);
   }

   AstNode* params = func->next;

   u64 stack_top = bufCount(c->stack);

   // Put the parameters in registers and/or the stack.
   u64 n_param = 0;
   for (AstNode* param = params;
        param != NULL;
        param = param->next) {
      ExprType et = {0};
      emitExpression(c, param, &et, Target_NONE);
      pushParameter(c, n_param++, &et);
   }

   i32 expected_nparam = funcNumParams(sym->c.func.node);

   if (n_param != expected_nparam) {
      codegenError("Wrong number of arguments in call to %s. Expected %d but got %d.",
                   label, expected_nparam, n_param);
   }

   instructionPrintf(c, node->line_number, "call %s", label);
   c->stack_offset += pointerSizeBits() / 8;

   while (bufCount(c->stack) != stack_top) {
      stackPop(c, Reg_RBX);
   }
   // TODO: Restore registers. Not necessary at the moment because of DDCG

   if (target == Target_STACK) {
      stackPushReg(c, Reg_RAX);
   }

   AstNode* funcdef = sym->c.func.node;
   Assert(funcdef->type == Ast_FUNCDEF);

   expr_type->c = funcdef->child->ctype;
   expr_type->location = registerLocation(Reg_RAX);
}

void
emitExpression(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   if (nodeIsExpression(node)) {
      AstNode* child0 = node->child;

      switch (node->type) {
         case Ast_FUNCCALL: {
            emitFunctionCall(c, node, expr_type, target);
         } break;
         case Ast_NUMBER: {
            int bits = 32;
            switch (target) {
               case Target_ACCUM: {
                  Assert(bits < 64);
                  instructionPrintf(
                                    c,
                                    node->line_number,
                                    "mov %s, %d",
                                    locationString(c, registerLocation(Reg_RAX), bits),
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
               expr_type->c.type = Type_INT;
               expr_type->location = (Location) {
                  .type = Location_IMMEDIATE,
                  .immediate_value = node->tok->value,
               };
            }
         } break;
         case Ast_ID: {
            emitIdentifier(c, node, expr_type, target);
         } break;
         case Ast_STRUCT_MEMBER_ACCESS: {
            emitStructMemberAccess(c, node, expr_type, target);
         } break;
         // Assignment expressions
         case Ast_ASSIGN_EXPR: {
            AstNode* lhs = node->child;
            AstNode* rhs = lhs->next;
            Token* op = node->tok;

            ExprType lhs_type = Zero;
            codegenEmit(c, lhs, &lhs_type, Target_NONE); // Fill the location
            int bits = typeBits(&lhs_type.c);
            ExprType rhs_type = Zero;
            codegenEmit(c, rhs, &rhs_type, Target_ACCUM);  // TODO: Don't emit mov if rhs is immediate.
            Assert (typeBits(&lhs_type.c) == typeBits(&rhs_type.c));
            if (op->value == '=') {
               movOrCopy(c, node->line_number, lhs_type.location, rhs_type.location, bits);
            }
            else {
               Assert(bits < 64);
               // TODO: Check for arithmetic type here.
               instructionPrintf(c, node->line_number, "mov %s, %s",
                                 locationString(c, registerLocation(Reg_RBX), bits),
                                 locationString(c, lhs_type.location, bits));
               switch (op->value) {
                  case ASSIGN_INCREMENT: {
                     instructionReg(c, 0, "add %s, %s", bits, Reg_RBX, Reg_RAX);
                  } break;
                  default: {
                     NotImplemented("Different assignment expressions");
                  }
               }

               instructionPrintf(c, node->line_number, "mov %s, %s",
                                 locationString(c, lhs_type.location, bits),
                                 locationString(c, registerLocation(Reg_RBX), bits));

               if (target == Target_ACCUM) {
                  instructionPrintf(c, node->line_number, "mov %s, %s",
                                    locationString(c, registerLocation(Reg_RAX), bits),
                                    locationString(c, lhs_type.location, bits));
               }
            }
         } break;
         case Ast_POSTFIX_INC:
         case Ast_POSTFIX_DEC: {
            // TODO: Check for type of postfix
            AstNode* expr = node->child;
            ExprType local_etype = Zero;
            emitExpression(c, expr, &local_etype, Target_STACK);
            if (local_etype.location.type == Location_IMMEDIATE) {
               codegenError("Attempting to increment an rvalue.");
            }
            emitArithBinaryExpr(c, Ast_ADD, NULL, expr, c->one, Target_ACCUM);

            Location var = local_etype.location;

            instructionPrintf(c, node->line_number, "mov %s, %s",
                              locationString(c, var, typeBits(&local_etype.c)),
                              locationString(c, registerLocation(Reg_RAX), typeBits(&local_etype.c)));
            if (target == Target_STACK) {
               // Result is already on the stack.
            }
            else if (target == Target_ACCUM) {
               // Return old value.
               stackPop(c, Reg_RAX);
            }
            if (expr_type) {
               *expr_type = local_etype;

            }
         } break;
         case Ast_ADDRESS: {
            AstNode* expr = node->child;
            ExprType* et = AllocType(c->arena, ExprType);
            emitExpression(c, expr, et, Target_NONE);

            ExprType result = Zero;

            switch (et->c.type) {
               case Type_POINTER: {
                  // result.c = *et.c.pointer.pointee;
                  NotImplemented("address of a pointer");
               } break;
               case Type_AGGREGATE: {
                  if (et->location.type == Location_STACK) {
                     result.c.type = Type_POINTER;
                     result.c.pointer.pointee = et;
                  } else {
                     NotImplemented("Aggregate somewhere other than the stack.");
                  }
               } break;
               case Type_FUNC: {
                  NotImplemented("address of func");
               } break;
               case Type_ARRAY: {
                  NotImplemented("address of array");
               } break;
               default: {
                  codegenError("Cannot take the address of this type of expression.");
               }
            }

            if (target != Target_NONE) {
               Location* loc = &result.c.pointer.pointee->location;
               switch (loc->type) {
                  case Location_STACK: {
                     // TODO lea
                     instructionPrintf(c, node->line_number, "mov rax, rsp");
                     instructionPrintf(c, 0, "add rax, %d", c->stack_offset - loc->offset);
                  } break;
                  default: {
                     NotImplemented("Address of something not on the stack");
                  }
               }
               if (target == Target_STACK) {
                  stackPushReg(c, Reg_RAX);
                  result.location = (Location){ .type = Location_STACK, .offset = c->stack_offset };
               }
               else {
                  result.location = (Location){ .type = Location_REGISTER, .reg = Reg_RAX };
               }
               *expr_type = result;
            }
         } break;
         // Binary operators
         default: {
            AstNode* child1 = child0->next;
            if (node->type == Ast_ADD ||
                node->type == Ast_SUB ||
                node->type == Ast_MUL ||
                node->type == Ast_DIV) {
               emitArithBinaryExpr(c, node->type, expr_type, child0, child1, target);
            }
            else if (node->type == Ast_LESS ||
                     node->type == Ast_LEQ ||
                     node->type == Ast_GREATER ||
                     node->type == Ast_GEQ ||
                     node->type == Ast_NOT_EQUALS ||
                     node->type == Ast_EQUALS) {
               AstNode* left = node->child;
               AstNode* right = node->child->next;
               ExprType left_type = {0};
               ExprType right_type = {0};
               codegenEmit(c, right, &right_type, Target_STACK);
               codegenEmit(c, left, &left_type, Target_ACCUM);
               stackPop(c, Reg_RBX);

               u64 line = left->line_number;
               instructionReg(c, line, "cmp %s, %s", typeBits(&left_type.c), Reg_RAX, Reg_RBX);

               char* instr = 0;
               switch(node->type) {
                  case Ast_EQUALS: { instr = "sete %s"; } break;
                  case Ast_LESS: { instr = "setl %s"; } break;
                  case Ast_LEQ: { instr = "setle %s"; } break;
                  case Ast_GREATER: { instr = "setg %s"; } break;
                  case Ast_GEQ: { instr = "setge %s"; } break;
                  case Ast_NOT_EQUALS: { instr = "setne %s"; } break;
                  default: { InvalidCodePath; } break;
               }
               instructionReg(c, line, instr, 8 /* SETCC operates on byte registers*/, Reg_RAX);

               if (target == Target_STACK) {
                  stackPushReg(c, Reg_RAX);
               }
            }
            else {
               NotImplemented("Missing codegen for expression AST node.");
            }
         }
      }
   }
   else {
      InvalidCodePath;
   }
}

void
emitConditionalJump(Codegen* c, AstNode* cond, char* then, char* els) {
   ExprType expr_type = {0};
   // codegenEmit(c, cond, &expr_type, Target_ACCUM);
   switch (cond->type) {
      case Ast_LESS:
      case Ast_LEQ:
      case Ast_GREATER:
      case Ast_GEQ:
      case Ast_NOT_EQUALS:
      case Ast_EQUALS: {
         AstNode* left = cond->child;
         AstNode* right = cond->child->next;
         ExprType left_type = {0};
         ExprType right_type = {0};
         codegenEmit(c, right, &right_type, Target_STACK);
         codegenEmit(c, left, &left_type, Target_ACCUM);
         stackPop(c, Reg_RBX);
         if (typeBits(&left_type.c) != typeBits(&right_type.c)) {
            NotImplemented("Promotion rules");
         }
         instructionReg(c, cond->line_number, "cmp %s, %s", typeBits(&left_type.c), Reg_RAX, Reg_RBX);
         switch(cond->type) {
            case Ast_EQUALS: { instructionPrintf(c, 0, "je %s", then); } break;
            case Ast_LESS: { instructionPrintf(c, 0, "jl %s", then); } break;
            case Ast_LEQ: { instructionPrintf(c, 0, "jle %s", then); } break;
            case Ast_GREATER: { instructionPrintf(c, 0, "jg %s", then); } break;
            case Ast_GEQ: { instructionPrintf(c, 0, "jge %s", then); } break;
            case Ast_NOT_EQUALS: { instructionPrintf(c, 0, "jne %s", then); } break;
            default: InvalidCodePath; break;
         }
         instructionPrintf(c, 0, "jmp %s", els);
      } break;
      default: {
         codegenEmit(c, cond, &expr_type, Target_ACCUM);
         instructionReg(c, cond->line_number, "cmp %s, %s", typeBits(&expr_type.c), Reg_RAX, Reg_RBX);
         instructionPrintf(c, 0, "jne %s", then);
         instructionPrintf(c, 0, "jmp %s", els);
      } break;
   }
}

// forward decl
void emitCompoundStatement(Codegen* c, AstNode* compound, EmitTarget target);

void
emitDeclaration(Codegen* c, AstNode* node, EmitTarget target) {
   AstNode* specifier = node->child;
   AstNode* declarator = specifier->next;
   AstNode* rhs = declarator->next;

   // TODO: Emit warning for empty declarations.

   u64 bits = 0;

   // Figure out the size of the declaration from the type specifier.
   if (specifier->ctype.type != Type_AGGREGATE) {
      bits = typeBits(&specifier->ctype);
   }
   else {
      char* tag_str = specifier->ctype.aggr.tag;
      AstNode* decls = specifier->ctype.aggr.decls;
      // TODO: Anonymous structs

      if (tag_str && decls) {
         Assert(typeBits(&specifier->ctype) != 0);
         bits = typeBits(&specifier->ctype);

         if (findTag(c, tag_str)) {
            codegenError("Struct identifier redeclared: %s");
         }

         ExprType entry = {
            .c = specifier->ctype,
            // TODO: tag table should have different entry.
            .location = { .type = Location_STACK, .offset = 0 /*struct tag does not have a place*/ },
         };

         u64 offset = 0;
         for (AstNode* decl = decls;
              decl;
              decl = decl->next) {
            AstNode* spec = decl->child;
            AstNode* declarator = spec->next;
            char* member_id = declarator->child->tok->cast.string;

            Assert(offset % 8 == 0);
            struct StructMember member = { member_id, &spec->ctype, offset/8 };
            bufPush(entry.c.aggr.members, member);

            Ctype* ctype = NULL;
            if (declarator->is_pointer) {
               DevBreak("struct member is pointer");
            }
            else {
               ctype = &spec->ctype;
            }

            offset += typeBits(ctype);
            offset = AlignPow2(offset, 8);
         }
         Assert(typeBits(&specifier->ctype) == offset);

         symInsert(&c->scope->tag_table, tag_str, entry);
      }
      else if (tag_str && declarator->type != Ast_NONE) {
         ExprType* entry = findTag(c, tag_str);
         if (!entry) {
            codegenError("Use of undeclared struct %s", tag_str);
         }

         if (declarator->is_pointer)
            bits = pointerSizeBits();
         else
            bits = typeBits(&entry->c);
      }
   }

   Assert (bits != 0);

   // Declare a new symbol.
   if (declarator->type != Ast_NONE) {
      AstNode* ast_id = declarator->child;
      char* id_str = ast_id->tok->cast.string;
      if (symGet(&c->scope->symbol_table, id_str) != NULL) {
         codegenError("Symbol redeclared in scope");
      }
      // TODO: top level declarations
      stackPushOffset(c, bits/8);

      ExprType* entry = symInsert(&c->scope->symbol_table,
                                  id_str,
                                  (ExprType){
                                     .c = specifier->ctype,
                                     .location = { .type = Location_STACK, .offset = c->stack_offset },
                                  });

      Ctype ctype;

      if (declarator->is_pointer) {
         ctype.type = Type_POINTER;
         ctype.pointer.pointee = entry;
      }
      else {
         ctype = specifier->ctype;
      }

      if (isLiteral(rhs)) {               // Literal right-hand-side
         // TODO: Non-integer values.
         int value = rhs->tok->value;

         switch (typeBits(&ctype)) {
            case 64: {
               instructionPrintf(c, node->line_number, "mov QWORD [ rsp ], 0x%x", value);
            } break;
            case 32: {
               instructionPrintf(c, node->line_number, "mov DWORD [ rsp ], 0x%x", value);
            } break;
            case 16: {
               instructionPrintf(c, node->line_number, "mov WORD [ rsp ], 0x%x", value);
            } break;
            case 8: {
               instructionPrintf(c, node->line_number, "mov BYTE [ rsp ], 0x%x", value);
            } break;
            default: {
               InvalidCodePath;
            } break;
         }
      }
      else if (rhs->type != Ast_NONE) {    // Non-literal right-hand-side.
         ExprType type = Zero;  // TODO: Here is probably where we want to specify the type in case there is an initializer list on the other side.
         emitExpression(c, rhs, &type, Target_ACCUM);
         movOrCopy(c, rhs->line_number, entry->location, type.location, typeBits(&type.c));
      }
      else {
         // TODO: scc initializes to zero by default.
      }

   }
}

void
emitStatement(Codegen* c, AstNode* stmt, EmitTarget target) {
   switch (stmt->type) {
      case Ast_COMPOUND_STMT : {
         emitCompoundStatement(c, stmt, target);
      } break;
      case Ast_RETURN: {
         // Emit code for the expression and move it to rax.
         if (stmt->child) {
            ExprType et = {0};
            emitExpression(c, stmt->child, &et, Target_ACCUM);
            instructionPrintf(c, stmt->line_number, "jmp .func_end");
         }
      } break;
      case Ast_DECLARATION: {
         emitDeclaration(c, stmt, target);
      } break;
      case Ast_IF: {
         AstNode* cond = stmt->child;
         AstNode* then = cond->next;
         AstNode* els = then ? then->next : NULL;
         char then_label[1024] = {0};
         char else_label[1024] = {0};
         snprintf(then_label, 1024, ".then%d", c->scope->if_count);
         snprintf(else_label, 1024, ".else%d", c->scope->if_count++);
         emitConditionalJump(c, cond, then_label, else_label);
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
         int loop_id = c->scope->if_count++;
         pushScope(c);
         AstNode* decl = stmt->child;
         AstNode* control = decl->next;
         AstNode* after = control->next;
         AstNode* body = after->next;
         char loop_label[1024] = {0}; {
            snprintf(loop_label, sizeof(loop_label), ".loop%d", loop_id);
         }
         char body_label[1024] = {0}; {
            snprintf(body_label, sizeof(loop_label), ".body%d", loop_id);
         }
         char end_label[1024] = {0}; {
            snprintf(end_label, sizeof(end_label), ".end%d", loop_id);
         }
         b32 after_is_control = (control->type == Ast_NONE);
         if (decl->type != Ast_NONE) { emitStatement(c, decl, Target_ACCUM); }

         instructionPrintf(c, 0, "%s:", loop_label);
         if (!after_is_control) {
            emitConditionalJump(c, control, body_label, end_label);
         }

         instructionPrintf(c, 0, "%s:", body_label);
         if (body->type != Ast_NONE) {
            emitStatement(c, body, Target_ACCUM);
         }
         if (after->type != Ast_NONE) {
            emitStatement(c, after, Target_ACCUM);
         }
         instructionPrintf(c, 0, "jmp %s", loop_label);
         instructionPrintf(c, 0, "%s:", end_label);
         popScope(c);
      } break;
      default: {
         // Expression statements
         if (nodeIsExpression(stmt)) {
            emitExpression(c, stmt, NULL, target);
         }
         else {
            NotImplemented("Missing codegen for AST statement node.");
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
      b32 is_last = stmt->next == NULL;
      emitStatement(c, stmt, is_last ? target : Target_NONE);
      stmt = stmt->next;
   }

   popScope(c);
}


void
emitFunctionDefinition(Codegen* c, AstNode* node, EmitTarget target) {
   AstNode* specifier   = node->child;
   AstNode* declarator  = specifier->next;
   AstNode* compound    = declarator->next;

   if (specifier && declarator && compound) {
      char *func_name = declarator->child->tok->cast.string;

      ExprType* entry = findSymbol(c, func_name);
      if (entry) {
         codegenError("Redefining function %s", func_name);
      } else {
         symInsert(&c->scope->symbol_table,
                   func_name,
                   (ExprType) {
                      .c= node->ctype,
                      .location = { .type = Location_IMMEDIATE, .offset = 0 }, // TODO: location for functions
                   });
      }

      instructionPrintf(c, node->line_number, "global %s", func_name);
      instructionPrintf(c, node->line_number, "%s:", func_name);
      instructionPrintf(c, node->line_number, "push rbp");
      c->stack_offset += 8;
      instructionPrintf(c, node->line_number, "mov rbp, rsp");

      // Helper when running in a debugger. Break on function entry.
      // instructionPrintf(c, 0, "int 3");

      // Push
      pushScope(c);

      AstNode* params = declarator->child->next;
      if (params) {
         AstNode* p = params;
         u64 n_param = 0;
         u64 offset = 16;  // 8 bytes for call instruction, 8 bytes for function prelude
         while (p) {
            Assert (p->type == Ast_PARAMETER);
            AstNode* param_type_spec = p->child;
            AstNode* param_declarator = param_type_spec->next;
            char* id_str = param_declarator->child->tok->cast.string;

            Assert (param_type_spec && param_type_spec->type == Ast_DECLARATION_SPECIFIER);
            Assert (param_declarator && param_declarator->child->type == Ast_ID);

            Location param_loc = popParameter(c, &param_type_spec->ctype, n_param++, &offset);

            ExprType entry = {
               .c = param_type_spec->ctype,
               .location = param_loc,
            };
            symInsert(&c->scope->symbol_table, id_str, entry);

            p = p->next;
         }
      }

      emitCompoundStatement(c, compound, Target_ACCUM);

      //i64 stack = c->scope->stack_size;

      // TODO: Align the stack again.
      // stack = AlignPow2(stack, 16);
      // TODO: On mac OS, the stack needs to be aligned to 32 or 64 byte boundaries when m256 or m512 values are passed on the stack.

      instructionPrintf(c, 0, ".func_end:");

      while (bufCount(c->stack) > 0)  {
         stackPop(c, Reg_RBX);
      }

      popScope(c);

      // Restore non-volatile registers.

      instructionPrintf(c, 0, "pop rbp");
      instructionPrintf(c, 0, "ret");
   }
   else {
      codegenError("Funcdef: Invalid node in the tree.");
   }
}

void
codegenTranslationUnit(Codegen* c, AstNode* node) {
   pushScope(c);
   while (node) {
      if (node->type == Ast_FUNCDEF) {
         codegenEmit(c, node, NULL, Target_NONE);
      }
      else if (node->type == Ast_DECLARATION) {
         codegenEmit(c, node, NULL, Target_NONE);
      }
      else {
         NotImplemented ("Top level declarations.");
      }

      node = node->next;
   }
   popScope(c);
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
