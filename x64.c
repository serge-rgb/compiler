static Machine* g_mach;
static FILE* g_asm;

void        machMov(Machine* m, ExprType* dst, ExprType* src);
void        machStackPushReg(Machine* m, RegisterEnum reg);
void        machStackPushImm(Machine* m, ExprType* et, i64 val);
void        machStackPushOffset(Machine* m, u64 bytes);
ExprType*   machHelperInt64();
ExprType*   machHelperInt32();
ExprType*   machHelperInt();
ExprType*   machImmediateFromToken(Token* tok);
ExprType*   machImmediateInt(u64 value);
ExprType*   machAccumInt32();
ExprType*   machAccumInt64();
ExprType*   machAccumInt();

struct Register {
   char* reg;
   char* reg_32;
   char* reg_8;
   b8    is_volatile;
} typedef Register;

// Must correspond to RegisterEnum.
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

struct Machine
{
   u32         config;   // CodegenConfigFlags enum

   StackValue* stack;  // Stack allocation / de-allocation is done on a per-function basis.
   i64         stack_offset;  // # Bytes from the bottom of the stack to RSP.
};

void
codegenError(char* msg, ...) {
   va_list args;
   va_start(args, msg);
   char buffer[LineMax] = {0};
   vsnprintf(buffer, LineMax, msg, args);
   fprintf(stderr, "Codegen error: %s\n", buffer);
   va_end(args);

   Assert (!"Codegen error");

   volatile b32 fatal = true;  // Set to false in debugger

   if (fatal) {
      exit(-1);
   }
}

void
setupVolatility(Machine* m) {
   int* volatile_regs = NULL;
   if (   (m->config & Config_TARGET_MACOS)
       || (m->config & Config_TARGET_LINUX)) {
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
   else if (m->config & Config_TARGET_WIN) {
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

char*
locationString(Machine* m, Location r, int bits) {
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
         char tmp_string[PathMax] = {0};
         sz size = snprintf(tmp_string, ArrayCount(tmp_string), "0x%x", (int)r.immediate_value);
         if (size > ArrayCount(tmp_string)) {
            codegenError("Immediate value too large.");
         }
         res = getString(tmp_string);
      } break;
      case Location_STACK: {
         if (r.offset > m->stack_offset) {
            codegenError("A stack variable has an invalid offset.");
         }
         u64 rsp_relative = m->stack_offset - r.offset;

#define ResultSize 64
         char tmp_string[ResultSize] = {0};
         if (bits == 8)
            snprintf(tmp_string, ArrayCount(tmp_string), "BYTE [ rsp + 0x%x ]", (int)rsp_relative);
         else if (bits == 16)
            snprintf(tmp_string, ArrayCount(tmp_string), "WORD [ rsp + 0x%x ]", (int)rsp_relative);
         else if (bits == 32)
            snprintf(tmp_string, ArrayCount(tmp_string), "DWORD [ rsp + 0x%x ]", (int)rsp_relative);
         else if (bits == 64)
            snprintf(tmp_string, ArrayCount(tmp_string), "QWORD [ rsp + 0x%x ]", (int)rsp_relative);
         else
            InvalidCodePath;
         res = getString(tmp_string);
      } break;
      case Location_STACK_FROM_REG: {
#define ResultSize 64
         char tmp_string[ResultSize] = {0};
         char* reg_str = g_registers[r.reg].reg;
         if (bits == 8)
            snprintf(tmp_string, ArrayCount(tmp_string), "BYTE [ %s + 0x%x ]", reg_str, (int)r.reg_offset);
         else if (bits == 16)
            snprintf(tmp_string, ArrayCount(tmp_string), "WORD [ %s + 0x%x ]", reg_str, (int)r.reg_offset);
         else if (bits == 32)
            snprintf(tmp_string, ArrayCount(tmp_string), "DWORD [ %s + 0x%x ]", reg_str, (int)r.reg_offset);
         else if (bits == 64)
            snprintf(tmp_string, ArrayCount(tmp_string), "QWORD [ %s + 0x%x ]", reg_str, (int)r.reg_offset);
         else
            InvalidCodePath;
         res = getString(tmp_string);

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
instructionPrintf(char* asm_line, ...) {
   va_list args;
   va_start(args, asm_line);

   char out_asm[LineMax] = {0};

   int written = vsnprintf(out_asm, LineMax, asm_line, args);
   if (written >= LineMax - 2) {  // Counting two extra character for the new line and 0 terminator.
      codegenError("LineMax is not sufficient for instruction length.");
   }
   if (out_asm[written-1] != '\n') {
      out_asm[written] = '\n';
   }
   fwrite(out_asm, 1, strlen(out_asm), g_asm);

   printf("%s", out_asm);

   va_end(args);
}

Location
registerLocation(RegisterEnum reg) {
   Location loc = { .type = Location_REGISTER, .reg = reg };
   return loc;
}


void
instructionReg(Machine* m, char* asm_line, int bits, ...) {
#define MaxRegs 2
   va_list args;
   va_start(args, bits);
   RegisterEnum reg = Reg_RAX;

   RegisterEnum regs[MaxRegs] = Zero;
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
         instructionPrintf(asm_line);
      } break;
      case 1: {
         instructionPrintf(asm_line,
                           locationString(m, registerLocation(regs[0]), bits));
      } break;
      case 2: {
         instructionPrintf(asm_line,
                           locationString(m, registerLocation(regs[0]), bits),
                           locationString(m, registerLocation(regs[1]), bits));
      } break;
      default: {
         InvalidCodePath;
      } break;
   }
#undef MaxRegs
}

// Forward declarations. (TODO: are these still necessary after the x64 refactor?)
ExprType* findTag(Codegen* c, char* name);
ExprType* findSymbol(Codegen* c, char* name);

void
machStackPop(Machine* m, ExprType* et) {
   Assert(et->location.type == Location_REGISTER);
   StackValue s = bufPop(m->stack);
   switch (s.type) {
      case Stack_QWORD: {
         instructionReg(m, "pop %s", 64, et->location.reg);
         m->stack_offset -= 8;
      } break;
      case Stack_OFFSET: {
         m->stack_offset -= s.offset;
         instructionPrintf("add rsp, %d", s.offset);
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

   Machine* m = c->m;

   if (isRealType(&etype->c)) {
      NotImplemented("Floating parameters.");
   }

   if ((m->config & Config_TARGET_LINUX) || (m->config & Config_TARGET_MACOS)) {
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
         ExprType reg = {
            .c = etype->c,
            .location = registerLocation(r),
         };
         Break;
         machMov(m, &reg, machAccumInt64());
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
            if (isIntegerType(&etype->c) || etype->c.type == Type_POINTER) {
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
         }      }
      else {
         machStackPushOffset(c->m, typeBits(&etype->c) / 8);
         loc.type = Location_STACK;
         loc.offset = c->m->stack_offset;
      }
      ExprType reg = {
         .c = etype->c,
         .location = loc,
      };
      machMov(m, &reg, etype);
   }
}

Location
popParameter(Codegen* c, Ctype* ctype, u64 n_param, u64* offset) {
   Location loc = Zero;

   if (isRealType(ctype)) {
      NotImplemented("float params");
   }

   if (typeBits(ctype) <= 64) {
      if ((c->m->config & Config_TARGET_MACOS) || (c->m->config & Config_TARGET_LINUX)) {
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
            loc = (Location){ .type = Location_STACK, .offset = c->m->stack_offset - *offset };
            *offset += typeBits(ctype);
         }
      }
      else if (c->m->config & Config_TARGET_WIN){
         if (n_param < 4) {
            if (isIntegerType(ctype) || ctype->type == Type_POINTER) {
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
            loc = (Location){ .type = Location_STACK, .offset = c->m->stack_offset - *offset };
            *offset += typeBits(ctype);
         }
      }
   }
   else {
      loc = (Location){ .type = Location_STACK, .offset = c->m->stack_offset - *offset };
      *offset += typeBits(ctype);
   }
   return loc;
}

void
machTestAndJump(Machine* m, u32 bits, char* then, char* els) {
   instructionReg(m, "test %s, %s", bits, Reg_RAX, Reg_RAX);
   instructionPrintf("jne %s", then);
   instructionPrintf("jmp %s", els);
}

void
machCmp(Machine* m, ExprType* dst, ExprType* src) {
   u32 bits = typeBits(&dst->c);
   instructionPrintf("cmp %s, %s",
                     locationString(m, dst->location, bits),
                     locationString(m, src->location, bits));
}

// Compare the accumulator with the top of the stack.
void
machCmpJmp(Machine* m, AstType type, u32 bits, char* then, char* els) {
   machStackPop(m, machHelperInt64());
   instructionReg(m, "cmp %s, %s", bits, Reg_RAX, Reg_RBX);
   switch (type) {
      case Ast_EQUALS:     { instructionPrintf("je %s", then); } break;
      case Ast_LESS:       { instructionPrintf("jl %s", then); } break;
      case Ast_LEQ:        { instructionPrintf("jle %s", then); } break;
      case Ast_GREATER:    { instructionPrintf("jg %s", then); } break;
      case Ast_GEQ:        { instructionPrintf("jge %s", then); } break;
      case Ast_NOT_EQUALS: { instructionPrintf("jne %s", then); } break;
      default:             { InvalidCodePath; }
   }
   instructionPrintf("jmp %s", els);
}

void
machCmpSetAccum(Machine* m, AstType type) {
   char* instr = 0;
   switch(type) {
      case Ast_EQUALS: { instr = "sete %s"; } break;
      case Ast_LESS: { instr = "setl %s"; } break;
      case Ast_LEQ: { instr = "setle %s"; } break;
      case Ast_GREATER: { instr = "setg %s"; } break;
      case Ast_GEQ: { instr = "setge %s"; } break;
      case Ast_NOT_EQUALS: { instr = "setne %s"; } break;
      default: { InvalidCodePath; } break;
   }
   instructionReg(m, instr, 8 /* SETCC operates on byte registers*/, Reg_RAX);
}

ExprType*
machAccumInt8() {
   static ExprType accum = {
      .c = { .type = Type_CHAR },
      .location = { .type = Location_REGISTER, .reg = Reg_RAX },
   };

   return &accum;
}

ExprType*
machAccumInt32() {
   static ExprType accum = {
      .c = { .type = Type_INT },
      .location = { .type = Location_REGISTER, .reg = Reg_RAX },
   };

   return &accum;
}

ExprType*
machAccumInt64() {
   static ExprType accum = {
      .c = { .type = Type_LONG },
      .location = { .type = Location_REGISTER, .reg = Reg_RAX },
   };

   return &accum;
}

ExprType*
machAccumInt(u32 bits) {
   ExprType* result = NULL;
   switch (bits) {
      case 64: {
         result = machAccumInt64();
      } break;
      case 32: {
         result = machAccumInt32();
      } break;
      case 8: {
         result = machAccumInt8();
      } break;
      default: {
         NotImplemented("machAccumInt()");
      } break;
   }
   return result;
}

// NOTE: Every use of the machImmediate* functions invalidates the previous call.

ExprType*
machImmediateFromToken(Token* tok) {
   static ExprType imm = {
      .c = { .type = Type_INT },
      .location = { .type = Location_IMMEDIATE },
   };

   if (tok->type == TType_FLOAT) {  // Float lhs, integer literal rhs
      imm.c.type = Type_DOUBLE;
      imm.location.cast.real64 = tok->cast.real64;
   }
   else if (tok->type == TType_NUMBER)  {
      imm.c.type = Type_INT;
      imm.location.immediate_value= tok->value;
   }
   return &imm;
}


ExprType*
machImmediateInt(u64 value) {
   static ExprType imm = {
      .c = { .type = Type_INT },
      .location = { .type = Location_IMMEDIATE },
   };
   imm.location.immediate_value = value;
   return &imm;
}

ExprType*
machHelperInt8() {
   static ExprType helper = {
      .c = { .type = Type_CHAR },
      .location = { .type = Location_REGISTER, .reg = Reg_RBX },
   };

   return &helper;
}

ExprType*
machHelperInt32() {
   static ExprType helper = {
      .c = { .type = Type_INT },
      .location = { .type = Location_REGISTER, .reg = Reg_RBX },
   };

   return &helper;
}

ExprType*
machHelperInt64() {
   static ExprType helper = {
      .c = { .type = Type_LONG },
      .location = { .type = Location_REGISTER, .reg = Reg_RBX },
   };

   return &helper;
}

ExprType*
machHelperInt(u32 bits) {
   ExprType* helper = NULL;
   switch (bits) {
      case 8: {
         helper = machHelperInt8();
      } break;
      case 32: {
         helper = machHelperInt32();
      } break;
      case 64: {
         helper = machHelperInt32();
      } break;
      default: {
         NotImplemented("machHelperInt()");
      }
   }
   return helper;
}

void
machMov(Machine* m, ExprType* dst, ExprType* src) {
   u32 bits = typeBits(&dst->c);
   Assert(bits);

   if (bits <= 64) {
      if (dst->location.type == Location_REGISTER
          && bits <= 16) {
         Assert (!isRealType(&dst->c));
         instructionPrintf("xor %s, %s",
                           locationString(m, dst->location, 64),
                           locationString(m, dst->location, 64));
      }
      if (isRealType(&dst->c) &&
          isImmediate(src)) {
         switch(typeBits(&dst->c)) {
            case 64: {
               instructionPrintf("mov %s, __float64__(%f)",
                                 locationString(m, dst->location, bits),
                                 src->location.cast.real64);
            } break;
            case 32: {
               instructionPrintf("mov %s, __float32__(%f)",
                                 locationString(m, dst->location, bits),
                                 src->location.cast.real64);
            } break;
            default:
               codegenError("Invalid floating point variable size");
         }
      }
      else {
         instructionPrintf("mov %s, %s",
                           locationString(m, dst->location, bits),
                           locationString(m, src->location, bits));
      }
   }
   else /* bits > 64*/ {
      if (dst->location.type == Location_REGISTER) {
         NotImplemented("Big copy - into register.");
      }
      else if (dst->location.type == Location_STACK) {
         int bytes = bits/8;
         if (src->location.type == Location_REGISTER) {
            instructionReg(m, "mov rsi, %s", 64, src->location);
         }
         else if (src->location.type == Location_STACK) {
            instructionPrintf("lea rsi, [ rsp + %d ]", m->stack_offset - src->location.offset);
         }
         instructionPrintf("mov rdi, rsp");
         if (dst->location.offset != m->stack_offset) {
            instructionPrintf("add rdi, %d", m->stack_offset - dst->location.offset);
         }
         instructionPrintf("mov rcx, 0x%x", bytes);
         instructionPrintf("rep movsb");
      }
      else {
         InvalidCodePath;
      }
   }
}

void
machAdd(Machine* m, ExprType* dst, ExprType* src) {
   u32 bits = typeBits(&dst->c);
   Assert(!isRealType(&dst->c));
   instructionPrintf("add %s, %s",
                     locationString(m, dst->location, bits),
                     locationString(m, src->location, bits));
}

void
machSub(Machine* m, ExprType* dst, ExprType* src) {
   u32 bits = typeBits(&dst->c);
   Assert(!isRealType(&dst->c));
   instructionPrintf("sub %s, %s",
                     locationString(m, dst->location, bits),
                     locationString(m, src->location, bits));
}

void
machMul(Machine* m, ExprType* dst, ExprType* src) {
   u32 bits = typeBits(&dst->c);
   if (!isRealType(&dst->c)) {
      instructionPrintf("imul %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));

   }
   else {
      NotImplemented("float mul");
   }
}

void
machDiv(Machine* m, ExprType* dst, ExprType* src) {
   u32 bits = typeBits(&dst->c);
   if (!isRealType(&dst->c)) {
      instructionPrintf("idiv %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));

   }
   else {
      NotImplemented("float div");
   }
}

void
machMovAccum(Machine* m, ExprType* et , Token* rhs_tok)  {
   instructionPrintf("mov %s, %d",
                     locationString(m, registerLocation(Reg_RAX), typeBits(&et->c)),
                     rhs_tok->cast.int32);

   et->location = (Location) {
      .type = Location_REGISTER,
      .reg = Reg_RAX,
   };
}

// Puts the stack address in the accumulator.
void
machStackAddressInAccum(Machine* m, ExprType* entry) {
   Assert(entry->location.type == Location_STACK);
   instructionPrintf("lea rax, [rsp + %d]", m->stack_offset - entry->location.offset);
}

void
machCall(Machine* m, char* label) {
   instructionPrintf("call %s", label);
}

void
machStackPushReg(Machine* m, RegisterEnum reg) {
   instructionPrintf("push %s", locationString(m, registerLocation(reg), 64));
   m->stack_offset += 8;
   bufPush(m->stack, (StackValue){ .type = Stack_QWORD });
}

void
machStackPushImm(Machine* m, ExprType* et, i64 val) {
   instructionPrintf("push %d", val);
   m->stack_offset += 8;
   bufPush(m->stack, (StackValue){ .type = Stack_QWORD });
   et->location = (Location) {
      .type = Location_STACK,
      .reg = m->stack_offset,
   };
}

void
machStackPushOffset(Machine* m, u64 bytes) {
   Assert(bytes);
   instructionPrintf("sub rsp, %d", bytes);
   m->stack_offset += bytes;
   StackValue val = { .type = Stack_OFFSET, .offset = bytes };
   bufPush(m->stack, val);
}

void
machFinish(void) {
   char* end =
      "int 3\n"
      "ret\n";
   fwrite(end, 1, strlen(end), g_asm);
   fclose(g_asm);
}

void
machInit(Machine* m) {
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

   setupVolatility(m);
}

void
machFunctionPrelude(Machine* m, char* func_name) {
   instructionPrintf("global %s", func_name);
   instructionPrintf("%s:", func_name);
   instructionPrintf("push rbp");
   instructionPrintf("mov rbp, rsp");

   m->stack_offset += 16;  // 8 for rbp. 8 for return address from call
}

void
machFunctionEpilogue(Machine* m) {
   instructionPrintf(".func_end:");

   while (bufCount(m->stack) > 0)  {
      machStackPop(m, machHelperInt64());
   }

   // Restore non-volatile registers.

   instructionPrintf("pop rbp");
   m->stack_offset -= 8;

   instructionPrintf("ret");
   m->stack_offset -= 8;
}

void
machLabel(char* label) {
   instructionPrintf("%s:", label);
}

void
machJumpToLabel(char* label) {
   instructionPrintf("jmp %s", label);
}

void
machAddressOf(Machine* m, ExprType* dst, Location* loc) {
   switch (loc->type) {
      case Location_STACK: {
         instructionPrintf("mov rax, rsp");
         instructionPrintf("add rax, %d", m->stack_offset - loc->offset);
      } break;
      default: {
         NotImplemented("Address of something not on the stack");
      }
   }
}
