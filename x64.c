static Machine* g_mach;
static FILE* g_asm;

struct MachineX64 {
   Machine machine;  // Space for function pointers.

   u32         config;   // MachineConfigFlags
   StackValue* s_stack;  // Stack allocation / de-allocation is done on a per-function basis.
   i64         stack_offset;  // # Bytes from the bottom of the stack to RSP.

   // Parameter passing state
   u32 intParamIdx;
   RegisterEnum paramIntegerRegs[6];

   u32 floatParamIdx;
   RegisterEnum paramFloatRegs[8];
} typedef MachineX64;

void        x64Mov(MachineX64* m, ExprType* dst, ExprType* src);
Location    x64StackPushReg(MachineX64* m, RegisterEnum reg);
Location    x64StackPushImm(MachineX64* m, ExprType* et, i64 val);
Location    x64StackPushOffset(MachineX64* m, u64 bytes);
ExprType*   x64ImmediateInt(u64 value);
ExprType*   x64Accum64(int type /*Ctype.type*/ );
ExprType*   x64Helper64(int type /*Ctype.type*/ );

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

   { .reg = "xmm0", .reg_32 = "xmm0", .reg_8 = NULL },
   { .reg = "xmm1", .reg_32 = "xmm1", .reg_8 = NULL },
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
setupVolatility(MachineX64* m) {
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
locationString(MachineX64* m, Location r, int bits) {
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
instructionReg(MachineX64* m, char* asm_line, int bits, ...) {
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

// Forward declarations.
//ExprType* findTag(Scope* c, char* name);
//ExprType* findSymbol(Codegen* c, char* name);

void
x64StackPop(MachineX64* m, ExprType* et) {
   Assert(et->location.type == Location_REGISTER);
   StackValue s = bufPop(m->s_stack);
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

typedef enum
{
   Param_NO_CLASS,

   Param_POINTER,
   Param_INTEGER,
   Param_SSE,
   Param_SSEUP,
   Param_X87,
   Param_X87UP,
   Param_COMPLEX_X87,
   Param_MEMORY,
} SystemVParamClass;

SystemVParamClass
sysvClassifyNode(Scope* scope, Ctype ctype, int* num_registers) {
   SystemVParamClass class = Param_NO_CLASS;

   int n_regs = 1;

   if (ctype.type == Type_POINTER) {
      class = Param_POINTER;
   }
   else if (isIntegerType(&ctype)) {
      class = Param_INTEGER;
   }
   else if (isRealType(&ctype)) {
      class = Param_SSE;
   }
   else if (ctype.type == Type_AGGREGATE) {
      Tag* tag = findTag(scope, ctype.aggr.tag);

      u32 eightbyte_aligned = AlignPow2(typeBits(&ctype), Eightbytes(1));

      if (eightbyte_aligned > Eightbytes(2) || hasUnalignedMembers(tag)) {
         class = Param_MEMORY;
      }
      else {
         sz n_members = bufCount(tag->s_members);

         if (eightbyte_aligned == Eightbytes(2)) {
            n_regs = 2;
         }

         for (sz member_i = 0; member_i < n_members; ++member_i) {
            TagMember* member = &tag->s_members[member_i];
            SystemVParamClass member_class = sysvClassifyNode(scope, member->ctype, NULL);

            // Merge step
            if (class == Param_NO_CLASS) {
               class = member_class;
            }
            else if (member_class == Param_NO_CLASS) {
               // Do nothing
            }
            else if (class == Param_MEMORY || member_class == Param_MEMORY) {
               class = Param_MEMORY;
            }
            else if (class == Param_INTEGER || member_class == Param_INTEGER) {
               class = Param_INTEGER;
            }
            else if (class == Param_X87 || member_class == Param_X87) {
               class = Param_MEMORY;
            }
            else {
               class = Param_SSE;
            }
         }
      }
   }
   else {
      NotImplemented("SystemV parameter class");
   }

   if (num_registers) {
      *num_registers = n_regs;
   }
   return class;
}

RegisterEnum
sysVIntegerRegisterEnum(u64 n_param) {
   RegisterEnum r = Reg_RDI;
   switch (n_param) {
      case 0: { } break;
      case 1: { r = Reg_RSI; } break;
      case 2: { r = Reg_RDX; } break;
      case 3: { r = Reg_RCX; } break;
      case 4: { r = Reg_R8;  } break;
      case 5: { r = Reg_R9;  } break;
   }

   return r;
}

void
x64BeginFuncParams(MachineX64* m) {
   m->intParamIdx = 0;
   m->floatParamIdx = 0;
}

void
x64EndFuncParams(MachineX64* m) {

}

void
x64PushParameter(MachineX64* m, Scope* scope, u64 n_param, ExprType* etype) {
   if (isRealType(&etype->c)) {
      NotImplemented("Floating parameters.");
   }

   if ((m->config & Config_TARGET_LINUX) || (m->config & Config_TARGET_MACOS)) {
      Location loc = {0};
      if (isIntegerType(&etype->c)) {
         if (n_param < 6) {
            loc = registerLocation(sysVIntegerRegisterEnum(n_param));
            ExprType reg = {
               .c = etype->c,
               .location = loc,
            };
            x64Mov(m, &reg, etype);
         }
         else {
            NotImplemented("Pass integer param on stack");
         }
      }
      else if (etype->c.type == Type_AGGREGATE) {
         int n_regs = 0;
         SystemVParamClass class = sysvClassifyNode(scope, etype->c, &n_regs);

         if (class == Param_INTEGER && n_regs + m->intParamIdx > 6) {
            class = Param_MEMORY;
         }

         if (class == Param_SSE && n_regs + m->floatParamIdx > 8) {
            class = Param_MEMORY;
         }

         switch (class) {
            case Param_INTEGER: {

               u64 bits = typeBits(&etype->c);

               ExprType partial_argument = *etype;

               while (n_regs--) {
                  Location loc = registerLocation(m->paramIntegerRegs[m->intParamIdx++]);

                  ExprType reg = {
                     .c = (Ctype) {
                        .type = Type_LONG,
                     },
                     .location = loc,
                  };

                  if (bits <= 8) {
                     reg.c.type = Type_CHAR;
                  }
                  else if (bits <= 16) {
                     reg.c.type = Type_SHORT;
                  }
                  else if (bits <= 32) {
                     reg.c.type = Type_INT;
                  }

                  x64Mov(m, &reg, &partial_argument);


                  if (etype->location.type == Location_STACK) {
                     partial_argument.location.offset -= 8;
                  }
                  else {
                     NotImplemented("Shift big non-stack param.");
                  }
                  bits -= 64;
               }

            } break;
            default: {
               NotImplemented("Parameter class push");
            }
         }
      }
      else {
         NotImplemented("Non integer types");
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
            if (!isRealType(&etype->c)) {
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
         x64StackPushOffset(m, typeBits(&etype->c) / 8);
         loc.type = Location_STACK;
         loc.offset = m->stack_offset;
      }
      ExprType reg = {
         .c = etype->c,
         .location = loc,
      };
      x64Mov(m, &reg, etype);
   }
}

ExprType* x64Helper(MachineX64* m, int type /*Ctype.type*/, u32 bits); // fwd decl


Location
x64PopParameter(MachineX64* m, Scope* scope, Ctype* ctype, u64 n_param, u64* offset) {
   Location loc = Zero;

   if ((m->config & Config_TARGET_MACOS) || (m->config & Config_TARGET_LINUX)) {
      if (isIntegerType(ctype)) {
         if (n_param < 6) {
            loc = registerLocation(sysVIntegerRegisterEnum(n_param));
         }
         else {
            NotImplemented("Pass integer param on stack");
         }
      }
      else if (ctype->type == Type_AGGREGATE) {
         int n_regs = 0;
         SystemVParamClass class = sysvClassifyNode(scope, *ctype, &n_regs);

         if (class == Param_INTEGER && n_regs + m->intParamIdx > 6) {
            class = Param_MEMORY;
         }

         if (class == Param_SSE && n_regs + m->floatParamIdx > 8) {
            class = Param_MEMORY;
         }

         switch (class) {
            case Param_INTEGER: {
               u64 bits = typeBits(ctype);

               // Reserve stack for struct.
               ExprType dst = {
                  .c = *ctype,
                  .location = x64StackPushOffset(m, bits / 8),
               };

               loc = dst.location; // Result

               dst.c.type = Type_LONG;
               while (n_regs--) {
                  // We need a new location

                  RegisterEnum regEnum = m->paramIntegerRegs[m->intParamIdx++];
                  Location src = registerLocation(regEnum);
                  dst.location.offset  = loc.offset - bits/8;

                  ExprType reg = {
                     .c = (Ctype) {
                        .type = Type_LONG,
                     },
                     .location = src,
                  };

                  if (bits <= 8) {
                     reg.c.type = Type_CHAR;
                     dst.c.type = Type_CHAR;
                  }
                  else if (bits <= 16) {
                     reg.c.type = Type_SHORT;
                     dst.c.type = Type_SHORT;
                  }
                  else if (bits <= 32) {
                     reg.c.type = Type_INT;
                     dst.c.type = Type_INT;
                  }

                  u64 nextPow2 = 1 << platformFirstBitSet(bits);
                  if (nextPow2 < bits) { nextPow2 <<= 1; }

                  x64Mov(m, &dst, &reg);

                  bits -= 64;
               }

            } break;
            default: {
               NotImplemented("Parameter class pop");
            }
         }
      }
      else {
         NotImplemented("Non integer types");
      }
   }
   else if (m->config & Config_TARGET_WIN){
      if (typeBits(ctype) <= 64) {
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
            if (isRealType(ctype)) {
               NotImplemented("float params");
            }
         }
         else {
            loc = (Location){ .type = Location_STACK, .offset = m->stack_offset - *offset };
            *offset += typeBits(ctype);
         }
      }
      else {
         loc = (Location){ .type = Location_STACK, .offset = m->stack_offset - *offset };
         *offset += typeBits(ctype);
      }
   }
   return loc;
}

void
x64TestAndJump(MachineX64* m, u32 bits, char* then, char* els) {
   instructionReg(m, "test %s, %s", bits, Reg_RAX, Reg_RAX);
   instructionPrintf("jne %s", then);
   instructionPrintf("jmp %s", els);
}

void
x64Cmp(MachineX64* m, ExprType* dst, ExprType* src) {
   u32 bits = typeBits(&dst->c);
   instructionPrintf("cmp %s, %s",
                     locationString(m, dst->location, bits),
                     locationString(m, src->location, bits));
}

// Compare the accumulator with the top of the stack.
void
x64CmpJmp(Machine* m, AstType ast_type, ExprType* type, char* then, char* els) {
   u32 bits = typeBits(&type->c);
   m->stackPop(m, x64Helper64(type->c.type));

   instructionReg((MachineX64*)m, "cmp %s, %s", bits, Reg_RAX, Reg_RBX);
   switch (ast_type) {
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
x64CmpSetAccum(MachineX64* m, AstType type) {
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
x64AccumInt8() {
   static ExprType accum = {
      .c = { .type = Type_CHAR },
      .location = { .type = Location_REGISTER, .reg = Reg_RAX },
   };

   return &accum;
}

ExprType*
x64AccumInt32() {
   static ExprType accum = {
      .c = { .type = Type_INT },
      .location = { .type = Location_REGISTER, .reg = Reg_RAX },
   };

   return &accum;
}

ExprType*
x64AccumInt64() {
   static ExprType accum = {
      .c = { .type = Type_LONG },
      .location = { .type = Location_REGISTER, .reg = Reg_RAX },
   };

   return &accum;
}

ExprType*
x64AccumInt(u32 bits) {
   ExprType* result = NULL;
   switch (bits) {
      case 64: {
         result = x64AccumInt64();
      } break;
      case 32: {
         result = x64AccumInt32();
      } break;
      case 8: {
         result = x64AccumInt8();
      } break;
      default: {
         NotImplemented("x64AccumInt()");
      } break;
   }
   return result;
}

// NOTE: Every use of the machImmediate* functions invalidates the previous call.

ExprType*
x64ImmediateFromToken(MachineX64* m, Token* tok) {
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
x64ImmediateInt(u64 value) {
   static ExprType imm = {
      .c = { .type = Type_INT },
      .location = { .type = Location_IMMEDIATE },
   };
   imm.location.immediate_value = value;
   return &imm;
}

ExprType*
x64HelperInt8() {
   static ExprType helper = {
      .c = { .type = Type_CHAR },
      .location = { .type = Location_REGISTER, .reg = Reg_RBX },
   };

   return &helper;
}

ExprType*
x64Accum64(int type /*Ctype.type*/ ) {
   ExprType* result = NULL;
   if (type & Type_REAL) {
      static ExprType helper = {
         .c = { .type = Type_DOUBLE },
         .location = { .type = Location_REGISTER, .reg = Reg_XMM0 },
      };

      result = &helper;
   }
   else if (type & Type_PEANO) {
      static ExprType helper = {
         .c = { .type = Type_LONG },
         .location = { .type = Location_REGISTER, .reg = Reg_RAX },
      };
      result = &helper;
   }

   Assert (result);
   return result;
}

ExprType*
x64Accum32(int type /*Ctype.type*/ ) {
   ExprType* result = NULL;
   if (type & Type_REAL) {
      static ExprType helper = {
         .c = { .type = Type_FLOAT },
         .location = { .type = Location_REGISTER, .reg = Reg_XMM0 },
      };

      result = &helper;
   }
   else if (type & Type_PEANO) {
      static ExprType helper = {
         .c = { .type = Type_INT },
         .location = { .type = Location_REGISTER, .reg = Reg_RAX },
      };
      result = &helper;
   }

   Assert (result);
   return result;
}

ExprType*
x64Accum8(int type /*Ctype.type*/ ) {
   ExprType* result = NULL;
   if (type & Type_REAL) {
      static ExprType helper = {
         .c = { .type = Type_FLOAT },
         .location = { .type = Location_REGISTER, .reg = Reg_XMM0 },
      };

      Assert(!"Floating point 8-bit register!");

      result = &helper;
   }
   else if (type & Type_PEANO) {
      static ExprType helper = {
         .c = { .type = Type_CHAR },
         .location = { .type = Location_REGISTER, .reg = Reg_RAX },
      };
      result = &helper;
   }

   Assert (result);
   return result;
}

ExprType*
x64Accum16(int type /*Ctype.type*/ ) {
   ExprType* result = NULL;
   if (type & Type_REAL) {
      static ExprType helper = {
         .c = { .type = Type_FLOAT },
         .location = { .type = Location_REGISTER, .reg = Reg_XMM0 },
      };

      Assert(!"Floating point 16-bit register!");

      result = &helper;
   }
   else if (type & Type_PEANO) {
      static ExprType helper = {
         .c = { .type = Type_SHORT },
         .location = { .type = Location_REGISTER, .reg = Reg_RAX },
      };
      result = &helper;
   }

   Assert (result);
   return result;
}

ExprType*
x64Accum(MachineX64* m, int type /*Ctype.type*/, u32 bits) {
   ExprType* result = NULL;
   switch (bits) {
      case 8: {
         result = x64Accum8(type);
      } break;
      case 32: {
         result = x64Accum32(type);
      } break;
      case 64: {
         result = x64Accum64(type);
      } break;
      default: {
         NotImplemented("Unhandled size for accum register.");
      } break;
   }
   Assert (result);
   return result;
}

ExprType*
x64AccumC(MachineX64* m, Ctype c) {
   u32 bits = typeBits(&c);
   ExprType* result = x64Accum(m, c.type, bits);
   return result;
}

ExprType*
x64Helper32(int type /*Ctype.type*/ ) {
   ExprType* result = NULL;
   if (type & Type_REAL) {
      static ExprType helper = {
         .c = { .type = Type_FLOAT },
         .location = { .type = Location_REGISTER, .reg = Reg_XMM1 },
      };

      result = &helper;
   }
   else if (type & Type_PEANO) {
      static ExprType helper = {
         .c = { .type = Type_INT },
         .location = { .type = Location_REGISTER, .reg = Reg_RBX },
      };
      result = &helper;
   }

   Assert (result);
   return result;
}

ExprType*
x64Helper64(int type /*Ctype.type*/ ) {
   ExprType* result = NULL;
   if (type & Type_REAL) {
      static ExprType helper = {
         .c = { .type = Type_DOUBLE },
         .location = { .type = Location_REGISTER, .reg = Reg_XMM1 },
      };

      result = &helper;
   }
   else if (type & Type_PEANO) {
      static ExprType helper = {
         .c = { .type = Type_LONG },
         .location = { .type = Location_REGISTER, .reg = Reg_RBX },
      };
      result = &helper;
   }

   Assert (result);
   return result;
}

ExprType*
x64Helper(MachineX64* m, int type /*Ctype.type*/, u32 bits) {
   ExprType* result = NULL;
   switch (bits) {
      case 32: {
         result = x64Helper32(type);
      } break;
      case 64: {
         result = x64Helper64(type);
      } break;
      default: {
         NotImplemented("Unhandled size for helper register.");
      } break;
   }
   Assert (result);
   return result;
}

ExprType*
x64HelperC(MachineX64* m, Ctype c) {
   u32 bits = typeBits(&c);
   ExprType* result = x64Helper(m, c.type, bits);
   return result;
}

ExprType*
x64HelperInt32() {
   static ExprType helper = {
      .c = { .type = Type_INT },
      .location = { .type = Location_REGISTER, .reg = Reg_RBX },
   };

   return &helper;
}

ExprType*
x64HelperInt64() {
   static ExprType helper = {
      .c = { .type = Type_LONG },
      .location = { .type = Location_REGISTER, .reg = Reg_RBX },
   };

   return &helper;
}

ExprType*
x64HelperInt_(u32 bits) {
   ExprType* helper = NULL;
   switch (bits) {
      case 8: {
         helper = x64HelperInt8();
      } break;
      case 32: {
         helper = x64HelperInt32();
      } break;
      case 64: {
         helper = x64HelperInt64();
      } break;
      default: {
         NotImplemented("x64HelperInt()");
      }
   }
   return helper;
}

void
x64Mov(MachineX64* m, ExprType* dst, ExprType* src) {
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
            default: {
               codegenError("Invalid floating point variable size");
            }
         }
      }
      else {
         // If both locations are on the stack, use the accumulator as a temp register.
         if (   (dst->location.type == Location_STACK || dst->location.type == Location_STACK_FROM_REG)
             && (src->location.type == Location_STACK || src->location.type == Location_STACK_FROM_REG)) {
            instructionPrintf("mov %s, %s",
                              locationString(m, x64AccumInt(bits)->location, bits),
                              locationString(m, src->location, bits));

            instructionPrintf("mov %s, %s",
                              locationString(m, dst->location, bits),
                              locationString(m, x64AccumInt(bits)->location, bits));

         }
         else if (isRealType(&dst->c)) {
            instructionPrintf("movss %s, %s",
                              locationString(m, dst->location, bits),
                              locationString(m, src->location, bits));
         }
         else {
            instructionPrintf("mov %s, %s",
                              locationString(m, dst->location, bits),
                              locationString(m, src->location, bits));
         }
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
x64Add(MachineX64* m, ExprType* dst, ExprType* src) {
   u32 bits = typeBits(&dst->c);
   if (!isRealType(&dst->c)) {
      instructionPrintf("add %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));
   }
   else {
      instructionPrintf("addps %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));
   }
}

void
x64Sub(MachineX64* m, ExprType* dst, ExprType* src) {
   u32 bits = typeBits(&dst->c);
   if (!isRealType(&dst->c)) {
      instructionPrintf("sub %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));
   }
   else {
      instructionPrintf("subps %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));
   }
}

void
x64Mul(MachineX64* m, ExprType* dst, ExprType* src) {
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
x64Div(MachineX64* m, ExprType* dst, ExprType* src) {
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
x64MovAccum(MachineX64* m, ExprType* et , Token* rhs_tok)  {
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
x64StackAddressInAccum(MachineX64* m, ExprType* entry) {
   Assert(entry->location.type == Location_STACK);
   instructionPrintf("lea rax, [rsp + %d]", m->stack_offset - entry->location.offset);
}

void
x64Call(MachineX64* m, char* label) {
   instructionPrintf("call %s", label);
}

Location
x64StackPushReg(MachineX64* m, RegisterEnum reg) {
   instructionPrintf("push %s", locationString(m, registerLocation(reg), 64));
   Location location = (Location) {
      .type = Location_STACK,
      .offset = m->stack_offset,
   };

   m->stack_offset += 8;
   bufPush(m->s_stack, (StackValue){ .type = Stack_QWORD });
   return location;
}

Location
x64StackPushImm(MachineX64* m, ExprType* et, i64 val) {
   instructionPrintf("push %d", val);
   m->stack_offset += 8;
   bufPush(m->s_stack, (StackValue){ .type = Stack_QWORD });
   et->location = (Location) {
      .type = Location_STACK,
      .reg = m->stack_offset,
   };
   return et->location;
}

Location
x64StackPushOffset(MachineX64* m, u64 bytes) {
   Location location = (Location) {
      .type = Location_STACK,
      .offset = m->stack_offset,
   };

   Assert(bytes);
   instructionPrintf("sub rsp, %d", bytes);
   m->stack_offset += bytes;
   StackValue val = { .type = Stack_OFFSET, .offset = bytes };
   bufPush(m->s_stack, val);
   return location;
}

void
x64Finish(MachineX64* m) {
   char* end =
      //"int 3\n"
      "ret\n";
   fwrite(end, 1, strlen(end), g_asm);
   fclose(g_asm);
}


void
x64FunctionPrelude(MachineX64* m, char* func_name) {
   instructionPrintf("global %s", func_name);
   instructionPrintf("%s:", func_name);
   instructionPrintf("push rbp");
   instructionPrintf("mov rbp, rsp");

   m->stack_offset += 16;  // 8 for rbp. 8 for return address from call
}

void
x64FunctionEpilogue(MachineX64* m) {
   instructionPrintf(".func_end:");

   while (bufCount(m->s_stack) > 0)  {
      m->machine.stackPop(m, x64Helper64(Type_INT));
   }

   // Restore non-volatile registers.

   instructionPrintf("pop rbp");
   m->stack_offset -= 8;

   instructionPrintf("ret");
   m->stack_offset -= 8;
}

void
x64Label(MachineX64* m, char* label) {
   instructionPrintf("%s:", label);
}

void
x64Jmp(MachineX64* m, char* label) {
   instructionPrintf("jmp %s", label);
}

void
x64AddressOf(MachineX64* m, Location* loc) {
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


void
x64ConvertFloatToInt(MachineX64* machine, Location from) {
   if (from.type == Location_REGISTER) {
      char* from_str = g_registers[from.reg].reg;
      instructionPrintf("cvtss2si rax, %s", from_str);
   }
   else {
      NotImplemented("X64 conversion instruction")
   }
}


Machine*
makeMachineX64(Arena* a, MachineConfigFlags mflags) {
   MachineX64* m64 = AllocType(a, MachineX64);

   m64->config = mflags;

   char* prelude =
   // TODO: unix / win32 targets
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

   setupVolatility(m64);

   Machine* m = &m64->machine;

   #if defined(__MACH__)
      #pragma clang diagnostic push
      #pragma clang diagnostic ignored "-Wincompatible-pointer-types"
   #endif
   {
      // Function pointers
      m->stackPop = x64StackPop;

      m->immediateFromToken = x64ImmediateFromToken;

      m->stackPop = x64StackPop;
      m->stackPushReg = x64StackPushReg;
      m->stackPushImm = x64StackPushImm;
      m->stackPushOffset = x64StackPushOffset;
      m->stackAddressInAccum = x64StackAddressInAccum;
      m->addressOf = x64AddressOf;
      m->functionPrelude = x64FunctionPrelude;
      m->beginFuncParams = x64BeginFuncParams;
      m->pushParameter = x64PushParameter;
      m->popParameter = x64PopParameter;
      m->endFuncParams = x64EndFuncParams;;
      m->functionEpilogue = x64FunctionEpilogue;
      m->mov = x64Mov;
      m->movAccum = x64MovAccum;
      m->add = x64Add;
      m->sub = x64Sub;
      m->mul = x64Mul;
      m->div = x64Div;
      m->cmp = x64Cmp;
      m->cmpSetAccum = x64CmpSetAccum;
      m->cmpJmp = x64CmpJmp;
      m->testAndJump = x64TestAndJump;
      m->jmp = x64Jmp;
      m->label = x64Label;
      m->call = x64Call;

      m->helper = x64Helper;
      m->helperC = x64HelperC;

      m->accum = x64Accum;
      m->accumC = x64AccumC;

      m->finish = x64Finish;

      m->convertFloatToInt = x64ConvertFloatToInt;
   }
   #if defined(__MACH__)
      #pragma clang diagnostic pop
   #endif

   m64->intParamIdx = 0;

   m64->paramIntegerRegs[0] = Reg_RDI;
   m64->paramIntegerRegs[1] = Reg_RSI;
   m64->paramIntegerRegs[2] = Reg_RDX;
   m64->paramIntegerRegs[3] = Reg_RCX;
   m64->paramIntegerRegs[4] = Reg_R8;
   m64->paramIntegerRegs[5] = Reg_R9;

   m64->floatParamIdx = 0;

   m64->paramFloatRegs[0] = Reg_XMM0;
   m64->paramFloatRegs[1] = Reg_XMM1;
   m64->paramFloatRegs[2] = Reg_XMM2;
   m64->paramFloatRegs[3] = Reg_XMM3;
   m64->paramFloatRegs[4] = Reg_XMM4;
   m64->paramFloatRegs[5] = Reg_XMM5;
   m64->paramFloatRegs[6] = Reg_XMM6;
   m64->paramFloatRegs[7] = Reg_XMM7;

   return m;
}
