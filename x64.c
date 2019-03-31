static Machine* g_mach;
static FILE* g_asm;

struct MachineX64 {
   Machine machine;  // Space for function pointers.

   StackValue* s_stack;  // Stack allocation / de-allocation is done on a per-function basis.
   i64         stack_offset;  // # Bytes from the bottom of the stack to RSP.

   // Parameter passing state
   union {

      // Unix
      struct {
         u32 intIdx;
         RegisterEnum integerRegs[6];

         u32 floatParamIdx;
         RegisterEnum floatRegs[8];
      };

      // Windows
      struct {
         // Begins as offset to stack before function prelude
         u64 offset;
         int n_param;
      };
   } params;
} typedef MachineX64;

void        x64Mov(MachineX64* m, RegVar* dst, RegVar* src);
Location    x64StackPushOffset(MachineX64* m, u64 bytes);

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
   if (   (m->machine.flags & Config_TARGET_MACOS)
       || (m->machine.flags & Config_TARGET_LINUX)) {
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
   else if (m->machine.flags & Config_TARGET_WIN) {
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
      } break;
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
      case Location_REG_POINTER: {
#define ResultSize 64
         char tmp_string[ResultSize] = {0};
         char* reg_str = g_registers[r.reg].reg;
         char* sign = r.reg_offset >= 0 ? "+" : "-";
         u32 offset = abs(r.reg_offset);
         if (bits == 8)
            snprintf(tmp_string, ArrayCount(tmp_string), "BYTE [ %s %s 0x%x ]", reg_str, sign, offset);
         else if (bits == 16)
            snprintf(tmp_string, ArrayCount(tmp_string), "WORD [ %s %s 0x%x ]", reg_str, sign, offset);
         else if (bits == 32)
            snprintf(tmp_string, ArrayCount(tmp_string), "DWORD [ %s %s 0x%x ]", reg_str, sign, offset);
         else if (bits == 64)
            snprintf(tmp_string, ArrayCount(tmp_string), "QWORD [ %s %s 0x%x ]", reg_str, sign, offset);
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
instructionPrintf(MachineX64* m, char* asm_line, ...) {
   va_list args;
   va_start(args, asm_line);

   if (!(m->machine.flags & Config_INSTR_OUTPUT_DISABLED)) {
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
         instructionPrintf(m, asm_line);
      } break;
      case 1: {
         instructionPrintf(m, asm_line,
                           locationString(m, registerLocation(regs[0]), bits));
      } break;
      case 2: {
         instructionPrintf(m, asm_line,
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
//RegVar* findTag(Scope* c, char* name);
//RegVar* findSymbol(Codegen* c, char* name);

void
x64StackPop(MachineX64* m, RegVar* et) {
   Assert(et->location.type == Location_REGISTER);
   StackValue s = bufPop(m->s_stack);
   switch (s.type) {
      case Stack_QWORD: {
         RegisterEnum reg = et->location.reg;
         if (reg < Reg_XMM0) {
            instructionReg(m, "pop %s", 64, reg);
         }
         else {
            instructionReg(m, "movsd %s, QWORD[rsp]", 64, reg);
            instructionPrintf(m, "add rsp, 8");
         }
         m->stack_offset -= 8;
      } break;
      case Stack_OFFSET: {
         m->stack_offset -= s.offset;
         instructionPrintf(m, "add rsp, %d", s.offset);
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

RegisterEnum
sysVFloatRegisterEnum(u64 n_param) {
   RegisterEnum r = Reg_XMM0 + n_param;
   return r;
}

void
x64BeginFuncParams(MachineX64* m) {
   if (m->machine.flags & Config_TARGET_WIN) {
      m->params.offset = 16;  // Function prelude stack offset. RBP 8 bytes. Return address 8 bytes.
      m->params.n_param = 0;
   }
   else {
      m->params.intIdx = 0;
      m->params.floatParamIdx = 0;
   }

}

void
x64EndFuncParams(MachineX64* m) {

}

void
x64PushParameter(MachineX64* m, Scope* scope, RegVar* rvar) {
   if ((m->machine.flags & Config_TARGET_LINUX) || (m->machine.flags & Config_TARGET_MACOS)) {
      Location loc = {0};
      if (isIntegerType(&rvar->c) || isDerivedType(&rvar->c)) {
         if (m->params.intIdx < ArrayCount(m->params.integerRegs)) {
            loc = registerLocation(sysVIntegerRegisterEnum(m->params.intIdx++));
            RegVar reg = {
               .c = rvar->c,
               .location = loc,
            };
            x64Mov(m, &reg, rvar);
         }
         else {
            NotImplemented("Pass integer param on stack");
         }
      }
      else if (isRealType(&rvar->c)) {
         if (m->params.floatParamIdx < ArrayCount(m->params.floatRegs)) {
            loc = registerLocation(sysVFloatRegisterEnum(m->params.floatParamIdx++));
            RegVar reg = {
               .c = rvar->c,
               .location = loc,
            };
            x64Mov(m, &reg, rvar);
         }
         else {
            NotImplemented("Pass float param on stack");
         }
      }
      else if (rvar->c.type == Type_AGGREGATE) {
         int n_regs = 0;
         SystemVParamClass class = sysvClassifyNode(scope, rvar->c, &n_regs);

         if (class == Param_INTEGER && n_regs + m->params.intIdx > 6) {
            class = Param_MEMORY;
         }

         if (class == Param_SSE && n_regs + m->params.floatParamIdx > 8) {
            class = Param_MEMORY;
         }

         switch (class) {
            case Param_INTEGER: {

               u64 bits = typeBits(&rvar->c);

               RegVar partial_argument = *rvar;

               while (n_regs--) {
                  Location loc = registerLocation(m->params.integerRegs[m->params.intIdx++]);

                  RegVar reg = {
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


                  if (rvar->location.type == Location_STACK) {
                     partial_argument.location.offset -= 8;
                  }
                  else if (rvar->location.type == Location_REG_POINTER) {
                     partial_argument.location.reg_offset += 8;
                  }
                  else if (rvar->location.type == Location_REGISTER) {
                     Assert (bits <= 64);
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
         NotImplemented("Non integer types (push)");
      }
   }
   else {
      // Windows x64 calling convention:
      // First 4 integers (left-to-right): RCX, RDX, R8, and R9
      // First 4 floats (left-to-right): XMM0-3
      // Items 5 an higher on the stack.
      // Items larger than 16 bytes passed by reference.
      Location loc = {0};
      if (typeBits(&rvar->c) <= 64) {
         loc.type = Location_REGISTER;
         if (m->params.n_param < 4) {
            if (!isRealType(&rvar->c)) {
               switch(m->params.n_param) {
                  case 0: { loc.reg = Reg_RCX; } break;
                  case 1: { loc.reg = Reg_RDX; } break;
                  case 2: { loc.reg = Reg_R8;  } break;
                  case 3: { loc.reg = Reg_R9;  } break;
               }
            }
            else  {
               switch(m->params.n_param) {
                  case 0: { loc.reg = Reg_XMM0; } break;
                  case 1: { loc.reg = Reg_XMM1; } break;
                  case 2: { loc.reg = Reg_XMM2; } break;
                  case 3: { loc.reg = Reg_XMM3; } break;
               }
            }
         }
         else {
            NotImplemented("more than 4 params");
         }
      }
      else {
         x64StackPushOffset(m, typeBits(&rvar->c) / 8);
         loc.type = Location_STACK;
         loc.offset = m->stack_offset;
      }
      RegVar reg = {
         .c = rvar->c,
          .location = loc,
      };
      x64Mov(m, &reg, rvar);
      m->params.n_param++;
   }
}

RegVar* x64Helper(MachineX64* m, int type /*Ctype.type*/, u32 bits); // fwd decl


Location
x64PopParameter(MachineX64* m, Scope* scope, Ctype* ctype) {
   Location loc = Zero;

   if ((m->machine.flags & Config_TARGET_MACOS) || (m->machine.flags & Config_TARGET_LINUX)) {
      if (isIntegerType(ctype) || isDerivedType(ctype)) {
         if (m->params.intIdx < ArrayCount(m->params.integerRegs)) {
            loc = registerLocation(sysVIntegerRegisterEnum(m->params.intIdx++));
         }
         else {
            NotImplemented("Pass integer param on stack");
         }
      }
      else if (isRealType(ctype)) {
         if (m->params.floatParamIdx < ArrayCount(m->params.floatRegs)) {
            loc = registerLocation(sysVFloatRegisterEnum(m->params.floatParamIdx++));
         }
         else {
            NotImplemented("Pass real param on stack");
         }
      }
      else if (ctype->type == Type_AGGREGATE) {
         int n_regs = 0;
         SystemVParamClass class = sysvClassifyNode(scope, *ctype, &n_regs);

         if (class == Param_INTEGER && n_regs + m->params.intIdx > 6) {
            class = Param_MEMORY;
         }

         if (class == Param_SSE && n_regs + m->params.floatParamIdx > 8) {
            class = Param_MEMORY;
         }

         switch (class) {
            case Param_INTEGER: {
               u64 bits = typeBits(ctype);

               // Reserve stack for struct.
               RegVar dst = {
                  .c = *ctype,
                  .location = x64StackPushOffset(m, bits / 8),
               };
               Assert((bits % 8) == 0);

               loc = dst.location; // Result

               dst.c.type = Type_LONG;
               while (n_regs--) {
                  // We need a new location

                  RegisterEnum regEnum = m->params.integerRegs[m->params.intIdx++];
                  Location src = registerLocation(regEnum);

                  RegVar reg = {
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

                  u64 nextPow2 = (u64)1 << platformFirstBitSet(bits);
                  if (nextPow2 < bits) { nextPow2 <<= 1; }

                  x64Mov(m, &dst, &reg);

                  dst.location.offset  = loc.offset - 8;
                  bits -= 64;
               }

            } break;
            default: {
               NotImplemented("Parameter class pop");
            }
         }
      }
      else {
         NotImplemented("Non integer types (pop)");
      }
   }
   else if (m->machine.flags & Config_TARGET_WIN) {
      if (typeBits(ctype) <= 64) {
         if (m->params.n_param < 4) {
            if (isIntegerType(ctype) || ctype->type == Type_POINTER || isAggregateType(ctype)) {
               loc.type = Location_REGISTER;
               switch(m->params.n_param) {
                  case 0: { loc.reg = Reg_RCX; } break;
                  case 1: { loc.reg = Reg_RDX; } break;
                  case 2: { loc.reg = Reg_R8;  } break;
                  case 3: { loc.reg = Reg_R9;  } break;
               }
            }
            if (isRealType(ctype)) {
               loc.type = Location_REGISTER;
               switch (m->params.n_param) {
                  case 0: { loc.reg = Reg_XMM0; } break;
                  case 1: { loc.reg = Reg_XMM1; } break;
                  case 2: { loc.reg = Reg_XMM2; } break;
                  case 3: { loc.reg = Reg_XMM3; } break;
               }
            }
         }
         else {
            loc = (Location){ .type = Location_STACK, .offset = m->stack_offset - m->params.offset };
            m->params.offset += (typeBits(ctype) + 7) / 8;
         }
         m->params.n_param++;
      }
      else {
         loc = (Location){ .type = Location_STACK, .offset = m->stack_offset - m->params.offset };
         m->params.offset += (typeBits(ctype) + 7) / 8;
      }
   }
   return loc;
}

void
x64TestAndJump(MachineX64* m, RegisterEnum reg, u32 bits, char* then, char* els) {
   instructionReg(m, "test %s, %s", bits, reg, reg);
   instructionPrintf(m, "jne %s", then);
   instructionPrintf(m, "jmp %s", els);
}

void
x64Cmp(MachineX64* m, RegVar* dst, RegVar* src) {
   if (isIntegerType(&src->c)) {
      u32 bits = typeBits(&dst->c);
      instructionPrintf(m, "cmp %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));
   }
   else if (src->c.type == Type_FLOAT) {
      u32 bits = typeBits(&dst->c);
      instructionPrintf(m, "ucomiss %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));

   }
   else if (src->c.type == Type_DOUBLE) {
      u32 bits = typeBits(&dst->c);
      instructionPrintf(m, "ucomisd %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));

   }
   else {
      NotImplemented("cmp type");
   }
}

void
x64CmpJmp(MachineX64* m, AstType ast_type, char* label) {
   switch (ast_type) {
      case Ast_EQUALS:     { instructionPrintf(m, "je %s", label); } break;
      case Ast_LESS:       { instructionPrintf(m, "jl %s", label); } break;
      case Ast_LEQ:        { instructionPrintf(m, "jle %s", label); } break;
      case Ast_GREATER:    { instructionPrintf(m, "jg %s", label); } break;
      case Ast_GEQ:        { instructionPrintf(m, "jge %s", label); } break;
      case Ast_NOT_EQUALS: { instructionPrintf(m, "jne %s", label); } break;
      default:             { InvalidCodePath; }
   }
}

// Compare the accumulator with the top of the stack.
void
x64CmpJmpStackTop(MachineX64* m, AstType ast_type, Location loc_to_compare, RegVar* type, char* then, char* els) {
   u32 bits = typeBits(&type->c);
   x64StackPop(m, x64Helper(m, type->c.type, bits));

   instructionReg((MachineX64*)m, "cmp %s, %s", bits, loc_to_compare.reg, Reg_RBX);

   x64CmpJmp(m, ast_type, then);

   instructionPrintf(m, "jmp %s", els);
}

void
x64CmpSet(MachineX64* m, AstType type, Location dst) {
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
   Assert(dst.type == Location_REGISTER);
   instructionReg(m, instr, 8 /* SETCC operates on byte registers*/, dst.reg);
   instructionReg(m, "and %s, 1", 64, dst.reg);
}

RegVar*
x64AccumInt8() {
   static RegVar accum = {
      .c = { .type = Type_CHAR },
      .location = { .type = Location_REGISTER, .reg = Reg_RAX },
   };

   return &accum;
}

RegVar*
x64AccumInt32() {
   static RegVar accum = {
      .c = { .type = Type_INT },
      .location = { .type = Location_REGISTER, .reg = Reg_RAX },
   };

   return &accum;
}

RegVar*
x64AccumInt64() {
   static RegVar accum = {
      .c = { .type = Type_LONG },
      .location = { .type = Location_REGISTER, .reg = Reg_RAX },
   };

   return &accum;
}

RegVar*
x64AccumInt(u32 bits) {
   RegVar* result = NULL;
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

RegVar*
x64ImmediateFromToken(MachineX64* m, Token* tok) {
   static RegVar imm = {
      .c = { .type = Type_INT },
      .location = { .type = Location_IMMEDIATE },
   };

   if (tok->type == TType_FLOAT) {  // Float lhs, integer literal rhs
      imm.c.type = Type_FLOAT;
      imm.location.cast.real64 = tok->cast.real32;
   }
   else if (tok->type == TType_DOUBLE) {
      imm.c.type = Type_DOUBLE;
      imm.location.cast.real64 = tok->cast.real64;
   }
   else if (tok->type == TType_NUMBER)  {
      imm.c.type = Type_INT;
      imm.location.immediate_value= tok->value;
   }
   return &imm;
}


RegVar*
x64ImmediateInt(u64 value) {
   static RegVar imm = {
      .c = { .type = Type_INT },
      .location = { .type = Location_IMMEDIATE },
   };
   imm.location.immediate_value = value;
   return &imm;
}

RegVar*
x64Accum(MachineX64* m, int type /*Ctype.type*/, u32 bits) {
   RegVar* result = NULL;
   switch (bits) {
      case 8:
      case 16:
      case 32:
      case 64: {
         if (type & Type_REAL) {
            static RegVar helper = {
               .c = { .type = Type_NONE },
               .location = { .type = Location_REGISTER, .reg = Reg_XMM0 },
            };

            helper.c.type = type;

            result = &helper;
         }
         else if ((type & Type_PEANO) || type == Type_POINTER || type == Type_AGGREGATE) {
            static RegVar helper = {
               .c = { .type = Type_NONE },
               .location = { .type = Location_REGISTER, .reg = Reg_RAX },
            };
            helper.c.type = type;
            if (type == Type_AGGREGATE) {
               helper.c.aggr.bits = bits;
            }
            result = &helper;
         }
      } break;
      default: {
         NotImplemented("Unhandled size for accum register.");
      } break;
   }

   Assert (result);
   return result;
}

RegVar*
x64AccumC(MachineX64* m, Ctype c) {
   u32 bits = typeBits(&c);
   RegVar* result = x64Accum(m, c.type, bits);
   return result;
}


// TODO: Helper and Accum should both call a common func
RegVar*
x64Helper(MachineX64* m, int type /*Ctype.type*/, u32 bits) {
   RegVar* result = NULL;
   switch (bits) {
      case 8:
      case 16:
      case 32:
      case 64: {
         if (type & Type_REAL) {
            static RegVar helper = {
               .c = { .type = Type_NONE },
               .location = { .type = Location_REGISTER, .reg = Reg_XMM1 },
            };

            helper.c.type = type;
            result = &helper;
         }
         else if ((type & Type_PEANO) || type == Type_POINTER || type == Type_AGGREGATE) {
            static RegVar helper = {
               .c = { .type = Type_NONE },
               .location = { .type = Location_REGISTER, .reg = Reg_RBX },
            };
            helper.c.type = type;
            if (type == Type_AGGREGATE) {
               helper.c.aggr.bits = bits;
            }

            result = &helper;
         }

         Assert (result);
      } break;
      default: {
         NotImplemented("Unhandled size for helper register.");
      } break;
   }
   return result;
}

RegVar*
x64HelperC(MachineX64* m, Ctype c) {
   u32 bits = typeBits(&c);
   RegVar* result = x64Helper(m, c.type, bits);
   return result;
}

RegVar*
x64HelperInt32() {
   static RegVar helper = {
      .c = { .type = Type_INT },
      .location = { .type = Location_REGISTER, .reg = Reg_RBX },
   };

   return &helper;
}

RegVar*
x64HelperInt64() {
   static RegVar helper = {
      .c = { .type = Type_LONG },
      .location = { .type = Location_REGISTER, .reg = Reg_RBX },
   };

   return &helper;
}

void
x64Mov(MachineX64* m, RegVar* dst, RegVar* src) {
   u32 bits = typeBits(&dst->c);
   Assert(bits);

   if (bits <= 64) {
      if (dst->location.type == Location_REGISTER
          && bits <= 16) {
         Assert (!isRealType(&dst->c));
         instructionPrintf(m, "xor %s, %s",
                           locationString(m, dst->location, 64),
                           locationString(m, dst->location, 64));
      }
      if (isRealType(&dst->c) &&
          isImmediate(src)) {
         double imm = 0;

         if (isRealType(&src->c)) {
            imm = src->location.cast.real64;
         }
         else if (isIntegerType(&src->c)) {
            imm = (double)src->location.cast.int64;
         }
         else {
            InvalidCodePath;
         }

         switch(typeBits(&dst->c)) {
            case 64: {
               instructionPrintf(m, "mov rbx, __float64__(%f)",
                                 imm);
               instructionPrintf(m, "mov %s, rbx",
                                 locationString(m, dst->location, bits));
            } break;
            case 32: {
               instructionPrintf(m, "mov %s, __float32__(%f)",
                                 locationString(m, dst->location, bits),
                                 imm);
            } break;
            default: {
               codegenError("Invalid floating point variable size");
            }
         }
      }
      else {
         // If both locations are on the stack, use the accumulator as a temp register.
         if (   (dst->location.type == Location_STACK || dst->location.type == Location_REG_POINTER)
             && (src->location.type == Location_STACK || src->location.type == Location_REG_POINTER)) {
            instructionPrintf(m, "mov %s, %s",
                              locationString(m, x64AccumInt(bits)->location, bits),
                              locationString(m, src->location, bits));

            instructionPrintf(m, "mov %s, %s",
                              locationString(m, dst->location, bits),
                              locationString(m, x64AccumInt(bits)->location, bits));

         }
         else if (isRealType(&dst->c)) {
            if (dst->c.type == Type_FLOAT) {
               instructionPrintf(m, "movss %s, %s",
                                 locationString(m, dst->location, bits),
                                 locationString(m, src->location, bits));

            }
            else if (dst->c.type == Type_DOUBLE) {
               instructionPrintf(m, "movsd %s, %s",
                                 locationString(m, dst->location, bits),
                                 locationString(m, src->location, bits));
            }
            else {
               InvalidCodePath;
            }
         }
         else {
            instructionPrintf(m, "mov %s, %s",
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
            instructionPrintf(m, "lea rsi, [ rsp + %d ]", m->stack_offset - src->location.offset);
         }
         else if (src->location.type == Location_REG_POINTER) {
            instructionPrintf(m, "lea rsi, [ %s + %d ]", g_registers[src->location.reg].reg, src->location.reg_offset);
         }
         else {
            NotImplemented("Location for big mov");
         }
         instructionPrintf(m, "mov rdi, rsp");
         if (dst->location.offset != m->stack_offset) {
            instructionPrintf(m, "add rdi, %d", m->stack_offset - dst->location.offset);
         }
         instructionPrintf(m, "mov rcx, 0x%x", bytes);
         instructionPrintf(m, "rep movsb");
      }
      else {
         InvalidCodePath;
      }
   }
}

void
x64Add(MachineX64* m, RegVar* dst, RegVar* src) {
   u32 bits = typeBits(&dst->c);
   if (!isRealType(&dst->c)) {
      instructionPrintf(m, "add %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));
   }
   else if (dst->c.type == Type_FLOAT) {
      instructionPrintf(m, "addps %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));
   }
   else if (dst->c.type == Type_DOUBLE) {
      instructionPrintf(m, "addpd %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));

   }
   else {
      InvalidCodePath;
   }
}

void
x64Sub(MachineX64* m, RegVar* dst, RegVar* src) {
   u32 bits = typeBits(&dst->c);
   if (!isRealType(&dst->c)) {
      instructionPrintf(m, "sub %s, %s",
                        locationString(m, dst->location, bits),
                        locationString(m, src->location, bits));
   }
   else if (dst->c.type == Type_FLOAT) {
      instructionPrintf(m, "subps %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));
   }
   else if (dst->c.type == Type_DOUBLE) {
      instructionPrintf(m, "subpd %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));
   }
   else {
      InvalidCodePath;
   }
}

void
x64Mul(MachineX64* m, RegVar* dst, RegVar* src) {
   u32 bits = typeBits(&dst->c);
   if (!isRealType(&dst->c)) {
      instructionPrintf(m, "imul %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));

   }
   else if (dst->c.type == Type_FLOAT) {
      instructionPrintf(m, "mulps %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));
   }
   else if (dst->c.type == Type_DOUBLE) {
      instructionPrintf(m, "mulpd %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));
   }
   else {
      InvalidCodePath;
   }
}

void
x64Div(MachineX64* m, RegVar* dst, RegVar* src) {
   u32 bits = typeBits(&dst->c);
   if (!isRealType(&dst->c)) {
      instructionPrintf(m, "idiv %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));

   }
   else if (dst->c.type == Type_FLOAT) {
      instructionPrintf(m, "divps %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));
   }
   else if (dst->c.type == Type_DOUBLE) {
      instructionPrintf(m, "divpd %s, %s",
         locationString(m, dst->location, bits),
         locationString(m, src->location, bits));
   }
   else {
      InvalidCodePath;
   }
}

void
x64Mod(MachineX64* m, RegVar* dst, RegVar* src) {
   NotImplemented("x64Mod");
}

void
x64BitAnd(MachineX64* m, RegVar* dst, RegVar* src) {
   NotImplemented("x64BitAnd");
}

void
x64BitOr(MachineX64* m, RegVar* dst, RegVar* src) {
   NotImplemented("x64BitOr");
}

void
x64BitXor(MachineX64* m, RegVar* dst, RegVar* src) {
   NotImplemented("x64BitXor");
}

void
x64ShiftLeft(MachineX64* m, RegVar* dst, RegVar* src) {
   NotImplemented("x64ShiftLeft");
}

void
x64ShiftRight(MachineX64* m, RegVar* dst, RegVar* src) {
   NotImplemented("x64ShiftRight");
}

void
x64StackAddress(MachineX64* m, RegVar* entry, Location target_loc) {
   Assert(entry->location.type == Location_STACK);
   instructionPrintf(m, "lea %s, [rsp + %d]", locationString(m, target_loc, 64), m->stack_offset - entry->location.offset);
}

void
x64Call(MachineX64* m, char* label) {
   instructionPrintf(m, "call %s", label);
}

Location
x64StackPushReg(MachineX64* m, RegisterEnum reg) {
   // Integer
   if (reg < Reg_XMM0) {
      instructionPrintf(m, "push %s", locationString(m, registerLocation(reg), 64));
   }
   // double
   else {
      instructionPrintf(m, "sub rsp, 8");
      instructionPrintf(m, "movsd QWORD[rsp], %s", locationString(m, registerLocation(reg), 64));
   }
   m->stack_offset += 8;

   Location location = (Location) {
      .type = Location_STACK,
      .offset = m->stack_offset,
   };

   bufPush(m->s_stack, (StackValue){ .type = Stack_QWORD });
   return location;
}

Location
x64StackPushImm(MachineX64* m, i64 val) {
   instructionPrintf(m, "push %d", val);
   m->stack_offset += 8;
   bufPush(m->s_stack, (StackValue){ .type = Stack_QWORD });
   Location location /*location!*/= {
      .type = Location_STACK,
      .reg = m->stack_offset,
   };
   return location;
}

Location
x64StackPushOffset(MachineX64* m, u64 bytes) {
   m->stack_offset += bytes;
   Location location = (Location) {
      .type = Location_STACK,
      .offset = m->stack_offset,
   };

   Assert(bytes);
   instructionPrintf(m, "sub rsp, %d", bytes);
   StackValue val = { .type = Stack_OFFSET, .offset = bytes };
   bufPush(m->s_stack, val);
   return location;
}


Location
x64StackPush(MachineX64* m, Location location) {
   Location loc = Zero;
   switch (location.type) {
      case Location_REGISTER: {
         loc = x64StackPushReg(m, location.reg);
      } break;
      case Location_IMMEDIATE: {
         loc = x64StackPushImm(m, location.immediate_value);
      } break;
      default: {
         NotImplemented("Generic stack push from non-register non-immediate");
      } break;
   }
   return loc;
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
   instructionPrintf(m, "global %s", func_name);
   instructionPrintf(m, "%s:", func_name);
   instructionPrintf(m, "push rbp");
   instructionPrintf(m, "mov rbp, rsp");

   m->stack_offset += 16;  // 8 for rbp. 8 for return address from call
}

void
x64FunctionEpilogue(MachineX64* m) {
   instructionPrintf(m, ".func_end:");

   while (bufCount(m->s_stack) > 0)  {
      m->machine.stackPop(m, x64Helper(m, Type_LONG, 64));
   }

   // Restore non-volatile registers.

   instructionPrintf(m, "pop rbp");
   m->stack_offset -= 8;

   instructionPrintf(m, "ret");
   m->stack_offset -= 8;
}

void
x64Label(MachineX64* m, char* label) {
   instructionPrintf(m, "%s:", label);
}

void
x64Jmp(MachineX64* m, char* label) {
   instructionPrintf(m, "jmp %s", label);
}

void
x64AddressOf(MachineX64* m, Location* loc, Location target_loc) {
   switch (loc->type) {
      case Location_STACK: {
         char* loc_str = locationString(m, target_loc, 64);
         instructionPrintf(m, "mov %s, rsp", loc_str);
         instructionPrintf(m, "add %s, %d", loc_str, m->stack_offset - loc->offset);
      } break;
      default: {
         NotImplemented("Address of something not on the stack");
      }
   }
}

void
x64ConvertIntegerToFloat(MachineX64* m, Location from, EmitTarget target) {
   if (target == Target_ACCUM) {
      instructionPrintf(m, "cvtsi2ss xmm0, %s", locationString(m, from, 32));
   }
   else if (target == Target_STACK) {
      instructionPrintf(m, "cvtsi2ss xmm1, %s", locationString(m, from, 32));
      x64StackPushReg(m, Reg_XMM1);
   }
}

void
x64ConvertFloatToInt(MachineX64* m, Location from, EmitTarget target) {
   char* from_str = locationString(m, from, 32);
   if (target == Target_ACCUM) {
      instructionPrintf(m, "cvtss2si rax, %s", from_str);
   }
   else if (target == Target_STACK) {
      instructionPrintf(m, "cvtss2si rbx, %s", from_str);
      x64StackPushReg(m, Reg_RBX);
   }
}

void
x64ConvertFloatToDouble(MachineX64* m, Location from, EmitTarget target) {
   char* from_str = locationString(m, from, 32);
   if (target == Target_ACCUM) {
      instructionPrintf(m, "cvtss2sd xmm0, %s", from_str);
   }
   else if (target == Target_STACK) {
      instructionPrintf(m, "cvtss2sd xmm1, %s", from_str);
      x64StackPushReg(m, Reg_XMM1);
   }
}

void
x64ConvertDoubleToFloat(MachineX64* m, Location from, EmitTarget target) {
   char* from_str = locationString(m, from, 64);
   if (target == Target_ACCUM) {
      instructionPrintf(m, "cvtsd2ss xmm0, %s", from_str);
   }
   else if (target == Target_STACK) {
      instructionPrintf(m, "cvtsd2ss xmm1, %s", from_str);
      x64StackPushReg(m, Reg_XMM1);
   }
}

void
x64ConvertIntegerToDouble(MachineX64* m, Location from, EmitTarget target) {
   if (target == Target_ACCUM) {
      instructionPrintf(m, "cvtsi2sd xmm0, %s", locationString(m, from, 32));
   }
   else if (target == Target_STACK) {
      instructionPrintf(m, "cvtsi2sd xmm1, %s", locationString(m, from, 32));
      x64StackPushReg(m, Reg_XMM1);
   }
}

void
x64ConvertDoubleToInt(MachineX64* m, Location from, EmitTarget target) {
   char* from_str = locationString(m, from, 64);
   if (target == Target_ACCUM) {
      instructionPrintf(m, "cvtsd2si rax, %s", from_str);
   }
   else if (target == Target_STACK) {
      instructionPrintf(m, "cvtsd2si rbx, %s", from_str);
      x64StackPushReg(m, Reg_RBX);
   }
}


Machine*
makeMachineX64(Arena* a, MachineConfigFlags mflags) {
   MachineX64* m64 = AllocType(a, MachineX64);

   m64->machine.flags = mflags;

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
      m->flags = mflags;

      // Function pointers
      m->immediateFromToken = x64ImmediateFromToken;

      m->stackPop = x64StackPop;
      m->stackPush = x64StackPush;
      m->stackPushReg = x64StackPushReg;
      m->stackPushImm = x64StackPushImm;
      m->stackPushOffset = x64StackPushOffset;
      m->stackAddress = x64StackAddress;
      m->addressOf = x64AddressOf;
      m->functionPrelude = x64FunctionPrelude;
      m->beginFuncParams = x64BeginFuncParams;
      m->pushParameter = x64PushParameter;
      m->popParameter = x64PopParameter;
      m->endFuncParams = x64EndFuncParams;;
      m->functionEpilogue = x64FunctionEpilogue;
      m->mov = x64Mov;
      m->add = x64Add;
      m->sub = x64Sub;
      m->mul = x64Mul;
      m->div = x64Div;

      m->mod = x64Mod;
      m->bitAnd = x64BitAnd;
      m->bitOr = x64BitOr;
      m->bitXor = x64BitXor;
      m->shiftLeft = x64ShiftLeft;
      m->shiftRight = x64ShiftRight;

      m->cmp = x64Cmp;
      m->cmpSet = x64CmpSet;
      m->cmpJmp = x64CmpJmp;
      m->cmpJmpStackTop = x64CmpJmpStackTop;
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
      m->convertIntegerToFloat = x64ConvertIntegerToFloat;
      m->convertDoubleToInt = x64ConvertDoubleToInt;
      m->convertIntegerToDouble = x64ConvertIntegerToDouble;
      m->convertDoubleToFloat = x64ConvertDoubleToFloat;
      m->convertFloatToDouble = x64ConvertFloatToDouble;
   }
   #if defined(__MACH__)
      #pragma clang diagnostic pop
   #endif

   if (!(mflags & Config_TARGET_WIN)) {
      m64->params.intIdx = 0;

      m64->params.integerRegs[0] = Reg_RDI;
      m64->params.integerRegs[1] = Reg_RSI;
      m64->params.integerRegs[2] = Reg_RDX;
      m64->params.integerRegs[3] = Reg_RCX;
      m64->params.integerRegs[4] = Reg_R8;
      m64->params.integerRegs[5] = Reg_R9;

      m64->params.floatParamIdx = 0;

      m64->params.floatRegs[0] = Reg_XMM0;
      m64->params.floatRegs[1] = Reg_XMM1;
      m64->params.floatRegs[2] = Reg_XMM2;
      m64->params.floatRegs[3] = Reg_XMM3;
      m64->params.floatRegs[4] = Reg_XMM4;
      m64->params.floatRegs[5] = Reg_XMM5;
      m64->params.floatRegs[6] = Reg_XMM6;
      m64->params.floatRegs[7] = Reg_XMM7;
   }
   // Windows
   else {

   }


   return m;
}
