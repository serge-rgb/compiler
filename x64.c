static Machine* g_mach;
static FILE* g_asm;


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
   StackValue* stack;  // Stack allocation / de-allocation is done on a per-function basis.
   u64         stack_offset;  // # Bytes from the bottom of the stack to RSP.
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

   exit(-1);
}

 ;

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
      default: {
         // WTF
         InvalidCodePath;
      } break;
   }
   Assert(res);
   return res;
}


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


char*
codegenHtmlHidden(Codegen* c, u64 line_number) {
   char* hidden = allocate(c->arena, LineMax);
   snprintf(hidden, LineMax, "%s: %" FORMAT_I64 , c->file_name, line_number);
   return hidden;
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
stackPushReg(Machine* m, RegisterEnum reg) {
   instructionPrintf("push %s", locationString(m, registerLocation(reg), 64));
   m->stack_offset += 8;
   bufPush(m->stack, (StackValue){ .type = Stack_QWORD });
}

void
stackPushImm(Codegen* c, i64 val) {
   instructionPrintf("push %d", val);
   c->m->stack_offset += 8;
   bufPush(c->m->stack, (StackValue){ .type = Stack_QWORD });
}

void
stackPushOffset(Codegen* c, u64 bytes) {
   Assert(bytes);
   instructionPrintf("sub rsp, %d", bytes);
   c->m->stack_offset += bytes;
   StackValue val = { .type = Stack_OFFSET, .offset = bytes };
   bufPush(c->m->stack, val);
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
movOrCopy(Machine* m, Location out, Location in, int bits) {
   if (bits <= 64) {
      instructionPrintf("mov %s, %s",
                        locationString(m, out, bits),
                        locationString(m, in, bits));
   }
   else {
      if (out.type == Location_REGISTER) {
         NotImplemented("Big copy - into register.");
      }
      else if (out.type == Location_STACK) {
         int bytes = bits/8;
         if (in.type == Location_REGISTER) {
            instructionReg(m, "mov rsi, %s", 64, in);
         }
         else if (in.type == Location_STACK) {
            instructionPrintf("mov rsi, rsp");
            if (in.offset != m->stack_offset) {          // TODO lea
               instructionPrintf("add rsi, %d", m->stack_offset - in.offset);
            }
         }
         instructionPrintf("mov rdi, rsp");
         if (out.offset != m->stack_offset) {
            instructionPrintf("add rdi, %d", m->stack_offset - out.offset);
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
stackPop(Machine* m, RegisterEnum reg) {
   StackValue s = bufPop(m->stack);
   switch (s.type) {
      case Stack_QWORD: {
         instructionReg(m, "pop %s", 64, reg);
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
         movOrCopy(m, registerLocation(r), registerLocation(Reg_RAX), typeBits(&etype->c));
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
         }
      }
      else {
         stackPushOffset(c, typeBits(&etype->c) / 8);
         loc.type = Location_STACK;
         loc.offset = c->m->stack_offset;
      }
      movOrCopy(m, loc, etype->location, typeBits(&etype->c));
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
            loc = (Location){ .type = Location_STACK, .offset = c->m->stack_offset - *offset };
            *offset += typeBits(ctype);
         }
      }
      else if (c->config & Config_TARGET_WIN){
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

// Forward declaration for recursive calls.
void codegenEmit(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target);

void
emitArithBinaryExpr(Codegen* c, AstType type, ExprType* expr_type,
                    AstNode* left, AstNode* right, EmitTarget target) {

   Machine* m = c->m;

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

   stackPop(m, Reg_RBX);

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
      case Ast_ADD: { instructionReg(m, "add %s, %s", bits, Reg_RAX, Reg_RBX); } break;
      case Ast_SUB: { instructionReg(m, "sub %s, %s", bits, Reg_RAX, Reg_RBX); } break;
      case Ast_MUL: { instructionReg(m, "imul %s", bits, Reg_RBX); } break;
      case Ast_DIV: { instructionReg(m, "idiv %s", bits, Reg_RBX); } break;
      default: break;
   }

   if (target == Target_STACK) {
      stackPushReg(m, Reg_RAX);
   }
}

void
emitIdentifier(Codegen*c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   Machine* m = c->m;
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
         instructionPrintf("mov rax, rsp");
         // TODO: LEA
         instructionPrintf("add rax, %d", c->m->stack_offset - entry->location.offset);
         if (target == Target_STACK) {
            stackPushReg(m, Reg_RAX);
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
            instructionPrintf("xor %s, %s",
                              locationString(m, registerLocation(Reg_RAX), 64),
                              locationString(m, registerLocation(Reg_RAX), 64));
         }
         movOrCopy(m, registerLocation(Reg_RAX), loc, typeBits(&entry->c));

         if (target == Target_STACK) {
            stackPushReg(m, Reg_RAX);
         }
      }
   }

   if (expr_type) {
      expr_type->c = entry->c;
      expr_type->location = loc;
   }

}

void
emitStructMemberAccess(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   Machine* m = c->m;

   char* struct_str = node->child->tok->cast.string;
   char* field_str = node->child->next->tok->cast.string;
   ExprType* symbol_entry = findSymbol(c, struct_str);
   if (!symbol_entry) {
      codegenError("%s undeclared.", struct_str);
   }

   Ctype *ctype = NULL;
   Location address = Zero;
   if (symbol_entry->c.type == Type_POINTER) {
      ctype = &symbol_entry->c.pointer.pointee->c;
      address = symbol_entry->location;
   }
   else {
      Assert(symbol_entry->c.type = Type_AGGREGATE);
      ctype = &symbol_entry->c;
      address = symbol_entry->location;
   }

   char* tag_str = ctype->aggr.tag;
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
   u64 member_offset = member->offset;
   int bits = typeBits(member->ctype);

   if (address.type == Location_STACK) {
      address.offset = symbol_entry->location.offset - member_offset;

      if (target != Target_NONE) {
         movOrCopy(m, registerLocation(Reg_RAX), address, bits);

         if (target == Target_STACK) {
            stackPushReg(m, Reg_RAX);
         }
      }
   }
   else if (address.type == Location_REGISTER) {
      if (target != Target_NONE) {
         instructionPrintf("mov %s, [%s + %d]",
                           locationString(m, (Location){ .type=Location_REGISTER, .reg=Reg_RAX }, typeBits(member->ctype)),
                           g_registers[address.reg].reg,
                           member_offset);
         if (target == Target_STACK) {
            stackPushReg(m, address.reg);
         }
      }
   }
   if (expr_type) {
      expr_type->c = *(member->ctype);
      expr_type->location = address;
   }
}

void emitExpression(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target);  // forward decl

b32
typesAreCompatible(Codegen* c, Ctype a, Ctype b) {
   b32 compatible = false;
   if (a.type == b.type) {
      if (a.type == Type_POINTER) {
         compatible = typesAreCompatible(c,
                                         a.pointer.pointee->c,
                                         b.pointer.pointee->c);
      }
      else {
         switch (a.type) {
            case Type_AGGREGATE: {
               // TODO: Anonymous structs
               if ((a.aggr.tag && b.aggr.tag)) {
                  ExprType* aggr_a = findTag(c, a.aggr.tag);
                  ExprType* aggr_b = findTag(c, b.aggr.tag);
                  // TODO: We could return here by just having the same tag. C
                  // spec says (6.7.2) that when two types have the same tag,
                  // defined in different translation units, they must have:
                  //    - The same number of parameters.
                  //    - Every corresponding parameter is compatible.
                  if (aggr_a == aggr_b) {
                     compatible = true;
                  }
                  // SPEC: We deviate from the spec by always considering two
                  // structs with the same layout as compatible.
                  else {
                     u64 n_a = bufCount(aggr_a->c.aggr.members);
                     u64 n_b = bufCount(aggr_b->c.aggr.members);
                     if (n_a == n_b) {
                        compatible = true;
                        for (u64 i = 0; i < n_a; ++i) {
                           // TODO: Alignment check.
                           if (!typesAreCompatible(c,
                                                   *aggr_a->c.aggr.members[i].ctype,
                                                   *aggr_b->c.aggr.members[i].ctype)) {
                              compatible = false;
                              break;
                           }
                        }
                     }
                  }
               }
            } break;
            default: {
               compatible = true;
            } break;
         }
      }
   }
   else {
      if (a.type == Type_POINTER || b.type == Type_POINTER) {
         compatible = false;
      }
      else {
         NotImplemented("Compatibility rules");
      }
   }
   return compatible;
}

void
emitFunctionCall(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   Machine* m = c->m;
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

   u64 stack_top = bufCount(c->m->stack);

   // Check count
   {
      u64 n_param = 0;
      for (AstNode* param = params;
           param != NULL;
           param = param->next) {
         n_param++;
      }

      u64 expected_nparam = funcNumParams(sym->c.func.node);
      if (n_param != expected_nparam) {
         codegenError("Wrong number of arguments in call to %s. Expected %d but got %d.",
                      label, expected_nparam, n_param);
      }
   }

   // Put the parameters in registers and/or the stack.
   u64 n_param = 0;

   AstNode* expected_param = funcParams(sym->c.func.node);

   for (AstNode* param = params;
        param != NULL;
        param = param->next) {
      ExprType et = {0};
      emitExpression(c, param, &et, Target_NONE);
      ExprType expected_et = {0};
      ExprType pointee = {0};
      expected_et.c.pointer.pointee = &pointee;
      if (!typesAreCompatible(c, et.c, (paramType(&expected_et.c, expected_param), expected_et.c))) {
         codegenError("Attempting to pass incompatible parameter to function.");
      }
      pushParameter(c, n_param++, &et);
      expected_param = expected_param->next;
   }

   instructionPrintf("call %s", label);
   c->m->stack_offset += pointerSizeBits() / 8;

   while (bufCount(c->m->stack) != stack_top) {
      stackPop(m, Reg_RBX);
   }
   // TODO: Restore registers. Not necessary at the moment because of DDCG

   if (target == Target_STACK) {
      stackPushReg(m, Reg_RAX);
   }

   AstNode* funcdef = sym->c.func.node;
   Assert(funcdef->type == Ast_FUNCDEF);

   expr_type->c = funcdef->child->ctype;
   expr_type->location = registerLocation(Reg_RAX);
}

void
emitExpression(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   Machine* m = c->m;
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
                                    "mov %s, %d",
                                    locationString(m, registerLocation(Reg_RAX), bits),
                                    node->tok->cast.int32);
                  expr_type->location = (Location) {
                     .type = Location_REGISTER,
                     .reg = Reg_RAX,
                  };
               } break;
               case Target_STACK: {
                  stackPushImm(c, node->tok->value);
                  expr_type->location = (Location) {
                     .type = Location_STACK,
                     .reg = c->m->stack_offset,
                  };
               } break;
               case Target_NONE: {
                  expr_type->location = (Location) {
                     .type = Location_IMMEDIATE,
                     .immediate_value = node->tok->value,
                  };
               } break;
               case Target_TMP: {
                  InvalidCodePath;  // Will remove this case later.
               } break;
            }
            expr_type->c.type = Type_INT;
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
               movOrCopy(c->m, lhs_type.location, rhs_type.location, bits);
            }
            else {
               Assert(bits < 64);
               // TODO: Check for arithmetic type here.
               instructionPrintf("mov %s, %s",
                                 locationString(m, registerLocation(Reg_RBX), bits),
                                 locationString(m, lhs_type.location, bits));
               switch (op->value) {
                  case ASSIGN_INCREMENT: {
                     instructionReg(c->m, "add %s, %s", bits, Reg_RBX, Reg_RAX);
                  } break;
                  default: {
                     NotImplemented("Different assignment expressions");
                  }
               }

               instructionPrintf("mov %s, %s",
                                 locationString(m, lhs_type.location, bits),
                                 locationString(m, registerLocation(Reg_RBX), bits));

               if (target == Target_ACCUM) {
                  instructionPrintf("mov %s, %s",
                                    locationString(m, registerLocation(Reg_RAX), bits),
                                    locationString(m, lhs_type.location, bits));
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

            instructionPrintf("mov %s, %s",
                              locationString(m, var, typeBits(&local_etype.c)),
                              locationString(m, registerLocation(Reg_RAX), typeBits(&local_etype.c)));
            if (target == Target_STACK) {
               // Result is already on the stack.
            }
            else if (target == Target_ACCUM) {
               // Return old value.
               stackPop(c->m, Reg_RAX);
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
                     instructionPrintf("mov rax, rsp");
                     instructionPrintf("add rax, %d", c->m->stack_offset - loc->offset);
                  } break;
                  default: {
                     NotImplemented("Address of something not on the stack");
                  }
               }
               if (target == Target_STACK) {
                  stackPushReg(c->m, Reg_RAX);
                  result.location = (Location){ .type = Location_STACK, .offset = c->m->stack_offset };
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
               stackPop(c->m, Reg_RBX);

               instructionReg(c->m, "cmp %s, %s", typeBits(&left_type.c), Reg_RAX, Reg_RBX);

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
               instructionReg(c->m, instr, 8 /* SETCC operates on byte registers*/, Reg_RAX);

               if (target == Target_STACK) {
                  stackPushReg(c->m, Reg_RAX);
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
         stackPop(c->m, Reg_RBX);
         if (typeBits(&left_type.c) != typeBits(&right_type.c)) {
            NotImplemented("Promotion rules");
         }
         instructionReg(c->m, "cmp %s, %s", typeBits(&left_type.c), Reg_RAX, Reg_RBX);
         switch(cond->type) {
            case Ast_EQUALS: { instructionPrintf("je %s", then); } break;
            case Ast_LESS: { instructionPrintf("jl %s", then); } break;
            case Ast_LEQ: { instructionPrintf("jle %s", then); } break;
            case Ast_GREATER: { instructionPrintf("jg %s", then); } break;
            case Ast_GEQ: { instructionPrintf("jge %s", then); } break;
            case Ast_NOT_EQUALS: { instructionPrintf("jne %s", then); } break;
            default: InvalidCodePath; break;
         }
         instructionPrintf("jmp %s", els);
      } break;
      default: {
         codegenEmit(c, cond, &expr_type, Target_ACCUM);
         instructionReg(c->m, "cmp %s, %s", typeBits(&expr_type.c), Reg_RAX, Reg_RBX);
         instructionPrintf("jne %s", then);
         instructionPrintf("jmp %s", els);
      } break;
   }
}



// ==================================
// TODO: Abstract away x86 code from functions above and move back to codegen.
// ==================================

void
machMovStackTop(Machine* m, ExprType* entry, Token* rhs_tok) {
   // Entry is at the top of the stack. Move the immediate value represented by rhs_tok.
   b32 is_integer = rhs_tok->type == TType_NUMBER;
   if (is_integer) {
      if (!isIntegerType(&entry->c) && entry->c.type != Type_POINTER) {
         if (entry->c.type == Type_FLOAT) {  // Float lhs, integer literal rhs
            switch (typeBits(&entry->c)) {
               // NOTE: Nasm provides the conversion for us, and we
               // probably don't need to think about floating point
               // constants until we make our own assembler.
               case 64: {
                  instructionPrintf("mov QWORD [ rsp ], __float64__(%f)", rhs_tok->cast.real32);
               } break;
               case 32: {
                  instructionPrintf("mov DWORD [ rsp ], __float32__(%f)", rhs_tok->cast.real32);
               } break;
               case 16:
               case 8:
               default: {
                  codegenError("Cannot put a floating point value in less than 32 bits.");
               } break;
            }
         }
         else {
            codegenError("Cannot convert integer literal to non-integer type.");
         }
      }
      else {
         int value = rhs_tok->value;

         switch (typeBits(&entry->c)) {
            case 64: {
               instructionPrintf("mov QWORD [ rsp ], 0x%x", value);
            } break;
            case 32: {
               instructionPrintf("mov DWORD [ rsp ], 0x%x", value);
            } break;
            case 16: {
               instructionPrintf("mov WORD [ rsp ], 0x%x", value);
            } break;
            case 8: {
               instructionPrintf("mov BYTE [ rsp ], 0x%x", value);
            } break;
            default: {
               InvalidCodePath;
            } break;
         }
      }
   }
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
machInit(Codegen* c) {
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
   c->m = AllocType(c->arena, Machine);
}

void
machFunctionPrelude(Machine* m, char* func_name) {
   instructionPrintf("global %s", func_name);
   instructionPrintf("%s:", func_name);
   instructionPrintf("push rbp");
   instructionPrintf("mov rbp, rsp");

   m->stack_offset += 8;
}

void
machFunctionEpilogue(Machine* m) {
   instructionPrintf(".func_end:");

   while (bufCount(m->stack) > 0)  {
      stackPop(m, Reg_RBX);
   }

   // Restore non-volatile registers.

   instructionPrintf("pop rbp");
   instructionPrintf("ret");
}

void
machLabel(char* label) {
   instructionPrintf("%s:", label);
}

void
machJumpToLabel(char* label) {
   instructionPrintf("jmp %s", label);
}
