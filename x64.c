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
   int foo;
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
stackPushReg(Codegen* c, RegisterEnum reg) {
   instructionPrintf("push %s", locationString(c, registerLocation(reg), 64));
   c->stack_offset += 8;
   bufPush(c->stack, (StackValue){ .type = Stack_QWORD });
}

void
stackPushImm(Codegen* c, i64 val) {
   instructionPrintf("push %d", val);
   c->stack_offset += 8;
   bufPush(c->stack, (StackValue){ .type = Stack_QWORD });
}

void
stackPushOffset(Codegen* c, u64 bytes) {
   Assert(bytes);
   instructionPrintf("sub rsp, %d", bytes);
   c->stack_offset += bytes;
   StackValue val = { .type = Stack_OFFSET, .offset = bytes };
   bufPush(c->stack, val);
}

void
instructionReg(Codegen* c, char* asm_line, int bits, ...) {
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
         instructionPrintf(asm_line);
      } break;
      case 1: {
         instructionPrintf(asm_line,
                           locationString(c, registerLocation(regs[0]), bits));
      } break;
      case 2: {
         instructionPrintf(asm_line,
                           locationString(c, registerLocation(regs[0]), bits),
                           locationString(c, registerLocation(regs[1]), bits));
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
movOrCopy(Codegen* c, Location out, Location in, int bits) {
   if (bits <= 64) {
      instructionPrintf("mov %s, %s",
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
            instructionReg(c, "mov rsi, %s", 64, in);
         }
         else if (in.type == Location_STACK) {
            instructionPrintf("mov rsi, rsp");
            if (in.offset != c->stack_offset) {          // TODO lea
               instructionPrintf("add rsi, %d", c->stack_offset - in.offset);
            }
         }
         instructionPrintf("mov rdi, rsp");
         if (out.offset != c->stack_offset) {
            instructionPrintf("add rdi, %d", c->stack_offset - out.offset);
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
stackPop(Codegen* c, RegisterEnum reg) {
   StackValue s = bufPop(c->stack);
   switch (s.type) {
      case Stack_QWORD: {
         instructionReg(c, "pop %s", reg);
         c->stack_offset -= 8;
      } break;
      case Stack_OFFSET: {
         c->stack_offset -= s.offset;
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
         movOrCopy(c, registerLocation(r), registerLocation(Reg_RAX), typeBits(&etype->c));
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
         loc.offset = c->stack_offset;
      }
      movOrCopy(c, loc, etype->location, typeBits(&etype->c));
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

// Forward declaration for recursive calls.
void codegenEmit(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target);

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
      case Ast_ADD: { instructionReg(c, "add %s, %s", bits, Reg_RAX, Reg_RBX); } break;
      case Ast_SUB: { instructionReg(c, "sub %s, %s", bits, Reg_RAX, Reg_RBX); } break;
      case Ast_MUL: { instructionReg(c, "imul %s", bits, Reg_RBX); } break;
      case Ast_DIV: { instructionReg(c, "idiv %s", bits, Reg_RBX); } break;
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
         instructionPrintf("mov rax, rsp");
         // TODO: LEA
         instructionPrintf("add rax, %d", c->stack_offset - entry->location.offset);
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
            instructionPrintf("xor %s, %s",
                              locationString(c, registerLocation(Reg_RAX), 64),
                              locationString(c, registerLocation(Reg_RAX), 64));
         }
         movOrCopy(c, registerLocation(Reg_RAX), loc, typeBits(&entry->c));

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
         movOrCopy(c, registerLocation(Reg_RAX), address, bits);

         if (target == Target_STACK) {
            stackPushReg(c, Reg_RAX);
         }
      }
   }
   else if (address.type == Location_REGISTER) {
      if (target != Target_NONE) {
         instructionPrintf("mov %s, [%s + %d]",
                           locationString(c, (Location){ .type=Location_REGISTER, .reg=Reg_RAX }, typeBits(member->ctype)),
                           g_registers[address.reg].reg,
                           member_offset);
         if (target == Target_STACK) {
            stackPushReg(c, address.reg);
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
                                    "mov %s, %d",
                                    locationString(c, registerLocation(Reg_RAX), bits),
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
                     .reg = c->stack_offset,
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
               movOrCopy(c, lhs_type.location, rhs_type.location, bits);
            }
            else {
               Assert(bits < 64);
               // TODO: Check for arithmetic type here.
               instructionPrintf("mov %s, %s",
                                 locationString(c, registerLocation(Reg_RBX), bits),
                                 locationString(c, lhs_type.location, bits));
               switch (op->value) {
                  case ASSIGN_INCREMENT: {
                     instructionReg(c, "add %s, %s", bits, Reg_RBX, Reg_RAX);
                  } break;
                  default: {
                     NotImplemented("Different assignment expressions");
                  }
               }

               instructionPrintf("mov %s, %s",
                                 locationString(c, lhs_type.location, bits),
                                 locationString(c, registerLocation(Reg_RBX), bits));

               if (target == Target_ACCUM) {
                  instructionPrintf("mov %s, %s",
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

            instructionPrintf("mov %s, %s",
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
                     instructionPrintf("mov rax, rsp");
                     instructionPrintf("add rax, %d", c->stack_offset - loc->offset);
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

               instructionReg(c, "cmp %s, %s", typeBits(&left_type.c), Reg_RAX, Reg_RBX);

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
               instructionReg(c, instr, 8 /* SETCC operates on byte registers*/, Reg_RAX);

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
         instructionReg(c, "cmp %s, %s", typeBits(&left_type.c), Reg_RAX, Reg_RBX);
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
         instructionReg(c, "cmp %s, %s", typeBits(&expr_type.c), Reg_RAX, Reg_RBX);
         instructionPrintf("jne %s", then);
         instructionPrintf("jmp %s", els);
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
                                     .c = Zero,
                                     .location = { .type = Location_STACK, .offset = c->stack_offset },
                                  });

      if (declarator->is_pointer) {
         entry->c.type = Type_POINTER;
         entry->c.pointer.pointee = AllocType(c->arena, ExprType);
         entry->c.pointer.pointee->c = specifier->ctype;
      }
      else {
         entry->c = specifier->ctype;
      }

      if (isLiteral(rhs)) {               // Literal right-hand-side
         b32 is_integer = rhs->tok->type == TType_NUMBER;
         if (is_integer) {
            if (!isIntegerType(&entry->c) && entry->c.type != Type_POINTER) {
               DevBreak("Before implementing floating point variables, we need an instruction abstraction with typed registers.")
               if (entry->c.type == Type_FLOAT) {  // Float lhs, integer literal rhs
                  switch (typeBits(&entry->c)) {
                     // NOTE: Nasm provides the conversion for us, and we
                     // probably don't need to think about floating point
                     // constants until we make our own assembler.
                     case 64: {
                        instructionPrintf("mov DWORD [ rsp ], __float64__(%f)", rhs->tok->cast.real32);
                     } break;
                     case 32: {
                        instructionPrintf("mov DWORD [ rsp ], __float32__(%f)", rhs->tok->cast.real32);
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
               int value = rhs->tok->value;

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
      else if (rhs->type != Ast_NONE) {    // Non-literal right-hand-side.
         ExprType type = Zero;
         emitExpression(c, rhs, &type, Target_ACCUM);
         movOrCopy(c, entry->location, type.location, typeBits(&type.c));
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
            instructionPrintf("jmp .func_end");
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
         instructionPrintf("%s:", then_label);
         if (then) {
            codegenEmit(c, then, NULL, Target_NONE);
         }
         else {
            codegenError("No then after if");
         }
         //instructionPrintf(then_label);
         instructionPrintf("%s:", else_label);
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

         instructionPrintf("%s:", loop_label);
         if (!after_is_control) {
            emitConditionalJump(c, control, body_label, end_label);
         }

         instructionPrintf("%s:", body_label);
         if (body->type != Ast_NONE) {
            emitStatement(c, body, Target_ACCUM);
         }
         if (after->type != Ast_NONE) {
            emitStatement(c, after, Target_ACCUM);
         }
         instructionPrintf("jmp %s", loop_label);
         instructionPrintf("%s:", end_label);
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
codegenFinish(void) {
   char* end =
      "int 3\n"
      "ret\n";
   fwrite(end, 1, strlen(end), g_asm);
   fclose(g_asm);
}


void
machineInit(Codegen* c)
{
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

TypedRegister
trInt(u64 i)
{
   TypedRegister r = { TypedR_IMMEDIATE_INTEGER, i };
   return r;
}


b32
trIsRegister(TypedRegister r) {
   b32 reg = false;
   switch (r.type) {
      case TypedR_INTEGER:
      case TypedR_FLOAT: {
         reg = true;
      } break;
   }
   return reg;
}


void
machFunctionPrelude(char* func_name) {
   instructionPrintf("global %s", func_name);
   instructionPrintf("%s:", func_name);
   instructionPrintf("push rbp");
   instructionPrintf("mov rbp, rsp");
}
