enum InstrOutput {
   InstrOutput_DISABLED,
   InstrOutput_ENABLED,
} typedef InstrOutput;

void
codegenInit(Codegen* c, char* outfile, MachineConfigFlags mflags) {
   char asmfile [PathMax] = Zero;
   snprintf(asmfile, PathMax, "%s.asm", outfile);
   g_asm = fopen(asmfile, "w");

   c->m = makeMachineX64(c->arena, mflags);

   // Constants
   c->one = makeAstNode(c->arena, Ast_NUMBER, 0, 0);
   Token* one_tok = AllocType(c->arena, Token);
   one_tok->type = TType_NUMBER;
   one_tok->value = 1;
   c->one->as_number.tok = one_tok;

   c->instrOutputStack[ c->n_instrOutputStack++ ] = InstrOutput_ENABLED;
}

Tag*
findTag(Scope* scope, char* name) {
   Tag* tag = NULL;
   while (scope) {
      tag = tagGet(&scope->tag_table, name);
      if (tag) break;
      else scope = scope->prev;
   }

   return tag;
}

RegVar*
findSymbol(Codegen* c, char* name) {
   RegVar* entry = NULL;
   Scope* scope = c->scope;
   while (scope) {
      entry = symGet(&scope->symbol_table, name);
      if (entry) break;
      else scope = scope->prev;
   }

   return entry;
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
pushInstructionOutput(Codegen* c, InstrOutput io) {
   if (c->n_instrOutputStack >= MaxInstrOutputStack) {
      codegenError("instrOutputStack stack overflow");
   }

   c->instrOutputStack[ c->n_instrOutputStack++ ] = io;

   // TODO: Instead of using this flag, we can create a dummy machine that has
   // stubs for all functions.

   if (io == InstrOutput_ENABLED) {
      c->m->flags &= ~Config_INSTR_OUTPUT_DISABLED;
   }
   else {
      c->m->flags |= Config_INSTR_OUTPUT_DISABLED;
   }
}

void
popInstructionOutput(Codegen* c) {
   --c->n_instrOutputStack;

   if (!c->n_instrOutputStack) {
      codegenError("Unexpected stack pop of instrOutputStack");
   }

   InstrOutput io = c->instrOutputStack[ c->n_instrOutputStack - 1 ];

   if (io == InstrOutput_ENABLED) {
      c->m->flags &= ~Config_INSTR_OUTPUT_DISABLED;
   }
   else {
      c->m->flags |= Config_INSTR_OUTPUT_DISABLED;
   }
}

// Forward declaration for recursive calls.
void codegenEmit(Codegen* c, AstNode* node, RegVar* reg_var, EmitTarget target);

b32
typesAreCompatible(Codegen* c, Ctype into, Ctype from) {
   b32 compatible = false;
   if (into.type == from.type) {
      if (into.type == Type_POINTER) {
         compatible = typesAreCompatible(c,
                                         *into.pointer.pointee,
                                         *from.pointer.pointee);
      }
      else {
         switch (into.type) {
            case Type_AGGREGATE: {
               // TODO: Anonymous structs
               if ((into.aggr.tag && from.aggr.tag)) {
                  Tag* tag_a = findTag(c->scope, into.aggr.tag);
                  Tag* tag_b = findTag(c->scope, from.aggr.tag);
                  // TODO: We could return here by just having the same tag. C
                  // spec says (6.2.7) that when two types have the same tag,
                  // defined in different translation units, they must have:
                  //    - The same number of parameters.
                  //    - Every corresponding parameter is compatible.
                  if (tag_a == tag_b) {
                     compatible = true;
                  }
                  else {
                     // TODO: compatible structs accross translation unit
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
      if (into.type == Type_POINTER || from.type == Type_POINTER) {
         compatible = false;
      }
      else {
         if ((isIntegerType(&into) || isRealType(&into)) &&
             (isIntegerType(&from) || isRealType(&from))) {
            compatible = true;
         }
         else if ((into.type == Type_ENUM && isIntegerType(&from)) ||
                  (from.type == Type_ENUM && isIntegerType(&into))) {
            compatible = true;
         }
      }
   }
   if (compatible) {
      // Check for identical qualifiers
      compatible = into.qualifiers == from.qualifiers;
   }
   return compatible;
}

b32
maybeEmitTypeConversion(Codegen* c, RegVar* value, Ctype target_type, EmitTarget target, char* incompatibleError) {
   Machine* m = c->m;
   b32 didConversion = true;
   if (!typesAreCompatible(c, target_type, value->c)) {
      codegenError(incompatibleError);
   }

   if (value->c.type == target_type.type
       || (isIntegerType(&value->c) && isIntegerType(&target_type))) {
      // Nothing to do
      didConversion = false;
   }
   else {
      Location value_loc = value->location;
      if (value_loc.type == Location_IMMEDIATE) {
         RegVar* helper = m->helperC(m, value->c);
         m->mov(m, helper, value);
         value_loc = helper->location;
      }
      // int <-> float
      if (value->c.type == Type_FLOAT &&
               target_type.type == Type_INT) {
         if (target != Target_NONE) {
            m->convertFloatToInt(m, value_loc, target);
         }
      }
      else if (isIntegerType(&value->c) &&
               target_type.type == Type_FLOAT) {
         if (target != Target_NONE) {
            m->convertIntegerToFloat(m, value_loc, target);
         }
      }
      // int <-> double
      else if (value->c.type == Type_DOUBLE &&
         target_type.type == Type_INT) {
         m->convertDoubleToInt(m, value_loc, target);
      }
      else if (isIntegerType(&value->c) &&
         target_type.type == Type_DOUBLE) {
         if (target != Target_NONE) {
            m->convertIntegerToDouble(m, value_loc, target);
         }
      }
      else if (isRealType(&value->c) && isRealType(&target_type)) {
         if (value->c.type == Type_DOUBLE) {
            m->convertDoubleToFloat(m, value_loc, target);
         }
         else {
            m->convertFloatToDouble(m, value_loc, target);
         }
      }
      else {
         NotImplemented("type conversion.");
      }
   }
   return didConversion;
}

void
emitArithBinaryExpr(Codegen* c, AstType type, RegVar* reg_var,
                    AstNode* left, AstNode* right, EmitTarget target) {
   Machine* m = c->m;

   RegVar tleft = {0};
   RegVar tright = {0};

   codegenEmit(c, right, &tright, Target_STACK);
   codegenEmit(c, left, &tleft, Target_ACCUM);

   if ( !isArithmeticType(tleft.c) ) {
      codegenError("Left operator in binary expression is not arithmetic type.");
   }
   else if ( !isArithmeticType(tright.c) ) {
      codegenError("Left operator in expression is not arithmetic type.");
   }

   Ctype new_ctype = arithmeticTypeConversion(tleft.c, tright.c);
   RegVar* helper = m->helper(m, tright.c.type, typeBits(&tright.c));
   m->stackPop(m, helper);

   if (maybeEmitTypeConversion(c,
                            helper,
                            new_ctype,
                            Target_STACK,
                            "Incompatible types in binary expression")) {
      helper = m->helper(m, new_ctype.type, typeBits(&tright.c));
      m->stackPop(m, helper);
   }

   maybeEmitTypeConversion(c,
                           &tleft,
                           new_ctype,
                           Target_ACCUM,
                           "Incompatible types in binary expression");

   if (reg_var) {
      *reg_var = tleft;
   }

   RegVar* dst = m->accum(m, new_ctype.type, typeBits(&new_ctype));
   RegVar* src = m->helper(m, new_ctype.type, typeBits(&new_ctype));

   switch (type) {
      case Ast_ADD: { m->add(m, dst, src); } break;
      case Ast_SUB: { m->sub(m, dst, src); } break;
      case Ast_MUL: { m->mul(m, dst, src); } break;
      case Ast_DIV: { m->div(m, dst, src); } break;
      default: break;
   }

   if (target == Target_STACK) {
      m->stackPushReg(m, m->accumC(m, new_ctype)->location.reg);
   }
}

RegVar*
getAddress(Codegen* c, AstNode* node) {
   RegVar* result = NULL;
   switch(node->type) {
      case Ast_ID: {
         result = findSymbol(c, node->as_id.tok->cast.string);
      } break;
   }
   return result;
}

void
emitIdentifier(Codegen*c, AstNode* node, RegVar* reg_var, EmitTarget target) {
   Assert(reg_var);

   Machine* m = c->m;
   char* id_str = node->as_id.tok->cast.string;
   RegVar* entry = findSymbol(c, id_str);

   Assert (entry->location.type == Location_STACK);

   reg_var->c = entry->c;
   reg_var->location = entry->location;

   if (!entry) {
      codegenError("Use of undeclared identifier %s", node->as_id.tok->cast.string);
   }

   if (typeBits(&entry->c) > 64) {
      if (target == Target_ACCUM) {
         RegVar* accum = m->accum(m, entry->c.type, pointerSizeBits());
         m->stackAddress(m, entry, accum->location);
         reg_var->location = (Location) {
            .type = Location_REG_POINTER,
            .reg = accum->location.reg,
         };
      }
      else if (target == Target_STACK) {
         Location loc = m->stackPushOffset(m, typeBits(&entry->c)/8);
         RegVar rvar = { .c = entry->c, .location = loc };
         m->mov(m, &rvar, entry);
         reg_var->location = loc;
      }
   }
   else {
      if (target == Target_STACK) {
         RegVar* helper = m->helperC(m, entry->c);
         m->mov(m, helper, entry);
         Location loc = m->stackPushReg(m, helper->location.reg);
         reg_var->location = loc;
      }
      else if (target == Target_ACCUM) {
         RegVar* accum = m->accumC(m, entry->c);
         m->mov(m, accum, entry);
         reg_var->location = accum->location;
      }
   }
}

char*
getLabelForPrimaryExpr(Codegen* g, AstNode* id) {
   char* result = NULL;
   if (id->type == Ast_ID) {
      result = id->as_id.tok->cast.string;
   }
   else {
      NotImplemented("Function label for non-id primary expressions");
   }
   return result;
}

void
emitStructMemberAccess(Codegen* c, AstNode* node, RegVar* reg_var, EmitTarget target) {
   Machine* m = c->m;

   char* struct_str = getLabelForPrimaryExpr(c, node->as_member_access.primary_expr);
   char* field_str = node->as_member_access.field;
   RegVar* symbol_entry = findSymbol(c, struct_str);
   if (!symbol_entry) {
      codegenError("%s undeclared.", struct_str);
   }

   Ctype *ctype = NULL;
   Location address = Zero;
   if (symbol_entry->c.type == Type_POINTER) {
      ctype = symbol_entry->c.pointer.pointee;
      RegVar* helper = m->helperC(m, symbol_entry->c);
      m->mov(m, helper, symbol_entry);
      address = helper->location;
   }
   else {
      Assert(symbol_entry->c.type = Type_AGGREGATE);
      ctype = &symbol_entry->c;
      address = symbol_entry->location;
   }

   char* tag_str = ctype->aggr.tag;
   Tag* struct_entry = findTag(c->scope, tag_str);
   if (!struct_entry) {
      codegenError("No struct named %s", tag_str);
   }
   struct TagMember* s_members = struct_entry->s_members;

   u64 member_idx = MaxU64;
   for (u64 i = 0; i < bufCount(s_members); ++i) {
      if (!strcmp(s_members[i].id, field_str)) {
         member_idx = i;
         break;
      }
   }
   if (member_idx == MaxU64) {
      codegenError("Struct %s does not have %s member", tag_str, field_str);
   }

   struct TagMember* member = s_members + member_idx;
   u64 member_offset = member->offset;

   if (address.type == Location_STACK) {
      address.offset = symbol_entry->location.offset - member_offset;

      RegVar reg = {
         .c = member->ctype,
         .location = address,
      };
      if (target == Target_ACCUM) {
         RegVar* accum = m->accumC(m, member->ctype);
         m->mov(m, accum, &reg);
      }
      else if (target == Target_STACK) {
         RegVar* helper = m->helperC(m, member->ctype);
         m->mov(m, helper, &reg);
         m->stackPushReg(m, helper->location.reg);
      }
   }
   else if (address.type == Location_REGISTER) {
      RegVar reg = {
         .c = member->ctype,
         .location = (Location){
            .type = Location_REG_POINTER,
            .reg = address.reg,
            .reg_offset = member_offset,
         },
      };
      if (target == Target_ACCUM) {
         RegVar* accum = m->accumC(m, member->ctype);
         m->mov(m, accum, &reg);
      }
      else if (target == Target_STACK) {
         RegVar* helper = m->helperC(m, member->ctype);
         m->mov(m, helper, &reg);
         m->stackPushReg(m, address.reg);
      }
   }
   if (reg_var) {
      reg_var->c = member->ctype;
      reg_var->location = address;
   }
}

void emitExpression(Codegen* c, AstNode* node, RegVar* reg_var, EmitTarget target); // Forward decl.

void
emitFunctionCall(Codegen* c, AstNode* node, RegVar* reg_var, EmitTarget target) {
   Machine* m = c->m;

   char* label = getLabelForPrimaryExpr(c, node->as_funccall.expr);

   RegVar* sym = findSymbol(c, label);
   if (!sym) {
      codegenError("Call to undefined function. %s", label);
   }
   Ctype* type = &sym->c;
   if (type->type != Type_FUNC) {
      codegenError("%s is not a function.", label);
   }

   struct AstNodeArgument* args = node->as_funccall.arg_expr_list;

   // TODO: Remove reference to MachineX64
   MachineX64* m_totally_temp = (MachineX64*)c->m;
   u64 stack_top = bufCount(m_totally_temp->s_stack);

   // Check count
   {
      u64 n_arg = 0;
      for (struct AstNodeArgument* arg = args;
           arg != NULL;
           arg = arg->next) {
         n_arg++;
      }

      u64 expected_nparam = bufCount(sym->c.func.s_params);
      if (n_arg != expected_nparam) {
         codegenError("Wrong number of arguments in call to %s. Expected %d but got %d.",
                      label, expected_nparam, n_arg);
      }
   }

   // Put the parameters in registers and/or the stack.
   Ctype* s_expected_params = sym->c.func.s_params;

   m->beginFuncParams(m);
   {
      u32 arg_idx = 0;
      for (struct AstNodeArgument* arg = args;
           arg != NULL;
           arg = arg->next) {
         RegVar et_buf = {0};
         RegVar* et = &et_buf;
         emitExpression(c, arg->expr, et, Target_ACCUM);
         Ctype expected_type = {0};
         //expected_et.c.pointer.pointee = &pointee;
         //paramType(&expected_et.c, expected_param);
         expected_type = s_expected_params[arg_idx];

         if (maybeEmitTypeConversion(c, et, expected_type, Target_STACK, "Attempting to pass incompatible parameter to function.")) {
            RegVar* helper = m->helperC(m, expected_type);
            m->stackPop(m, helper);
            et = helper;
         }

         m->pushParameter(m, c->scope, et);

         arg_idx++;
      }
   }
   m->endFuncParams(m);

   m->call(m, label);

   while (bufCount(m_totally_temp->s_stack) != stack_top) {
      RegVar* helper = m->helper(m, Type_INT, 64);
      m->stackPop(m, helper);
   }
   // TODO: Restore registers. Not necessary at the moment because of DDCG

   Ctype return_type = *sym->c.func.return_type;
   if (target == Target_STACK) {
      m->stackPushReg(m, m->accumC(m, return_type)->location.reg);
   }

   //Assert(func_node->type == Ast_FUNCDEF);

   reg_var->c = return_type;
   reg_var->location = m->accumC(m, return_type)->location;
}

void
emitExpression(Codegen* c, AstNode* node, RegVar* reg_var, EmitTarget target) {
   Machine* m = c->m;
   if (nodeIsExpression(node)) {
      switch (node->type) {
         case Ast_FUNCCALL: {
            emitFunctionCall(c, node, reg_var, target);
         } break;
         case Ast_NUMBER: {
            // Set type
            switch (node->as_number.tok->type) {
               case TType_NUMBER: {
                  reg_var->c.type = Type_INT;
               } break;
               case TType_FLOAT: {
                  reg_var->c.type = Type_FLOAT;
               } break;
               case TType_DOUBLE: {
                  reg_var->c.type = Type_DOUBLE;
               } break;
               default: {
                  InvalidCodePath;
               }
            }

            // Move
            switch (target) {
               case Target_ACCUM: {
                  RegVar* accum = m->accumC(m, reg_var->c);
                  m->mov(m,
                     accum,
                     m->immediateFromToken(m, node->as_number.tok));
                  reg_var->location = accum->location;
               } break;
               case Target_STACK: {
                  reg_var->location = m->stackPushImm(m, node->as_number.tok->value);
               } break;
               case Target_NONE: {
                  reg_var->location = (Location) {
                     .type = Location_IMMEDIATE,
                     .immediate_value = node->as_number.tok->value,
                  };
               } break;
            }
         } break;
         case Ast_ID: {
            emitIdentifier(c, node, reg_var, target);
         } break;
         case Ast_STRING_LITERAL: {
            char* str = node->as_string_literal.tok->cast.string;
            char** label = staticArrGet(c->hm_static_arrays, str);
            Assert(label);
            // m->declareStringLiteral(m, *label, str);

         } break;
         case Ast_STRUCT_MEMBER_ACCESS: {
            emitStructMemberAccess(c, node, reg_var, target);
         } break;
         // Assignment expressions
         case Ast_ASSIGN_EXPR: {
            AstNode* lhs = node->as_assignment_expr.lhs;
            AstNode* rhs = node->as_assignment_expr.rhs;
            Token* op = node->as_assignment_expr.op;

            RegVar lhs_type = Zero;

            codegenEmit(c, lhs, &lhs_type, Target_NONE); // Fill the location
            int bits = typeBits(&lhs_type.c);
            RegVar rhs_type_buf = Zero;
            RegVar* rhs_type = &rhs_type_buf;
            codegenEmit(c, rhs, rhs_type, Target_ACCUM);  // TODO: Don't emit mov if rhs is immediate.

            if (maybeEmitTypeConversion(c, rhs_type, lhs_type.c, Target_ACCUM, "Incompatible type in assignment")) {
               rhs_type = m->accumC(m, lhs_type.c);
            }

            if (op->value == '=') {
               m->mov(m, &lhs_type, rhs_type);
            }
            else {
               Assert(bits < 64);
               // TODO: Check for arithmetic type here.

               RegVar* helper = m->helper(m, lhs_type.c.type, 32);
               m->mov(m, helper, &lhs_type);

               RegVar* accum = m->accum(m, rhs_type->c.type, 32);

               switch (op->value) {
                  case ASSIGN_INCREMENT: {
                     m->add(m, helper, accum);
                  } break;
                  case ASSIGN_DECREMENT: {
                     m->sub(m, helper, accum);
                  } break;
                  case ASSIGN_MODULUS: {
                     m->mod(m, helper, accum);
                  } break;
                  case ASSIGN_BIT_AND: {
                     m->bitAnd(m, helper, accum);
                  } break;
                  case ASSIGN_MULTIPLY: {
                     m->mul(m, helper, accum);
                  } break;
                  case ASSIGN_DIVIDE: {
                     m->div(m, helper, accum);
                  } break;
                  case ASSIGN_SHIFT_LEFT: {
                     m->shiftLeft(m, helper, accum);
                  } break;
                  case ASSIGN_SHIFT_RIGHT: {
                     m->shiftRight(m, helper, accum);
                  } break;
                  case ASSIGN_BIT_XOR: {
                     m->bitXor(m, helper, accum);
                  } break;
                  case ASSIGN_BIT_OR: {
                     m->bitOr(m, helper, accum);
                  } break;
                  default: {
                     InvalidCodePath;
                  }
               }

               m->mov(m, &lhs_type, helper);

               if (target == Target_ACCUM) {
                  m->mov(m, m->accumC(m, lhs_type.c), &lhs_type);
               }
            }
         } break;
         case Ast_POSTFIX_INC:
         case Ast_POSTFIX_DEC: {
            AstNode* expr = node->as_single_expr.expr;
            RegVar local_rvar = Zero;
            emitExpression(c, expr, &local_rvar, Target_STACK);
            if (local_rvar.location.type == Location_IMMEDIATE) {
               codegenError("Attempting to increment an rvalue.");
            }

            switch (node->type) {
               case Ast_POSTFIX_INC: { emitArithBinaryExpr(c, Ast_ADD, NULL, expr, c->one, Target_ACCUM); } break;
               case Ast_POSTFIX_DEC: { emitArithBinaryExpr(c, Ast_SUB, NULL, expr, c->one, Target_ACCUM); } break;
               default: { InvalidCodePath; }
            }

            RegVar* accum = m->accumC(m, local_rvar.c);

            RegVar* addr = getAddress(c, expr);
            if (!addr) {
               codegenError("Trying to modify non-addressable.");
            }
            else {
               m->mov(c->m, addr, accum);
            }

            if (target == Target_STACK) {
               // Result is already on the stack.
            }
            else if (target == Target_ACCUM) {
               // Return old value.
               m->stackPop(m, accum);
            }
            else if (target == Target_NONE) {
               m->stackPop(m, m->helper(m, Type_INT, 64));
            }
            if (reg_var) {
               *reg_var = local_rvar;

            }
         } break;
         case Ast_ADDRESS: {
            AstNode* expr = node->as_single_expr.expr;
            RegVar* pointee = AllocType(c->arena, RegVar);
            emitExpression(c, expr, pointee, Target_NONE);

            if (pointee->location.type != Location_STACK) {
               NotImplemented("Taking the address of something not on the stack");
            }

            reg_var->c = (Ctype){ .type = Type_POINTER, .pointer.pointee = &pointee->c };

            if (target == Target_ACCUM) {
               RegVar* accum = m->accum(m, Type_INT, 64);
               reg_var->location = accum->location;
            }
            else if (target == Target_STACK) {
               m->stackPushOffset(m, pointerSizeBits() / 8);
               // RegVar* accum = m->accum(m, Type_INT, 64);
               // reg_var->location = accum->location;
               // RegVar* helper = m->helper(m, Type_INT, 64);
               // c->m->addressOf(c->m, loc, helper->location);

               // m->stackPushReg(m, helper->location.reg);

               MachineX64* m_totally_temp = (MachineX64*)c->m;
               reg_var->location = (Location){ .type = Location_STACK, .offset = m_totally_temp->stack_offset };
            }

            m->addressOf(m, &pointee->location, reg_var->location);
         } break;
         // Logical operators
         case Ast_ADD:
         case Ast_SUB:
         case Ast_MUL:
         case Ast_DIV: {
            AstNode* left = node->as_binary_expr.left;
            AstNode* right = node->as_binary_expr.right;
            emitArithBinaryExpr(c, node->type, reg_var, left, right, target);
         } break;

         case Ast_LESS:
         case Ast_LEQ:
         case Ast_GREATER:
         case Ast_GEQ:
         case Ast_NOT_EQUALS:
         case Ast_EQUALS: {
            AstNode* left = node->as_binary_expr.left;
            AstNode* right = node->as_binary_expr.right;
            RegVar left_type = {0};
            RegVar right_type = {0};
            codegenEmit(c, right, &right_type, Target_STACK);
            codegenEmit(c, left, &left_type, Target_ACCUM);

            Ctype target_type = arithmeticTypeConversion(left_type.c, right_type.c);

            RegVar* helper = m->helperC(m, right_type.c);
            m->stackPop(m, helper);

            if (maybeEmitTypeConversion(c,
                                       helper,
                                       target_type,
                                       Target_STACK,
                                       "Incompatible types in binary expression")) {
               helper = m->helperC(m, target_type);
               m->stackPop(m, helper);
            }

            maybeEmitTypeConversion(c,
                                    &left_type,
                                    target_type,
                                    Target_ACCUM,
                                    "Incompatible types in binary expression");

            RegVar* accum = m->accumC(m, target_type);
            m->cmp(m, accum, helper);

            reg_var->c = (Ctype) { .type = Type_INT };

            if (target == Target_ACCUM) {
               accum = m->accum(m, Type_INT, 32);
               m->cmpSet(m, node->type, accum->location);
               reg_var->location = accum->location;
            }
            else if (target == Target_STACK) {
               RegVar* helper = m->helper(m, Type_INT, 32);
               m->cmpSet(m, node->type, helper->location);
               reg_var->location = m->stackPushReg(m, helper->location.reg);
            }

         } break;
         // Logical AND/OR
         case Ast_LOGICAL_OR:
         case Ast_LOGICAL_AND: {
            AstNode* left = node->as_binary_expr.left;
            AstNode* right = node->as_binary_expr.right;
            RegVar left_type = {0};
            RegVar right_type = {0};

            pushInstructionOutput(c, InstrOutput_DISABLED); {
               // Grab type
               codegenEmit(c, left, &left_type, Target_NONE);
               codegenEmit(c, right, &right_type, Target_NONE);
            }
            popInstructionOutput(c);

            if (!isScalarType(left_type.c) || !isScalarType(right_type.c)) {
               char andOr[8] = Zero; {
                  snprintf(andOr, ArrayCount(andOr), "%s", node->type == Ast_LOGICAL_AND ? "AND" : "OR");
               }

               codegenError("Logical %s operation on non-scalar type.", andOr);
            }
            else {
               if (node->type == Ast_LOGICAL_AND) {
                  codegenEmit(c, left, &left_type, Target_ACCUM);
                  u64 bits = typeBits(&left_type.c);

                  // TODO: special case for comparisons
                  /*
                  if (left->type == Ast_EQUALS ||
                      left->type == Ast_LESS ||
                      left->type == Ast_LEQ ||
                      left->type == Ast_GREATER ||
                      left->type == Ast_GEQ ||
                      left->type == Ast_NOT_EQUALS) {
                     // We just emitted a compare.
                  }
                  else
                     */
                  {
                     RegVar* accum = m->accum(m, left_type.c.type, bits);
                     RegVar imm = {
                        .c = (Ctype) { .type = Type_LONG },
                        .location = (Location) { .type = Location_IMMEDIATE, .immediate_value = 0 },
                     };
                     m->cmp(m, accum, &imm);
                  }

                  char end_label[LabelMax] = Zero; {
                     static int count = 0;
                     snprintf(end_label, ArrayCount(end_label), ".cmp_end%d", count);
                  }

                  m->cmpJmp(m, Ast_EQUALS, end_label);

                  codegenEmit(c, right, &right_type, Target_ACCUM);
                  {
                     RegVar* accum = m->accum(m, right_type.c.type, typeBits(&right_type.c));
                     RegVar imm = {
                        .c = (Ctype) { .type = Type_LONG },
                        .location = (Location) { .type = Location_IMMEDIATE, .immediate_value = 0 },
                     };
                     m->cmp(m, accum, &imm);
                  }

                  m->label(m, end_label);

                  *reg_var = (RegVar){ .c = (Ctype) { .type = Type_INT } };

                  if (target == Target_ACCUM) {
                     Location loc = m->accum(m, Type_INT, 32)->location;
                     m->cmpSet(m, Ast_NOT_EQUALS, loc);
                     reg_var->location = loc;
                  }
                  else if (target == Target_STACK) {
                     Location loc = m->helper(m, Type_INT, 32)->location;
                     m->stackPushReg(m, loc.reg);
                  }
               }
               else {
                  NotImplemented("logical or");
               }
            }
         } break;
         // Binary operators
         default: {
            NotImplemented("Missing codegen for expression AST node.");
         }
      }
   }
   else {
      InvalidCodePath;
   }
}

void
emitConditionalJump(Codegen* c, AstNode* cond, char* then, char* els) {
   RegVar reg_var = {0};
   // codegenEmit(c, cond, &reg_var, Target_ACCUM);
   switch (cond->type) {
      case Ast_LESS:
      case Ast_LEQ:
      case Ast_GREATER:
      case Ast_GEQ:
      case Ast_NOT_EQUALS:
      case Ast_EQUALS: {
         AstNode* left = cond->as_binary_expr.left;
         AstNode* right = cond->as_binary_expr.right;
         RegVar left_type = {0};
         RegVar right_type = {0};
         codegenEmit(c, right, &right_type, Target_STACK);
         codegenEmit(c, left, &left_type, Target_ACCUM);

         if (typeBits(&left_type.c) != typeBits(&right_type.c)) {
            NotImplemented("Promotion rules");
         }

         c->m->cmpJmpStackTop(c->m, cond->type, c->m->accumC(c->m, left_type.c)->location, &left_type, then, els);
      } break;
      default: {
         codegenEmit(c, cond, &reg_var, Target_ACCUM);
         c->m->testAndJump(c->m, c->m->accumC(c->m, reg_var.c)->location.reg, typeBits(&reg_var.c), then, els);
      } break;
   }
}

// forward decl
void emitCompoundStatement(Codegen* c, AstNode* compound, EmitTarget target);

void
emitDeclaration(Codegen* c, struct AstNodeDeclaration* node, EmitTarget target) {
   struct AstNodeDeclSpec* specifier = node->decl_spec;
   struct AstNodeDeclaratorList* declarators = node->declarators;
   AstNode* rhs = node->rhs;
   Machine* m = c->m;

   // TODO: multiple declarators...
   struct AstNodeDeclarator* declarator = declarators ? declarators->declarator : NULL;

   // TODO: Emit warning for empty declarations.

   u64 bits = 0;

   Ctype* type = &specifier->ctype;
   if (declarator) { type = resolveTypeAndDeclarator(c->arena, &specifier->ctype, declarator->pointer_stars); }

   // Figure out the size of the declaration from the type specifier.
   if (specifier->ctype.type != Type_AGGREGATE) {
      bits = typeBits(type);
   }
   else {
      char* tag_str = specifier->ctype.aggr.tag;
      AstNode* decls = specifier->ctype.aggr.decls;
      // TODO: Anonymous structs

      if (tag_str && decls) {
         Assert(typeBits(&specifier->ctype) != 0);
         bits = typeBits(&specifier->ctype);

         if (findTag(c->scope, tag_str)) {
            codegenError("Struct identifier redeclared: %s");
         }

         Tag* tag = tagInsert(&c->scope->tag_table, tag_str, (Tag){0});
         tag->rvar = (RegVar) {
            .c = specifier->ctype,
            // TODO: tag table should have different entry.
            .location = { .type = Location_STACK, .offset = 0 /*struct tag does not have a place*/ },
         };
         u64 offset = 0;
         for (struct AstNodeDeclarationList* decl_list = &decls->as_declaration_list;
              decl_list;
              decl_list = decl_list->next) {
            struct AstNodeDeclaration* decl = decl_list->declaration;
            struct AstNodeDeclSpec* spec = decl->decl_spec;
            struct AstNodeDeclarator* declarator = decl->declarators->declarator;  // TODO: Multiple declarators in aggregate decl list?
            char* member_id = declarator->id;

            Assert(offset % 8 == 0);
            struct TagMember member = { member_id, spec->ctype, offset/8 };
            bufPush(tag->s_members, member);

            Ctype* ctype = resolveTypeAndDeclarator(c->arena, &spec->ctype, declarator->pointer_stars);

            offset += typeBits(ctype);
            offset = AlignPow2(offset, 8);
         }
         Assert(typeBits(&specifier->ctype) == offset);
      }
      else if (tag_str && specifier) {
         Tag* entry = findTag(c->scope, tag_str);
         if (!entry) {
            codegenError("Use of undeclared struct %s", tag_str);
         }

         bits = typeBits(&entry->rvar.c);
      }
   }

   Assert (bits != 0);

   // Declare a new symbol.
   if (declarator) {
      char* id_str = declarator->id;
      if (symGet(&c->scope->symbol_table, id_str) != NULL) {
         codegenError("Symbol redeclared in scope");
      }
      // TODO: top level declarations

      c->m->stackPushOffset(c->m, bits/8);

      MachineX64* m_totally_temp = (MachineX64*)c->m;

      RegVar* entry = symInsert(&c->scope->symbol_table,
                                  id_str,
                                  (RegVar){
                                     .c = *type,
                                     .location = { .type = Location_STACK, .offset = m_totally_temp->stack_offset },
                                  });


      if (rhs && isLiteral(rhs)) {               // Literal right-hand-side
         RegVar* imm = c->m->immediateFromToken(c->m, rhs->as_number.tok);

         c->m->mov(c->m, entry, imm);
      }
      else if (rhs && rhs->type != Ast_NONE) {    // Non-literal right-hand-side.
         RegVar rvar = Zero;
         emitExpression(c, rhs, &rvar, Target_ACCUM);
         if (maybeEmitTypeConversion(c, &rvar, entry->c, Target_ACCUM, "Attempting to assign from incompatible type.")) {
            c->m->mov(c->m, entry, m->accumC(m, entry->c));
         }
         else {
            c->m->mov(c->m, entry, &rvar);
         }

      }
      else {
         // TODO: scc initializes to zero by default.
      }
   }
}

void
emitStatement(Codegen* c, AstNode* stmt, EmitTarget target) {
   Machine* m = c->m;
   switch (stmt->type) {
      case Ast_COMPOUND_STMT : {
         emitCompoundStatement(c, stmt, target);
      } break;
      case Ast_RETURN: {
         // Emit code for the expression and move it to rax.

         if (stmt->as_single_expr.expr) {
            RegVar et = {0};
            emitExpression(c, stmt->as_single_expr.expr, &et, Target_ACCUM);
            Ctype ret_type = *c->current_function->as_funcdef.ctype.func.return_type;

            maybeEmitTypeConversion(c,
                                    &et,
                                    ret_type,
                                    Target_ACCUM,
                                    "Trying to return from a function with a non-compatible type.");

            m->jmp(m, ".func_end");
         }
      } break;
      case Ast_DECLARATION: {
         emitDeclaration(c, &stmt->as_declaration, target);
      } break;
      case Ast_IF: {
         AstNode* cond = stmt->as_if.condition;
         AstNode* then = stmt->as_if.then;
         AstNode* els = stmt->as_if.else_;
         char then_label[LabelMax] = {0};
         char else_label[LabelMax] = {0};
         char end_label[LabelMax] = {0};
         snprintf(then_label, ArrayCount(then_label), ".then%d", c->scope->if_count);
         snprintf(else_label, ArrayCount(else_label), ".else%d", c->scope->if_count);
         snprintf(end_label, ArrayCount(else_label), ".end%d", c->scope->if_count++);

         emitConditionalJump(c, cond, then_label, else_label);

         c->m->label(c->m, then_label);

         RegVar et = Zero;
         if (then) {
            codegenEmit(c, then, &et, Target_NONE);
         }
         else {
            codegenError("No then after if");
         }
         m->jmp(m, end_label);
         m->label(m, else_label);
         if (els) {
            codegenEmit(c, els, &et, Target_NONE);
         }
         m->label(m, end_label);
      } break;
      case Ast_ITERATION: {
                        // TODO: put all label counts in one place.
         int loop_id = c->scope->if_count++;
         pushScope(c);
         struct AstNodeDeclaration* decl = stmt->as_iteration.declaration;
         AstNode* control = stmt->as_iteration.before_iteration;
         AstNode* after = stmt->as_iteration.after_iteration;
         AstNode* body = stmt->as_iteration.body;
         char loop_label[1024] = {0}; {
            snprintf(loop_label, sizeof(loop_label), ".loop%d", loop_id);
         }
         char body_label[1024] = {0}; {
            snprintf(body_label, sizeof(body_label), ".body%d", loop_id);
         }
         char end_label[1024] = {0}; {
            snprintf(end_label, sizeof(end_label), ".end%d", loop_id);
         }
         b32 after_is_control = (control->type == Ast_NONE);
         if (decl) { emitDeclaration(c, decl, Target_ACCUM); }

         m->label(m, loop_label);
         if (!after_is_control) {
            emitConditionalJump(c, control, body_label, end_label);
         } else {
            NotImplemented("after is control");
         }

         m->label(m, body_label);
         if (body) {
            emitStatement(c, body, Target_ACCUM);
         }
         if (after) {
            emitStatement(c, after, Target_ACCUM);
         }
         m->jmp(m, loop_label);
         m->label(m, end_label);
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
   struct AstNodeCompoundStmt* comp = &compound->as_compound_stmt;

   // Emit function call prelude. Push stack
   while (comp->stmt) {
      b32 is_last = comp->next == NULL;
      emitStatement(c, comp->stmt, is_last ? target : Target_NONE);
      comp = comp->next;
   }

   popScope(c);
}


void
emitFunctionDefinition(Codegen* c, AstNode* node, EmitTarget target) {
   Machine* m = c->m;

   struct AstNodeDeclarator* declarator  = node->as_funcdef.declarator;
   struct AstNode* compound    = node->as_funcdef.compound_stmt;

   if (declarator && compound) {
      c->current_function = node;

      char *func_name = declarator->id;

      RegVar* entry = findSymbol(c, func_name);
      if (entry) {
         codegenError("Redefining function %s", func_name);
      }
      else {
         Ctype* return_type = node->as_funcdef.ctype.func.return_type;

         Ctype* s_params = node->as_funcdef.ctype.func.s_params;

         // TODO: function ctype. grab pointer from declarator and type from specifier
         symInsert(&c->scope->symbol_table,
                   func_name,
                   (RegVar) {
                      .c = (Ctype) {
                        .type = Type_FUNC,
                        .func.return_type = return_type,
                        .func.s_params = s_params,
                      },
                      .location = { .type = Location_IMMEDIATE, .offset = 0 }, // TODO: location for functions
                   });
      }


      // Push
      pushScope(c);

      c->m->functionPrelude(c->m, func_name);

      m->beginFuncParams(m);

      struct AstNodeParameter* param = declarator->params ? &declarator->params->as_parameter : NULL;
      while (param) {
         AstNode* param_type_spec = param->decl_specifier;
         struct AstNodeDeclarator* param_declarator = param->declarator;
         char* id_str = param_declarator->id;

         Assert (param_type_spec && param_type_spec->type == Ast_DECLARATION_SPECIFIER);

         Ctype param_type;
         if (param_declarator->pointer_stars) {
            param_type.type = Type_POINTER;
            param_type.pointer.pointee = AllocType(c->arena, RegVar);
            param_type.pointer.pointee = &param_type_spec->as_decl_specifier.ctype;
         }
         else {
            param_type = param_type_spec->as_decl_specifier.ctype;
         }

         Location param_loc = m->popParameter(m, c->scope, &param_type);

         Location stack_loc = Zero;
         if (param_loc.type != Location_STACK) {
            // In order for parameters to be l-values, they must be on the stack atm.
             stack_loc = m->stackPush(m, param_loc);
         }
         else {
            stack_loc = param_loc;
         }

         symInsert(&c->scope->symbol_table,
                                     id_str,
                                     (RegVar){
                                        .c = param_type,
                                        .location = stack_loc,
                                     });


         param = param->next;
      }
      m->endFuncParams(m);


      emitCompoundStatement(c, compound, Target_ACCUM);

      c->m->functionEpilogue(c->m);

      popScope(c);

      c->current_function = NULL;
      // TODO: Remove reference to MachineX64
      Assert (((MachineX64*)c->m)->stack_offset == 0);
   }
   else {
      codegenError("Funcdef: Invalid node in the tree.");
   }
}


void
codegenEmit(Codegen* c, AstNode* node, RegVar* reg_var, EmitTarget target) {
   if (node->type == Ast_FUNCDEF) {
      emitFunctionDefinition(c, node, target);
   }
   else if (nodeIsExpression(node)) {
      if (reg_var == NULL) {
         codegenError("reg_var is NULL when generating code for expression.");
      }
      emitExpression(c, node, reg_var, target);
   }
   else if (node->type == Ast_COMPOUND_STMT) {
      emitCompoundStatement(c, node, target);
   }
   else {
      emitStatement(c, node, target);
   }
}

void
codegenTranslationUnit(Codegen* c, AstNode* root) {
   pushScope(c);

   // Machine* m = c->m;
   {
      HMStaticArraysKeyIter iter;
      staticArrKeyIterBegin(c->hm_static_arrays, &iter);
   }


   // Reserve static buffers.
   //for (sz i = 0; i < bufCount(c->s_static_buffers); ++i) {
      // StaticBuffer* sb = &c->s_static_buffers[i];
      // switch (sb->type) {
      //    case SBType_STRING: {
      //       m->reserveStaticString(m, sb->tok->cast.string);
      //    } break;
      //    case SBType_ARRAY: {
      //       NotImplemented("Static array");
      //    } break;
      //    default: { InvalidCodePath; }
      // }
   // }


   // Emit code

   struct AstNodeTU* tu = &root->as_tu;
   while (tu->node) {
      if (tu->node->type == Ast_FUNCDEF) {
         codegenEmit(c, tu->node, NULL, Target_NONE);
      }
      else if (tu->node->type == Ast_DECLARATION) {
         codegenEmit(c, tu->node, NULL, Target_NONE);
      }
      else {
         NotImplemented ("Top level declarations.");
      }

      tu = tu->next;
   }
   popScope(c);
   c->m->finish(c->m);
}

