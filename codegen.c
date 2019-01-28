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
   c->one = makeAstNode(c->arena, Ast_NUMBER, 0,0);
   Token* one_tok = AllocType(c->arena, Token);
   one_tok->value = 1;
   c->one->tok = one_tok;

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

   int bits = typeBits(&tleft.c);
   m->stackPop(m, m->helper(m, Type_INT, bits));

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

   ExprType* dst = m->accum(m, tleft.c.type, bits);
   ExprType* src = m->helper(m, tright.c.type, bits);
   switch (type) {
      case Ast_ADD: { m->add(m, dst, src); } break;
      case Ast_SUB: { m->sub(m, dst, src); } break;
      case Ast_MUL: { m->mul(m, dst, src); } break;
      case Ast_DIV: { m->div(m, dst, src); } break;
      default: break;
   }

   if (target == Target_STACK) {
      m->stackPushReg(m, m->accum(m, tleft.c.type, bits)->location.reg);
   }
}

void
emitIdentifier(Codegen*c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   Assert(expr_type);

   Machine* m = c->m;
   char* id_str = node->tok->cast.string;
   ExprType* entry = findSymbol(c, id_str);

   expr_type->c = entry->c;
   expr_type->location = entry->location;

   if (!entry) {
      codegenError("Use of undeclared identifier %s", node->tok->cast.string);
   }

   if (typeBits(&entry->c) > 64) {
      if (target != Target_NONE) {
         m->stackAddressInAccum(m, entry);
         if (target == Target_STACK) {
            ExprType* accum = m->accumC(m, expr_type->c);
            m->stackPushReg(m,
                            accum->location.reg);
            expr_type->location = accum->location;
         }
      }

   }
   else {
      if (target != Target_NONE) {
         ExprType* accum = m->accumC(m, entry->c);
         m->mov(m, accum, entry);

         if (target == Target_STACK) {
            Location loc = m->stackPushReg(m, Reg_RAX);
            expr_type->location = loc;
         }
         else if (target == Target_ACCUM) {
            expr_type->location = accum->location;
         }
      }
   }
}

b32
typesAreCompatible(Codegen* c, Ctype into, Ctype from) {
   b32 compatible = false;
   if (into.type == from.type) {
      if (into.type == Type_POINTER) {
         compatible = typesAreCompatible(c,
                                         into.pointer.pointee->c,
                                         from.pointer.pointee->c);
      }
      else {
         switch (into.type) {
            case Type_AGGREGATE: {
               // TODO: Anonymous structs
               if ((into.aggr.tag && from.aggr.tag)) {
                  Tag* tag_a = findTag(c->scope, into.aggr.tag);
                  Tag* tag_b = findTag(c->scope, from.aggr.tag);
                  // TODO: We could return here by just having the same tag. C
                  // spec says (6.7.2) that when two types have the same tag,
                  // defined in different translation units, they must have:
                  //    - The same number of parameters.
                  //    - Every corresponding parameter is compatible.
                  if (tag_a == tag_b) {
                     compatible = true;
                  }
                  // SPEC: We deviate from the spec by always considering two
                  // structs with the same layout as compatible.
                  else {
                     u64 n_a = bufCount(tag_a->s_members);
                     u64 n_b = bufCount(tag_b->s_members);
                     if (n_a == n_b) {
                        compatible = true;
                        for (u64 i = 0; i < n_a; ++i) {
                           // TODO: Alignment check.
                           if (!typesAreCompatible(c,
                                                   tag_a->s_members[i].ctype,
                                                   tag_b->s_members[i].ctype)) {
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
      if (into.type == Type_POINTER || from.type == Type_POINTER) {
         compatible = false;
      }
      else {
         if ((isIntegerType(&into) || isRealType(&into)) &&
             (isIntegerType(&from) || isRealType(&from))) {
            compatible = true;
         }
         else {
            NotImplemented("Compatibility rules");
         }
      }
   }
   return compatible;
}

b32
maybeEmitTypeConversion(Codegen* c, ExprType* value, Ctype target_type, EmitTarget target, char* incompatibleError) {
   b32 didConversion = true;
   if (!typesAreCompatible(c, target_type, value->c)) {
      codegenError(incompatibleError);
   }

   if (value->c.type == target_type.type
       || (isIntegerType(&value->c) && isIntegerType(&target_type))) {
      // Nothing to do
      didConversion = false;
   }
   else if (value->c.type == Type_FLOAT &&
            target_type.type == Type_INT) {
      if (target != Target_NONE) {
         c->m->convertFloatToInt(c->m, value->location);
         if (target == Target_STACK) {
            c->m->stackPushReg(c->m, c->m->accum(c->m, target_type.type, typeBits(&target_type))->location.reg);
         }
      }
   }
   else {
      NotImplemented("type conversion.");
   }
   return didConversion;
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

      if (target != Target_NONE) {
         ExprType reg = {
            .c = member->ctype,
            .location = address,
         };
         ExprType* accum = m->accumC(m, member->ctype);
         m->mov(m, accum, &reg);

         if (target == Target_STACK) {
            m->stackPushReg(m, accum->location.reg);
         }
      }
   }
   else if (address.type == Location_REGISTER) {
      if (target != Target_NONE) {
         ExprType reg = {
            .c = member->ctype,
            .location = (Location){
               .type = Location_STACK_FROM_REG,
               .reg = address.reg,
               .reg_offset = member_offset,
            },
         };
         ExprType* accum = m->accumC(m, member->ctype);

         m->mov(m, accum, &reg);
         if (target == Target_STACK) {
            m->stackPushReg(m, address.reg);
         }
      }
   }
   if (expr_type) {
      expr_type->c = member->ctype;
      expr_type->location = address;
   }
}

void emitExpression(Codegen* c, AstNode* node, ExprType* expr_type, EmitTarget target); // Forward decl.

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

   // TODO: Remove reference to MachineX64
   MachineX64* m_totally_temp = (MachineX64*)c->m;
   u64 stack_top = bufCount(m_totally_temp->s_stack);

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
   AstNode* expected_param = funcParams(sym->c.func.node);

   m->beginFuncParams(m);
   {
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

         m->pushParameter(m, c->scope, &et);
         expected_param = expected_param->next;
      }
   }
   m->endFuncParams(m);

   m->call(m, label);

   while (bufCount(m_totally_temp->s_stack) != stack_top) {
      ExprType* helper = m->helper(m, Type_INT, 64);
      m->stackPop(m, helper);
   }
   // TODO: Restore registers. Not necessary at the moment because of DDCG

   Ctype return_type = funcReturnType(sym->c.func.node);
   if (target == Target_STACK) {
      m->stackPushReg(m, m->accumC(m, return_type)->location.reg);
   }

   AstNode* funcdef = sym->c.func.node;
   Assert(funcdef->type == Ast_FUNCDEF);

   expr_type->c = funcdef->child->ctype;
   expr_type->location = m->accumC(m, return_type)->location;
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
            switch (target) {
               case Target_ACCUM: {
                  // TODO: Literals with specific sizes.
                  // TODO: Literals with non-int types.
                  expr_type->c = (Ctype) {
                     .type = Type_INT,
                  };
                  m->movAccum(m, expr_type, node->tok);
               } break;
               case Target_STACK: {
                  m->stackPushImm(m, expr_type, node->tok->value);
               } break;
               case Target_NONE: {
                  expr_type->location = (Location) {
                     .type = Location_IMMEDIATE,
                     .immediate_value = node->tok->value,
                  };
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
               m->mov(c->m, &lhs_type, &rhs_type);
            }
            else {
               Assert(bits < 64);
               // TODO: Check for arithmetic type here.

               ExprType* helper = m->helper(m, lhs_type.c.type, 32);
               m->mov(m, helper, &lhs_type);

               switch (op->value) {
                  case ASSIGN_INCREMENT: {
                     m->add(m, helper, m->accum(m, rhs_type.c.type, 32));
                  } break;
                  default: {
                     NotImplemented("Different assignment expressions");
                  }
               }

               m->mov(m, &lhs_type, helper);

               if (target == Target_ACCUM) {
                  m->mov(c->m, m->accumC(m, lhs_type.c), &lhs_type);
               }
            }
         } break;
         case Ast_POSTFIX_INC:
         case Ast_POSTFIX_DEC: {
            AstNode* expr = node->child;
            ExprType local_etype = Zero;
            emitExpression(c, expr, &local_etype, Target_STACK);
            if (local_etype.location.type == Location_IMMEDIATE) {
               codegenError("Attempting to increment an rvalue.");
            }

            switch (node->type) {
               case Ast_POSTFIX_INC: { emitArithBinaryExpr(c, Ast_ADD, NULL, expr, c->one, Target_ACCUM); } break;
               case Ast_POSTFIX_DEC: { emitArithBinaryExpr(c, Ast_SUB, NULL, expr, c->one, Target_ACCUM); } break;
               default: { InvalidCodePath; }
            }

            ExprType* accum = m->accumC(m, local_etype.c);
            m->mov(c->m, &local_etype, accum);

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
               c->m->addressOf(c->m, loc);
               ExprType* accum = m->accum(m, Type_INT, 64);
               if (target == Target_STACK) {
                  m->stackPushReg(m, accum->location.reg);

                  MachineX64* m_totally_temp = (MachineX64*)c->m;
                  result.location = (Location){ .type = Location_STACK, .offset = m_totally_temp->stack_offset };
               }
               else {
                 result.location = accum->location;
               }
               *expr_type = result;
            }
         } break;
         // Logical operators
         case Ast_ADD:
         case Ast_SUB:
         case Ast_MUL:
         case Ast_DIV: {
            AstNode* child1 = child0->next;
            emitArithBinaryExpr(c, node->type, expr_type, child0, child1, target);
         } break;

         case Ast_LESS:
         case Ast_LEQ:
         case Ast_GREATER:
         case Ast_GEQ:
         case Ast_NOT_EQUALS:
         case Ast_EQUALS: {
            AstNode* left = node->child;
            AstNode* right = node->child->next;
            ExprType left_type = {0};
            ExprType right_type = {0};
            codegenEmit(c, right, &right_type, Target_STACK);
            codegenEmit(c, left, &left_type, Target_ACCUM);

            Ctype target_type = arithmeticTypeConversion(left_type.c, right_type.c);

            ExprType* helper = m->helperC(m, right_type.c);
            m->stackPop(m, helper);

            if (maybeEmitTypeConversion(c,
                                       &right_type,
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

            ExprType* accum = m->accumC(m, target_type);

            m->cmp(m, accum, helper);
            m->cmpSetAccum(m, node->type);

            expr_type->c = target_type;

            if (target == Target_ACCUM) {
               expr_type->location = accum->location;
            }
            else if (target == Target_STACK) {
               expr_type->location = m->stackPushReg(m, accum->location.reg);
            }

         } break;
         // Logical AND/OR
         case Ast_LOGICAL_OR:
         case Ast_LOGICAL_AND: {
            AstNode* left = node->child;
            AstNode* right = node->child->next;
            ExprType left_type = {0};
            ExprType right_type = {0};

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

                  // TODO: special case for comparissons
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
                     ExprType* accum = m->accum(m, left_type.c.type, bits);
                     ExprType imm = {
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
                     ExprType* accum = m->accum(m, right_type.c.type, typeBits(&right_type.c));
                     ExprType imm = {
                        .c = (Ctype) { .type = Type_LONG },
                        .location = (Location) { .type = Location_IMMEDIATE, .immediate_value = 0 },
                     };
                     m->cmp(m, accum, &imm);
                  }

                  m->label(m, end_label);


                  *expr_type = (ExprType){ .c = (Ctype) { .type = Type_INT } };

                  if (target != Target_NONE) {
                     Location loc = m->accum(m, Type_INT, 32)->location;
                     if (target == Target_ACCUM) {
                        m->cmpSetAccum(m, Ast_NOT_EQUALS);
                        expr_type->location = loc;
                     }
                     else if (target == Target_STACK) {
                        m->stackPushReg(m, loc.reg);
                     }
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

         if (typeBits(&left_type.c) != typeBits(&right_type.c)) {
            NotImplemented("Promotion rules");
         }

         c->m->cmpJmpStackTop(c->m, cond->type, &left_type, then, els);
      } break;
      default: {
         codegenEmit(c, cond, &expr_type, Target_ACCUM);
         c->m->testAndJump(c->m, typeBits(&expr_type.c), then, els);
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

         if (findTag(c->scope, tag_str)) {
            codegenError("Struct identifier redeclared: %s");
         }

         // TODO: Parameter passing is tied to ABI. Move to machine abstraction

         Tag* tag = tagInsert(&c->scope->tag_table, tag_str, (Tag){0});
         tag->etype = (ExprType) {
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
            struct TagMember member = { member_id, spec->ctype, offset/8 };
            bufPush(tag->s_members, member);

            Ctype* ctype = NULL;
            if (declarator->is_pointer) {
               NotImplemented("struct member is pointer");
            }
            else {
               ctype = &spec->ctype;
            }

            offset += typeBits(ctype);
            offset = AlignPow2(offset, 8);
         }
         Assert(typeBits(&specifier->ctype) == offset);

      }
      else if (tag_str && declarator->type != Ast_NONE) {
         Tag* entry = findTag(c->scope, tag_str);
         if (!entry) {
            codegenError("Use of undeclared struct %s", tag_str);
         }

         if (declarator->is_pointer)
            bits = pointerSizeBits();
         else
            bits = typeBits(&entry->etype.c);
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

      c->m->stackPushOffset(c->m, bits/8);

      MachineX64* m_totally_temp = (MachineX64*)c->m;

      ExprType* entry = symInsert(&c->scope->symbol_table,
                                  id_str,
                                  (ExprType){
                                     .c = Zero,
                                     .location = { .type = Location_STACK, .offset = m_totally_temp->stack_offset },
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
         if (isRealType(&entry->c) &&
             rhs->tok->type != TType_FLOAT)  {
            Assert (rhs->tok->type == TType_NUMBER);
            rhs->tok->type = TType_FLOAT;
            rhs->tok->cast.real64 = rhs->tok->cast.int32;
         }
         ExprType* imm = c->m->immediateFromToken(c->m, rhs->tok);

         c->m->mov(c->m, entry, imm);
      }
      else if (rhs->type != Ast_NONE) {    // Non-literal right-hand-side.
         ExprType type = Zero;
         emitExpression(c, rhs, &type, Target_ACCUM);
         c->m->mov(c->m, entry, &type);
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

         if (stmt->child) {
            ExprType et = {0};
            emitExpression(c, stmt->child, &et, Target_ACCUM);
            Ctype ret_type = funcReturnType(c->current_function);

            maybeEmitTypeConversion(c,
                                    &et,
                                    ret_type,
                                    Target_ACCUM,
                                    "Trying to return from a function with a non-compatible type.");

            m->jmp(m, ".func_end");
         }
      } break;
      case Ast_DECLARATION: {
         emitDeclaration(c, stmt, target);
      } break;
      case Ast_IF: {
         AstNode* cond = stmt->child;
         AstNode* then = cond->next;
         AstNode* els = then ? then->next : NULL;
         char then_label[LabelMax] = {0};
         char else_label[LabelMax] = {0};
         char end_label[LabelMax] = {0};
         snprintf(then_label, ArrayCount(then_label), ".then%d", c->scope->if_count);
         snprintf(else_label, ArrayCount(else_label), ".else%d", c->scope->if_count);
         snprintf(end_label, ArrayCount(else_label), ".end%d", c->scope->if_count++);

         emitConditionalJump(c, cond, then_label, else_label);

         c->m->label(c->m, then_label);

         ExprType et = Zero;
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

         m->label(m, loop_label);
         if (!after_is_control) {
            emitConditionalJump(c, control, body_label, end_label);
         } else {
            NotImplemented("after is control");
         }

         m->label(m, body_label);
         if (body->type != Ast_NONE) {
            emitStatement(c, body, Target_ACCUM);
         }
         if (after->type != Ast_NONE) {
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
   Machine* m = c->m;

   AstNode* specifier   = node->child;
   AstNode* declarator  = specifier->next;
   AstNode* compound    = declarator->next;

   if (specifier && declarator && compound) {
      c->current_function = node;

      char *func_name = declarator->child->tok->cast.string;

      ExprType* entry = findSymbol(c, func_name);
      if (entry) {
         codegenError("Redefining function %s", func_name);
      }
      else {
         // TODO: function ctype. grab pointer from declarator and type from specifier
         symInsert(&c->scope->symbol_table,
                   func_name,
                   (ExprType) {
                      .c = (Ctype) { .type = Type_FUNC, .func.node = node },
                      .location = { .type = Location_IMMEDIATE, .offset = 0 }, // TODO: location for functions
                   });
      }


      // Push
      pushScope(c);

      c->m->functionPrelude(c->m, func_name);

      m->beginFuncParams(m);


      AstNode* params = declarator->child->next;
      if (params) {
         AstNode* p = params;
         while (p) {
            Assert (p->type == Ast_PARAMETER);
            AstNode* param_type_spec = p->child;
            AstNode* param_declarator = param_type_spec->next;
            char* id_str = param_declarator->child->tok->cast.string;

            Assert (param_type_spec && param_type_spec->type == Ast_DECLARATION_SPECIFIER);
            Assert (param_declarator && param_declarator->child->type == Ast_ID);

            Ctype param_type;
            if (param_declarator->is_pointer) {
               param_type.type = Type_POINTER;
               param_type.pointer.pointee = AllocType(c->arena, ExprType);
               param_type.pointer.pointee->c = param_type_spec->ctype;
            }
            else {
               param_type = param_type_spec->ctype;
            }

            Location param_loc = m->popParameter(m, c->scope, &param_type);

            symInsert(&c->scope->symbol_table,
                                        id_str,
                                        (ExprType){
                                           .c = param_type,
                                           .location = param_loc,
                                        });


            p = p->next;
         }
         m->endFuncParams(m);
      }


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
      emitStatement(c, node, target);
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
   c->m->finish(c->m);
}

