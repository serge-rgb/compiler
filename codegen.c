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
   m->stackPop(m, Call(m, helper, Type_INT, bits));

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

   switch (type) {
      case Ast_ADD: { m->add(m, Call(m, accum, tleft.c.type, bits), Call(m, helper, tright.c.type, bits)); } break;
      case Ast_SUB: { m->sub(m, Call(m, accum, tleft.c.type, bits), Call(m, helper, tright.c.type, bits)); } break;
      case Ast_MUL: { m->mul(m, Call(m, accum, tleft.c.type, bits), Call(m, helper, tright.c.type, bits)); } break;
      case Ast_DIV: { m->div(m, Call(m, accum, tleft.c.type, bits), Call(m, helper, tright.c.type, bits)); } break;
      default: break;
   }

   if (target == Target_STACK) {
      Call(m, stackPushReg, Call(m, accum, tleft.c.type, bits)->location.reg);
   }
}

void
emitIdentifier(Codegen*c, AstNode* node, ExprType* expr_type, EmitTarget target) {
   Machine* m = c->m;
   char* id_str = node->tok->cast.string;
   ExprType* entry = findSymbol(c, id_str);

   if (!entry) {
      codegenError("Use of undeclared identifier %s", node->tok->cast.string);
   }

   Location loc = entry->location;

   if (typeBits(&entry->c) > 64) {
      if (target != Target_NONE) {
         m->stackAddressInAccum(m, entry);
         if (target == Target_STACK) {
            Call(m, stackPushReg, Call(m, accumC, expr_type->c)->location.reg);
         }
      }

   }
   else {
      if (target != Target_NONE) {
         ExprType reg = {
            .c = entry->c,
            .location = registerLocation(Reg_RAX)
         };
         m->mov(m, &reg, entry);

         if (target == Target_STACK) {
            Call(m, stackPushReg, Reg_RAX);
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
   struct StructMember* s_members = struct_entry->c.aggr.s_members;

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

   struct StructMember* member = s_members + member_idx;
   u64 member_offset = member->offset;

   if (address.type == Location_STACK) {
      address.offset = symbol_entry->location.offset - member_offset;

      if (target != Target_NONE) {
         ExprType reg = {
            .c = *member->ctype,
            .location = address,
         };
         ExprType* accum = Call(m, accumC, *member->ctype);
         m->mov(m, accum, &reg);

         if (target == Target_STACK) {
            Call(m, stackPushReg, accum->location.reg);
         }
      }
   }
   else if (address.type == Location_REGISTER) {
      if (target != Target_NONE) {
         ExprType reg = {
            .c = *member->ctype,
            .location = (Location){
               .type = Location_STACK_FROM_REG,
               .reg = address.reg,
               .reg_offset = member_offset,
            },
         };
         ExprType* accum = Call(m, accumC, *member->ctype);


         m->mov(m, accum, &reg);
         if (target == Target_STACK) {
            Call(m, stackPushReg, address.reg);
         }
      }
   }
   if (expr_type) {
      expr_type->c = *(member->ctype);
      expr_type->location = address;
   }
}

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
                     u64 n_a = bufCount(aggr_a->c.aggr.s_members);
                     u64 n_b = bufCount(aggr_b->c.aggr.s_members);
                     if (n_a == n_b) {
                        compatible = true;
                        for (u64 i = 0; i < n_a; ++i) {
                           // TODO: Alignment check.
                           if (!typesAreCompatible(c,
                                                   *aggr_a->c.aggr.s_members[i].ctype,
                                                   *aggr_b->c.aggr.s_members[i].ctype)) {
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

      m->pushParameter(m, n_param++, &et);
      expected_param = expected_param->next;
   }

   Call(c->m, call, label);

   while (bufCount(m_totally_temp->s_stack) != stack_top) {
      ExprType* helper = Call(m, helper, Type_INT, 64);
      Call(m, stackPop, helper);
   }
   // TODO: Restore registers. Not necessary at the moment because of DDCG

   Ctype return_type = funcReturnType(sym->c.func.node);
   if (target == Target_STACK) {
      Call(m, stackPushReg, Call(m, accumC, return_type)->location.reg);
   }

   AstNode* funcdef = sym->c.func.node;
   Assert(funcdef->type == Ast_FUNCDEF);

   expr_type->c = funcdef->child->ctype;
   expr_type->location = Call(m, accumC, return_type)->location;
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
                  Call(m, movAccum, expr_type, node->tok);
               } break;
               case Target_STACK: {
                  Call(m, stackPushImm, expr_type, node->tok->value);
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

               ExprType* helper = Call(m, helper, lhs_type.c.type, 32);
               m->mov(m, helper, &lhs_type);

               switch (op->value) {
                  case ASSIGN_INCREMENT: {
                     Call(m, add, helper, Call(m, accum, rhs_type.c.type, 32));
                  } break;
                  default: {
                     NotImplemented("Different assignment expressions");
                  }
               }

               m->mov(m, &lhs_type, helper);

               if (target == Target_ACCUM) {
                  m->mov(c->m, Call(m, accumC, lhs_type.c), &lhs_type);
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
            }

            ExprType* accum = Call(m, accumC, local_etype.c);
            m->mov(c->m, &local_etype, accum);

            if (target == Target_STACK) {
               // Result is already on the stack.
            }
            else if (target == Target_ACCUM) {
               // Return old value.
               Call(m, stackPop, accum);
            }
            else if (target == Target_NONE) {
               Call(m, stackPop, Call(m, helper, Type_INT, 64));
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
               Call(c->m, addressOf, loc);
               ExprType* accum = Call(m, accum, Type_INT, 64);
               if (target == Target_STACK) {
                  Call(m, stackPushReg, accum->location.reg);

                  MachineX64* m_totally_temp = (MachineX64*)c->m;
                  result.location = (Location){ .type = Location_STACK, .offset = m_totally_temp->stack_offset };
               }
               else {
                 result.location = accum->location;
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

               ExprType* accum = Call(m, accumC, left_type.c);
               ExprType* helper = Call(m, helperC, right_type.c);

               Call(m, stackPop, helper);
               Call(m, cmp, accum, helper);
               Call(m, cmpSetAccum, node->type);

               if (target == Target_STACK) {
                  Call(m, stackPushReg, accum->location.reg);
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

         if (typeBits(&left_type.c) != typeBits(&right_type.c)) {
            NotImplemented("Promotion rules");
         }

         Call(c->m, cmpJmp, cond->type, &left_type, then, els);
      } break;
      default: {
         codegenEmit(c, cond, &expr_type, Target_ACCUM);
         Call(c->m, testAndJump, typeBits(&expr_type.c), then, els);
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

         // TODO: Parameter passing is tied to ABI. Move to machine abstraction

         u64 offset = 0;
         for (AstNode* decl = decls;
              decl;
              decl = decl->next) {
            AstNode* spec = decl->child;
            AstNode* declarator = spec->next;
            char* member_id = declarator->child->tok->cast.string;

            Assert(offset % 8 == 0);
            struct StructMember member = { member_id, &spec->ctype, offset/8 };
            bufPush(entry.c.aggr.s_members, member);

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

      Call(c->m, stackPushOffset, bits/8);

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
         ExprType* imm = Call(c->m, immediateFromToken, rhs->tok);

         Call(c->m, mov, entry, imm);
      }
      else if (rhs->type != Ast_NONE) {    // Non-literal right-hand-side.
         ExprType type = Zero;
         emitExpression(c, rhs, &type, Target_ACCUM);
         Call(c->m, mov, entry, &type);
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
            Call(m, jmp, ".func_end");
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

         Call(c->m, label, then_label);

         ExprType et = Zero;
         if (then) {
            codegenEmit(c, then, &et, Target_NONE);
         }
         else {
            codegenError("No then after if");
         }
         Call(m, jmp, end_label);
         Call(m, label, else_label);
         if (els) {
            codegenEmit(c, els, &et, Target_NONE);
         }
         Call(m, label, end_label);
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

         Call(m, label, loop_label);
         if (!after_is_control) {
            emitConditionalJump(c, control, body_label, end_label);
         } else {
            NotImplemented("after is control");
         }

         Call(m, label, body_label);
         if (body->type != Ast_NONE) {
            emitStatement(c, body, Target_ACCUM);
         }
         if (after->type != Ast_NONE) {
            emitStatement(c, after, Target_ACCUM);
         }
         Call(m, jmp, loop_label);
         Call(m, label, end_label);
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

      AstNode* params = declarator->child->next;
      if (params) {
         AstNode* p = params;
         u64 n_param = 0;
         u64 offset = 0;
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

            Location param_loc = Call(c->m, popParameter, &param_type, n_param++, &offset);

            symInsert(&c->scope->symbol_table,
                                        id_str,
                                        (ExprType){
                                           .c = param_type,
                                           .location = param_loc,
                                        });


            p = p->next;
         }
      }

      // NOTE: The prelude goes after popping parameters, since it's easier to
      // handle them when we know they are at the top of the stack.
      Call(c->m, functionPrelude, func_name);

      emitCompoundStatement(c, compound, Target_ACCUM);

      Call(c->m, functionEpilogue);

      popScope(c);

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
   Call(c->m, finish);
}

