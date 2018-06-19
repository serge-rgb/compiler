
// TODO:
//
// In order to support floating point, we're going to have to abstract our
// instruction-emitting code.
//
// Currently, we mostly printf whatever instruction we need, but now that we
// are going to introduce different kinds of data we might find it useful to
// introduce a "typed register".
//
// With our current stack-machine model, we only need two typed registers, but
// later iterations of the code generator can probably build on this
// abstraction.
//
// In the typed register model, a "mov(A, B)" abstraction would result in (for instance)
// a conversion from float to int or vice versa, or simply an x86 mov, or a
// wide SSE2 instruction for wide types. It will be useful to dispatch the
// appropriate instruction from the abstraction function, instead of sprinkling
// switch statements all over the code.
//
// This means that before going into implementing floats, we must first
// abstract away our current code, moving all printf-style instructions to
// their abstracted counterparts.
//
//
//

void
codegenInit(Codegen* c, char* outfile, MachineConfigFlags mflags) {
   char asmfile [PathMax] = Zero;
   snprintf(asmfile, PathMax, "%s.asm", outfile);
   g_asm = fopen(asmfile, "w");

   c->m = AllocType(c->arena, Machine);
   c->m->config = mflags;
   machInit(c->m);

   // Constants
   c->one = makeAstNode(c->arena, Ast_NUMBER, 0,0);
   Token* one_tok = AllocType(c->arena, Token);
   one_tok->value = 1;
   c->one->tok = one_tok;
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

                  DevBreak("keep going here");

                  machMovAccum(c->m, expr_type, node->tok);

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

         if (typeBits(&left_type.c) != typeBits(&right_type.c)) {
            NotImplemented("Promotion rules");
         }

         machCmpJmp(c->m, cond->type, typeBits(&left_type.c), then, els);
      } break;
      default: {
         codegenEmit(c, cond, &expr_type, Target_ACCUM);
         machTestAndJump(c->m, typeBits(&expr_type.c), then, els);
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
                                     .location = { .type = Location_STACK, .offset = c->m->stack_offset },
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
         machMovStackTop(c->m, entry, rhs->tok);
      }
      else if (rhs->type != Ast_NONE) {    // Non-literal right-hand-side.
         ExprType type = Zero;
         emitExpression(c, rhs, &type, Target_ACCUM);
         movOrCopy(c->m, entry->location, type.location, typeBits(&type.c));
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
            machJumpToLabel(".func_end");
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

         machLabel(then_label);

         ExprType et = Zero;
         if (then) {
            codegenEmit(c, then, &et, Target_NONE);
         }
         else {
            codegenError("No then after if");
         }
         machJumpToLabel(end_label);
         machLabel(else_label);
         if (els) {
            codegenEmit(c, els, &et, Target_NONE);
         }
         machLabel(end_label);
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

         machLabel(loop_label);
         if (!after_is_control) {
            emitConditionalJump(c, control, body_label, end_label);
         }

         machLabel(body_label);
         if (body->type != Ast_NONE) {
            emitStatement(c, body, Target_ACCUM);
         }
         if (after->type != Ast_NONE) {
            emitStatement(c, after, Target_ACCUM);
         }
         machJumpToLabel(loop_label);
         machLabel(end_label);
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
         // TODO: function ctype. grab pointer from declarator and type from specifier
         symInsert(&c->scope->symbol_table,
                   func_name,
                   (ExprType) {
                      .c = (Ctype) { .type = Type_FUNC, .func.node = node },
                      .location = { .type = Location_IMMEDIATE, .offset = 0 }, // TODO: location for functions
                   });
      }

      machFunctionPrelude(c->m, func_name);

      // Push
      pushScope(c);

      AstNode* params = declarator->child->next;
      if (params) {
         AstNode* p = params;
         u64 n_param = 0;
         u64 offset = 16;  // TODO: get rid of this variable. probably after popParameter is abstracted.
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

            Location param_loc = popParameter(c, &param_type, n_param++, &offset);

            symInsert(&c->scope->symbol_table,
                                        id_str,
                                        (ExprType){
                                           .c = param_type,
                                           .location = param_loc,
                                        });


            p = p->next;
         }
      }

      emitCompoundStatement(c, compound, Target_ACCUM);

      machFunctionEpilogue(c->m);

      popScope(c);
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
      emitStatement(c, node, Target_TMP);
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

