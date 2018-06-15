
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
// NOTES:
//
// emit function prelude -> instructionPrintf -> line number coupling

void
codegenInit(Codegen* c, char* outfile) {
   char asmfile [PathMax] = Zero;
   snprintf(asmfile, PathMax, "%s.asm", outfile);
   g_asm = fopen(asmfile, "w");

   machineInit(c);

   // Constants
   c->one = makeAstNode(c->arena, Ast_NUMBER, 0,0);
   Token* one_tok = AllocType(c->arena, Token);
   one_tok->value = 1;
   c->one->tok = one_tok;
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


      machFunctionPrelude(func_name);
      // TODO: Keep going here. Move stack offset to machine model.
      c->stack_offset += 8;

      // Helper when running in a debugger. Break on function entry.
      // instructionPrintf(0, "int 3");

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

      //i64 stack = c->scope->stack_size;

      // TODO: Align the stack again.
      // stack = AlignPow2(stack, 16);
      // TODO: On mac OS, the stack needs to be aligned to 32 or 64 byte boundaries when m256 or m512 values are passed on the stack.

      instructionPrintf(".func_end:");

      while (bufCount(c->stack) > 0)  {
         stackPop(c, Reg_RBX);
      }

      popScope(c);

      // Restore non-volatile registers.

      instructionPrintf("pop rbp");
      instructionPrintf("ret");
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

