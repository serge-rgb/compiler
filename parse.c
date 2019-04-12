// A function that takes a Parser and returns an AstNode*
typedef AstNode* ParseFunction(Parser*);

void
initParser(Parser* p) {
   p->flags = ParserFlag_FANSI;

   p->hm_sizes = AllocType(p->arena, HMAggregateSizes);
   p->hm_static_arrays = AllocType(p->arena, HMStaticArrays);
}

void
noneIfNull(Arena* a, AstNode** n) {
   if (!*n) {
      *n = makeAstNode(a, Ast_NONE, NULL, NULL);
   }
}

AstNode*
newNode(Arena* a) {
   AstNode* r = AllocType(a, AstNode);
   return r;
}

AstNode*
makeBinaryExpr(Arena* a, AstType type, AstNode* left, AstNode* right) {
   AstNode* n = AllocType(a, AstNode);
   n->type = type;
   n->as_binary_expr.left = left;
   n->as_binary_expr.right = right;
   return n;
}

void
parseError(Parser* p, char* msg, ...) {
   va_list args;
   va_start(args, msg);
   char buffer[LineMax] = {0};
   vsnprintf(buffer, LineMax, msg, args);
   fprintf(stderr, "Syntax error: %s\n", buffer);

   Break;

   va_end(args);
   exit(1);
}

b32
peekPunctuator(Parser* p, int c) {
   b32 result = false;
   Assert(c < 256);
   result = p->token->type == TType_PUNCTUATOR && p->token->cast.character == c;
   return result;
}

Token*
marktrack(Parser* p) {
   // TODO: allocator should be able to rewind as well
   Token* result = p->token;
   return result;
}

void
backtrack(Parser* p, Token* t) {
   p->token = t;
}

Token*
nextToken(Parser* p) {
   Token* result = NULL;
   if (p->token) {
      result = p->token;
      p->token = p->token->next;
   }
   return result;
}

Token*
peekKeyword(Parser* p, int keyword) {
   Token* tok = p->token;
   if (tok->type == TType_KEYWORD && tok->cast.int32 == keyword) {
      return tok;
   }
   return NULL;
}

Token*
nextKeyword(Parser* p, int keyword) {
   Token* tok = nextToken(p);
   if (tok->type == TType_KEYWORD && tok->cast.int32 == keyword) {
      return tok;
   }
   else {
      backtrack(p, tok);
   }
   return NULL;
}

Token*
nextPunctuator(Parser* p, int c) {
   Token* result = NULL;
   if (peekPunctuator(p, c)) {
      result = nextToken(p);
   }
   return result;
}

void
expectPunctuator(Parser* p, int punctuator) {
   if (!nextPunctuator(p, punctuator)) {
      parseError(p, "%s: %d Expected '%c'", p->file_name, p->token->line_number, punctuator);
   }
}

// Execute tha parse function, but backtrack in case it fails.
AstNode*
parseOrBacktrack(ParseFunction func, Parser* p) {
   Token*   bt     = p->token;
   AstNode* result = func(p);
   if (!result) {
      backtrack(p, bt);
   }

   return result;
}

/**
  Expressions
**/

// Forward declaration of parseExpression.
AstNode* parseExpression(Parser* p);

AstNode*
primaryExpr(Parser* p) {
   AstNode* t   = NULL;
   Token*   tok = nextToken(p);
   if (!tok) { return NULL; }
   if (tok->type == TType_NUMBER ||
       tok->type == TType_FLOAT ||
       tok->type == TType_DOUBLE) {
      t       = newNode(p->arena);
      t->type = Ast_NUMBER;
      t->as_number.tok  = tok;
   }
   else if (tok->type == TType_ID) {
      t       = newNode(p->arena);
      t->type = Ast_ID;
      t->as_id.tok  = tok;
   }
   else if (tok->type == TType_STRING_LITERAL) {
      t = newNode(p->arena);
      t->type = Ast_STRING_LITERAL;
      t->as_string_literal.tok = tok;
       if (!staticArrGet(p->hm_static_arrays, tok->cast.string)) {
         static u32 id = 0;
         char label[64] = {0} ;
         snprintf(label, ArrayCount(label), "id%d", id++);
         staticArrInsert(p->hm_static_arrays, tok->cast.string, getString(label));
      }
   } else if (tok->type == TType_KEYWORD ||
              tok->type == TType_PUNCTUATOR) {
      // Don't do anything
   } else {
      InvalidCodePath;  // We are covering all token types.
   }
   if (t) {
      t->line_number = tok->line_number;
   } else {
      // Backtrack
      backtrack(p, tok);
   }
   return t;
}

// Circular dependency with postfixExpr.
AstNode* argumentExpressionList(Parser* p);

AstNode*
postfixExpr(Parser* p) {
   AstNode* left = primaryExpr(p);
   Token* t = NULL;
   while (left) {
      AstNode* right = NULL;

      if (nextPunctuator(p, '[')) {
         NotImplemented("Array postfix expression");
      }
      else if (nextPunctuator(p, '(')) {
         if (nextPunctuator(p, ')')) {
            right = left;
            left = makeAstNode(p->arena, Ast_FUNCCALL, 0, NULL);
            left->as_funccall.expr = right;
         }
         else {
            AstNode* args = argumentExpressionList(p);
            if (nextPunctuator(p, ')')) {
               right = left;
               left = makeAstNode(p->arena, Ast_FUNCCALL, 0,0);
               left->as_funccall.expr = right;
               left->as_funccall.arg_expr_list = &args->as_arg_expr_list;
            }
            else {
               parseError(p, "Expected ) in function call.");
            }
         }
      }
      else if ((t = nextPunctuator(p, '.'))
               || (t = nextPunctuator(p, ARROW))) {
         if (t->cast.character == ARROW && p->flags & ParserFlag_FANSI) {
            parseError(p, "The -> operator was removed in scc. Compile on legacy mode to use it.");
         }
         Token* id = nextToken(p);
         if (id->type != TType_ID) {
            parseError(p, "Expected identifier after '.' or '->'");
         }
         AstNode* right = makeAstNode(p->arena, Ast_ID, 0, 0);
         right->as_id.tok = id;
         AstNode* child = left;
         left = makeAstNode(p->arena, Ast_STRUCT_MEMBER_ACCESS, 0,0);
         left->as_member_access.primary_expr = child;
         left->as_member_access.field = right->as_id.tok->cast.string;
      }
      else if (nextPunctuator(p, INCREMENT)) {
         AstNode* expr = left;
         left = makeAstNode(p->arena, Ast_POSTFIX_INC, 0, 0);
         left->as_single_expr.expr = expr;
      }
      else if (nextPunctuator(p, DECREMENT)) {
         AstNode* expr = left;
         left = makeAstNode(p->arena, Ast_POSTFIX_DEC, 0, 0);
         left->as_single_expr.expr = expr;
      }

      if (!right) {
         break;
      }
   }
   return left;
}

AstNode*
unaryExpr(Parser* p) {
   /**
   ++
   --
   unary-operator
      &
      *
      +
      -
      ~
      !
   sizeof
   alignof
   **/

   AstNode* t = NULL;

   if (nextPunctuator(p, '*')) {
      NotImplemented("indirection operator");
   }
   else if (nextPunctuator(p, '&')) {
      AstNode* postfix = postfixExpr(p);
      if (!postfix) {
         parseError(p, "Expected expression after &");
      }
      t = makeAstNode(p->arena, Ast_ADDRESS, 0, 0);
      t->as_single_expr.expr = postfix;
   }
   else {
      t = postfixExpr(p);
   }

   return t;
}

AstNode*
castExpr(Parser* p) {
   AstNode* t = NULL;
   t = unaryExpr(p);
   return t;
}

/**
 * TODO: Write documentation about the way we handle left recursion in the C grammar.
 **/

AstNode*
multiplicativeExpr(Parser* p) {
   AstNode* left = castExpr(p);
   if (left) {
      while (peekPunctuator(p, '*') || peekPunctuator(p, '/')) {
         // Pop the *
         Token*   optok     = nextToken(p);
         // Another cast expression
         AstNode* right     = castExpr(p);
         if (right) {
            int   node_type = optok->cast.character == '*' ? Ast_MUL : Ast_DIV;
            left            = makeBinaryExpr(p->arena, node_type, left, right);
         } else {
            parseError(p, "Expected expression after '*'");
         }
      }
   }
   return left;
}

AstNode*
additiveExpr(Parser* p) {
   AstNode*       left      = multiplicativeExpr(p);
   if (left) {
      while (peekPunctuator(p, '+') || peekPunctuator(p, '-')) {
         // Pop the + or the -
         Token*   optok     = nextToken(p);
         // Pop another multiplicative expression.
         AstNode* right     = multiplicativeExpr(p);
         if (right) {
            int   node_type = optok->cast.character == '+' ? Ast_ADD : Ast_SUB;
            left = makeBinaryExpr(p->arena, node_type, left, right);
         } else {
            parseError(p, "Expected expression after '+'");
         }
      }
   }
   return left;
}

AstNode*
relationalExpression(Parser* p) {
   AstNode* left = parseOrBacktrack(additiveExpr, p);
   if (left) {
      while (peekPunctuator(p, '<') ||
             peekPunctuator(p, '>') ||
             peekPunctuator(p, GEQ) ||
             peekPunctuator(p, LEQ)) {
         Token* op = nextToken (p);
         AstNode* right = relationalExpression(p);
         if (!right) { parseError(p, "Expected expression after relational operator."); }
         AstType t = Ast_NONE;
         // TODO: Ast types should reuse keyword and punctuator values.
         switch (op->cast.character) {
            case '<': {
                t = Ast_LESS;
            } break;
            case '>': {
                t = Ast_GREATER;
            } break;
            case GEQ: {
                t = Ast_GEQ;
            } break;
            case LEQ: {
                t = Ast_LEQ;
            } break;
         }
         left = makeBinaryExpr(p->arena, t, left, right);

      }
   }
   return left;
}

AstNode*
equalityExpression(Parser* p) {
   AstNode* left = parseOrBacktrack(relationalExpression, p);
   // TODO: This left-recursion elimination pattern is repeating a lot.
   // Would it be a good tradeoff to abstract it away?
   if (left) {
      Token* eq = NULL;
      while ((eq = nextPunctuator(p, EQUALS)) || (eq = nextPunctuator(p, NOT_EQUALS))) {
         AstNode* right = relationalExpression(p);
         if (!right) { parseError(p, "Expected expression after equality operator."); }
         left = makeBinaryExpr(p->arena, eq->value == EQUALS? Ast_EQUALS : Ast_NOT_EQUALS, left, right);
      }
   }

   return left;
}
AstNode*
andExpression(Parser* p) {
   return equalityExpression(p);
}
AstNode*
exclusiveOrExpr(Parser* p) {
   return andExpression(p);
}
AstNode*
inclusiveOrExpr(Parser* p) {
   // TODO: Implement bit or.
   return exclusiveOrExpr(p);
}

AstNode*
logicalAndExpr(Parser* p) {
   AstNode*       left = inclusiveOrExpr(p);
   if (left) {
      while (peekPunctuator(p, LOGICAL_AND)) {
         nextToken(p);          // Pop the logical and.
         AstNode* right      = inclusiveOrExpr(p);
         if (!right) { parseError(p, "Expected expression after `&&`."); }
         left                = makeBinaryExpr(p->arena, Ast_LOGICAL_AND, left, right);
      }
   }
   return left;
}

AstNode*
logicalOrExpr(Parser* p) {
   AstNode*       left = logicalAndExpr(p);
   if (left) {
      while (peekPunctuator(p, LOGICAL_OR)) {
         nextToken(p);          // Pop the logical or
         AstNode* right      = logicalAndExpr(p);
         if (!right) { parseError(p, "Expected expression after `||`"); }
         int      node_type  = Ast_LOGICAL_OR;
         left                = makeBinaryExpr(p->arena, node_type, left, right);
      }
   }
   return left;
}

AstNode*
conditionalExpr(Parser* p) {
   AstNode*    t  = logicalOrExpr(p);
   Token*      bt = marktrack(p);
   if (nextPunctuator(p, '?')) {
      AstNode* then_expr = parseExpression(p);
      if (!then_expr) {
         parseError(p, "Expected expression after ? token.");
      }
      else {
         bt = marktrack(p);
         if (!nextPunctuator(p, ':')) {
            backtrack(p, bt);
         }
         else {
            AstNode* else_expr = parseExpression(p);
            if (!else_expr) {
               parseError(p, "Expected expression after `:`");
            }
            else {
               // Yay.
            }
         }
      }
   }
   else {
      backtrack(p, bt);
   }
   return t;
}

Token*
assignmentOperator(Parser* p) {
   Token* t = NULL;
   if (false) {}
   else if ((t = nextPunctuator(p, '='))) { }
   else if ((t = nextPunctuator(p, ASSIGN_MODULUS))) { }
   else if ((t = nextPunctuator(p, ASSIGN_BIT_AND))) { }
   else if ((t = nextPunctuator(p, ASSIGN_MULTIPLY))) { }
   else if ((t = nextPunctuator(p, ASSIGN_INCREMENT))) { }
   else if ((t = nextPunctuator(p, ASSIGN_DECREMENT))) { }
   else if ((t = nextPunctuator(p, ASSIGN_DIVIDE))) { }
   else if ((t = nextPunctuator(p, ASSIGN_SHIFT_LEFT))) { }
   else if ((t = nextPunctuator(p, ASSIGN_SHIFT_RIGHT))) { }
   else if ((t = nextPunctuator(p, ASSIGN_BIT_XOR))) { }
   else if ((t = nextPunctuator(p, ASSIGN_BIT_OR))) { }

   return t;
}

AstNode*
assignmentExpression(Parser* p) {
   AstNode* t = NULL;
   Token* bt = marktrack(p);

   AstNode* unary = NULL;
   Token* op = NULL;
   AstNode* assignment = NULL;
   if ((unary = unaryExpr(p))
       && (op = assignmentOperator(p))
       && (assignment = assignmentExpression(p))) {
      t = makeAstNode(p->arena, Ast_ASSIGN_EXPR, 0, 0);

      t->as_assignment_expr.op = op;
      t->as_assignment_expr.lhs = unary;
      t->as_assignment_expr.rhs = assignment;
   }
   else {
      backtrack(p, bt);
      t = conditionalExpr(p);
   }

   return t;
}

AstNode*
argumentExpressionList(Parser* p) {
   AstNode* res = makeAstNode(p->arena, Ast_ARGUMENT_EXPR_LIST,0,0);

   struct AstNodeArgument* arg = &res->as_arg_expr_list;

   while (true) {
      AstNode* assignment = assignmentExpression(p);
      if (!assignment) {
         parseError(p, "Expected argument in expression list");
      }
      else {
         arg->expr = assignment;
         if (peekPunctuator(p, ',')) {
            arg->next = AllocType(p->arena, struct AstNodeArgument);
            arg = arg->next;
            nextToken(p);
         }
         else {
            break;
         }
      }
   }
   return res;
}



AstNode*
parseExpression(Parser* p) {
   AstNode* t = assignmentExpression(p);
   return t;
}

// ==== Declarations ====

void parseTypeSpecifier(Parser* p, Token* t, Ctype* out);
AstNode* parseDeclarator(Parser* p);

int
parseTypeQualifier(Token* t) {
   int quals = 0;

   int v = t->value;

   quals =  (v == Keyword_const) ? Qual_CONST : 0
          | (v == Keyword_restrict) ? Qual_RESTRICT : 0
          | (v == Keyword_volatile) ? Qual_VOLATILE : 0;

   return quals;
}


AstNode*
parseSpecifierQualifierList(Parser* p) {
   AstNode* result = NULL;
   Ctype ctype = { .type = Type_NONE };
   i32 line_number = p->token->line_number;

   int qualifiers = 0;
   int qual;

   while (p->token->type == TType_KEYWORD) {
      Token* t = nextToken(p);
      if ((parseTypeSpecifier(p, t, &ctype), ctype.type)) {
      }
      else if ((qual = parseTypeQualifier(t))) {
         qualifiers |= qual;
      }
      else {
         backtrack(p, t);
         break;
      }
   }

   if (ctype.type != Type_NONE) {
      result = makeAstNodeWithLineNumber(p->arena, Ast_DECLARATION_SPECIFIER, NULL, NULL, line_number);
      result->as_decl_specifier.ctype = ctype;
   }

   return result;
}


AstNode*
parseStructDeclarationList(Parser* p) {
   AstNode* decls = makeAstNode(p->arena, Ast_DECLARATION_LIST, 0,0);
   struct AstNodeDeclarationList* list = &decls->as_declaration_list;
   struct AstNodeDeclarationList** list_iter = &list;
   // Parse struct declarations
   while (true) {
      AstNode* specs = parseSpecifierQualifierList(p);
      if (!specs) {
         break;
      }
      // Parse struct declarator list
      AstNode* declarators_node = makeAstNode(p->arena, Ast_DECLARATOR_LIST, 0,0);
      struct AstNodeDeclaratorList* declarators = &declarators_node->as_declarator_list;

      struct AstNodeDeclaratorList** decl_iter = &declarators;
      while (true) {
         if (!*decl_iter) {
            (*decl_iter) = AllocType(p->arena, struct AstNodeDeclaratorList);
         }
         AstNode* d = parseDeclarator(p);
         if (!d) {
            break;
         }
         (*decl_iter)->declarator = &d->as_declarator;

         decl_iter = &(*decl_iter)->next;
      }
      if (!declarators->declarator) {
         parseError(p, "Expected declarator in struct declaration.");
      }

      expectPunctuator(p, ';');

      if (!*list_iter) { *list_iter = AllocType(p->arena, struct AstNodeDeclarationList); }

      (*list_iter)->declaration = AllocType(p->arena, struct AstNodeDeclaration);
      (*list_iter)->declaration->decl_spec = &specs->as_decl_specifier;
      (*list_iter)->declaration->declarators = declarators;

      list_iter = &(*list_iter)->next;
   }

   return decls;
}

void
parseTypeSpecifier(Parser* p, Token* t, Ctype* out) {
   if (out->type != Type_NONE) {
      parseError(p, "Cannot have more than one type specifier in declaration.");
   }
   switch (t->value) {
      case Keyword_int: {
         out->type = Type_INT;
      } break;
      case Keyword_char: {
         out->type = Type_CHAR;
      } break;
      case Keyword_float: {
         out->type = Type_FLOAT;
      } break;
      case Keyword_double: {
         out->type = Type_DOUBLE;
      } break;
      case Keyword_struct: {
         AstNode* decl_list = NULL;
         out->type = Type_AGGREGATE;
         Token* id = nextToken(p);
         if (id->type != TType_ID) {
            expectPunctuator(p, '{');
            decl_list = parseStructDeclarationList(p);
            noneIfNull(p->arena, &decl_list);
            expectPunctuator(p, '}');
         } else {
            out->aggr.tag = id->cast.string;
            if (nextPunctuator(p, '{')) {
               decl_list = parseStructDeclarationList(p);
               noneIfNull(p->arena, &decl_list);
               expectPunctuator(p, '}');
            }
            else {
               u64* bits = aggrGet(p->hm_sizes, id->cast.string);
               Assert (bits);
               out->aggr.bits = *bits;
            }
         }
         if (decl_list) {
            for (struct AstNodeDeclarationList* decls = &decl_list->as_declaration_list;
                 decls;
                 decls = decls->next) {
               struct AstNodeDeclaration* decl = decls->declaration;
               struct AstNodeDeclSpec* spec = decl->decl_spec;
               out->aggr.bits += typeBits(&spec->ctype);
               out->aggr.bits = AlignPow2(out->aggr.bits, 8);
            }
            if ( !aggrGet(p->hm_sizes, id->cast.string) ) {
               aggrInsert(p->hm_sizes, id->cast.string, out->aggr.bits);
            }
            out->aggr.decls = decl_list;
         }
      } break;
      case Keyword_long: {
         NotImplemented("long");
      } break;
      case Keyword_short: {
         NotImplemented("short");
      } break;
      case Keyword_union: {
         NotImplemented("unions");
      } break;
      default: {
         // Not a type specifier.
      } break;
   }
}


AstNode*
parseDeclarationSpecifiers(Parser* p) {
   // One or more of:
   //   storage-class-specifier
   //   type-specifier
   //   function specifier
   // Token* bt = t;
   AstNode* result = NULL;
   Ctype ctype = { .type = Type_NONE };
   i32 line_number = p->token->line_number;

#define MaxSpecifiers 1
   int storage_spec[MaxSpecifiers] = Zero;
   int n_storage_spec = 0;
   int qualifiers = 0;
   int qual;

   while (p->token->type == TType_KEYWORD) {
      Token* t = nextToken(p);
      int v = t->value;
      // Storage class specifiers
      if (v == Keyword_typedef ||
          v == Keyword_extern ||
          v == Keyword_static ||
          v == Keyword_auto ||
          v == Keyword_register) {
         ArrayPush(storage_spec, v);
      }
      // Type specifiers
      else if ((parseTypeSpecifier(p, t, &ctype), ctype.type)) {
      }
      else if ((qual = parseTypeQualifier(t))) {
         qualifiers |= qual;
      }
      else {
         backtrack(p, t);
         break;
      }
   }
   if (ctype.type != Type_NONE) {
      result = makeAstNodeWithLineNumber(p->arena, Ast_DECLARATION_SPECIFIER, NULL, NULL, line_number);
      result->as_decl_specifier.ctype = ctype;
   }

   return result;
}

AstNode*
parameterTypeList(Parser* p) {
   AstNode* result = NULL;
   struct AstNodeParameter* param = NULL;

   while (true) {
      AstNode* decl_spec = parseOrBacktrack(parseDeclarationSpecifiers, p);
      if (decl_spec) {
         if (!result) {
            Assert(!param);
            result = makeAstNode(p->arena, Ast_PARAMETER, 0, 0);
            param = &result->as_parameter;
         }
         else {
            param->next = AllocType(p->arena, struct AstNodeParameter);
            param = param->next;
         }

         param->decl_specifier = decl_spec;

         AstNode* declarator = parseOrBacktrack(parseDeclarator, p);
         if (declarator) {
            param->declarator = &declarator->as_declarator;
         }
         if (peekPunctuator(p, ',')) {
            // There is another parameter.
            nextToken(p);
         } else {
            break;
         }
      }
      else {
         break; // Empty param list
      }
   }

   return result;
}

// TODO: Implement declarators completely
AstNode*
parseDeclarator(Parser* p) {
   AstNode* r = NULL;
   Token* t;
   Token* bt = marktrack(p);
   u32 pointer_stars = 0;

   while (nextPunctuator(p, '*')) {
      pointer_stars++;
   }

   if (nextPunctuator(p, '(')) {
      AstNode* inner = parseDeclarator(p);
      if (!inner) {
         parseError(p, "Expected declarator inside (");
      }
      else {
         expectPunctuator(p, ')');
      }
   }

   if ((t = nextToken(p)) && (t->type == TType_ID)) {
      AstNode* node = makeAstNodeWithLineNumber(p->arena, Ast_ID, NULL, NULL, t->line_number);
      node->as_id.tok = t;
      AstNode* params = NULL;
      if (nextPunctuator(p, '(')) {
         params = parseOrBacktrack(parameterTypeList, p);
         if (!nextPunctuator(p, ')')) {
            parseError(p, "Expected ) in declarator after parameter list");
         }
      }
      r = makeAstNodeWithLineNumber(p->arena, Ast_DECLARATOR, node, NULL, t->line_number);
      r->as_declarator.id = t->cast.string;
      r->as_declarator.params = params;
      r->as_declarator.pointer_stars = pointer_stars;
   }
   else {
      backtrack(p, bt);
   }
   return r;
}

AstNode*
parseDeclarationList(Parser* p) {
   return NULL;
}

AstNode*
parseInitializer(Parser* p) {
   // TODO: Initializers.
   AstNode* node = assignmentExpression(p);
   return node;
}

AstNode*
parseDeclaration(Parser* p) {
   AstNode* result = NULL;
   AstNode* specifiers = parseDeclarationSpecifiers(p);

   if (specifiers) {
      AstNode* declarator = parseDeclarator(p);

      AstNode* initializer = NULL;

      result = makeAstNode(p->arena, Ast_DECLARATION, 0,0);

      if (declarator) {
         if (nextPunctuator(p, '=')) {
            initializer = parseInitializer(p);
         }

         // TODO: Parse declarator list...
         AstNode* declarators = makeAstNode(p->arena, Ast_DECLARATOR_LIST, 0,0);
         declarators->as_declarator_list.declarator = &declarator->as_declarator;
         result->as_declaration.declarators = &declarators->as_declarator_list;
      }

      expectPunctuator(p, ';');

      result->as_declaration.decl_spec = &specifiers->as_decl_specifier;
      result->as_declaration.rhs = initializer;
   }
   return result;
}

// ==== Statements and blocks ====

AstNode*
parseJumpStatement(Parser* p) {
   AstNode* stmt = NULL;
   Token* bt = marktrack(p);
   Token* t = nextToken(p);
   if (t->type == TType_KEYWORD && t->value == Keyword_return) {
      AstNode* expr = parseOrBacktrack(parseExpression, p);
      stmt = makeAstNode(p->arena, Ast_RETURN, 0, NULL);
      stmt->as_single_expr.expr = expr;
      expectPunctuator(p, ';');
   }
   else {
      backtrack(p, bt);
   }
   return stmt;
}

AstNode* parseStatement(Parser* p);  // Resolve a circular dependency
                                     // between parseCompoundStatement
                                     // and parseStatement
AstNode*
parseCompoundStatement(Parser* p) {
   Token* tok = marktrack(p);
   AstNode* compound_stmt = NULL;

   struct AstNodeCompoundStmt comp = Zero;


   if (nextPunctuator(p, '{')) {
      for (struct AstNodeCompoundStmt* comp_iter = &comp;
           comp_iter;
           comp_iter = comp_iter->next) {
         comp_iter->stmt = parseOrBacktrack(parseDeclaration, p);
         if (!comp_iter->stmt) {
            comp_iter->stmt = parseOrBacktrack(parseStatement, p);
         }
         if (!comp_iter->stmt) {
            if (nextPunctuator(p, '}')) {
               compound_stmt = makeAstNode(p->arena, Ast_COMPOUND_STMT, NULL, NULL);
               compound_stmt->as_compound_stmt = comp;
            }
         }
         else {
            comp_iter->next = AllocType(p->arena, struct AstNodeCompoundStmt);
         }
      }
   } else {
      backtrack(p, tok);
   }

   return compound_stmt;
}

AstNode*
parseExpressionStatement(Parser* p) {
   AstNode* stmt =  NULL;
   if ((stmt = parseExpression(p))) {
      if (!nextPunctuator(p, ';')) {
         parseError(p, "Expected ';' after expression.");
      }
   }
   return stmt;
}

AstNode*
parseStatement(Parser* p) {
   AstNode* stmt = NULL;
   if (nextPunctuator(p, ';')) {
      noneIfNull(p->arena, &stmt);
   }
   else if ((stmt = parseCompoundStatement(p))) { }
   else if ((stmt = parseExpressionStatement(p))) { }
   else if ((stmt = parseJumpStatement(p))) {}
   else if (nextKeyword(p, Keyword_if)) {
      if (nextPunctuator(p, '(')) {
         // TODO: There should be checking of `if` conditions.
         // For instance, emit a warning when using = instead of ==.
         AstNode* cond = parseExpression(p);
         expectPunctuator(p, ')');
         AstNode* then = parseStatement(p);
         if (then) {
            stmt = makeAstNode(p->arena, Ast_IF, 0, 0);
            stmt->as_if.condition = cond;
            stmt->as_if.then = then;

            if (nextKeyword(p, Keyword_else)) {
               AstNode* else_ = parseStatement(p);
               stmt->as_if.else_ = else_;
            }
         } else {
            parseError(p, "Expected else clause after if.");
         }
      }
   }
   // === Iteration ===
   else if (nextKeyword(p, Keyword_while)) {
      if (!nextPunctuator(p, '(')) {
         parseError(p, "Expected ( after while.");
      } else {
         AstNode* expr = parseExpression(p);
         if (!expr) {
            parseError(p, "expected expression inside parens");
         }
         if (!nextPunctuator(p, ')')) {
            parseError(p, "Expected ).");
         }
         else {
            AstNode* statement = parseStatement(p);
            if (!statement) {
               parseError(p, "expected statement after while");
            }
            AstNode* control_before = expr;
            AstNode* body = statement;

            stmt = makeAstNode(p->arena, Ast_ITERATION, NULL, NULL);
            stmt->as_iteration.declaration = NULL;
            stmt->as_iteration.before_iteration = control_before;
            stmt->as_iteration.after_iteration = NULL;
            stmt->as_iteration.body = body;
         }
      }
   }
   else if (nextKeyword(p, Keyword_for)) {
      // Break;
      if (!nextPunctuator(p, '(')) {
         parseError(p, "Expected ( after while.");
      }
      else {
         AstNode* declaration = parseDeclaration(p);
         if (!declaration && !nextPunctuator(p, ';')) {
            parseError(p, "Expected ; after declaration clause.");
         }
         AstNode* control = parseExpression(p);
         if (!nextPunctuator(p, ';')) {
            parseError(p, "Expected ; after control expression.");
         }
         AstNode* after = parseExpression(p);
         if (!nextPunctuator(p, ')')) {
            parseError(p, "Expected ')'.");
         }
         AstNode* body = parseStatement(p);

         if (!body) {
            parseError(p, "Expected body after for(..)");
         }

         stmt = makeAstNode(p->arena, Ast_ITERATION, declaration, NULL);

         stmt->as_iteration.declaration = (declaration) ? &declaration->as_declaration : NULL;
         stmt->as_iteration.before_iteration = control;
         stmt->as_iteration.after_iteration = after;
         stmt->as_iteration.body = body->type != Ast_NONE ? body : NULL;
      }
   }
   return stmt;
}

Ctype*
resolveTypeAndDeclarator(Arena* arena, Ctype* type, u32 stars) {
   Ctype* result = type;
   while (stars--) {
      Ctype* new_type = AllocType(arena, Ctype);
      new_type->type = Type_POINTER;
      new_type->pointer.pointee = result;
      result = new_type;
   }
   return result;
}

AstNode*
parseFunctionDefinition(Parser* p) {
   AstNode* result = NULL;

   AstNode* declaration_specifier = NULL;
   AstNode* declarator = NULL;

   Token* bt = marktrack(p);

   if ((declaration_specifier = parseDeclarationSpecifiers(p)) != NULL &&
       (declarator = parseDeclarator(p)) != NULL) {

      parseOrBacktrack(parseDeclarationList, p);
      AstNode* stmts = parseCompoundStatement(p);

      if (!stmts) {
         backtrack(p, bt);
      }
      else {
         AstNode* node = makeAstNode(p->arena, Ast_FUNCDEF, 0, 0);

         struct AstNodeParameter* param = declarator->as_declarator.params ? &declarator->as_declarator.params->as_parameter : NULL;

         Ctype* s_params = NULL;

         while (param) {
            u32 stars = param->declarator->pointer_stars;
            Ctype* type = resolveTypeAndDeclarator(p->arena, &param->decl_specifier->as_decl_specifier.ctype, stars);
            bufPush(s_params, *type);
            param = param->next;
         }

         Ctype* return_type = resolveTypeAndDeclarator(p->arena, &declaration_specifier->as_decl_specifier.ctype, declarator->as_declarator.pointer_stars);

         Ctype type = {
            .type = Type_FUNC,
            .func = (struct CtypeFunc){
               .return_type = return_type,
               .s_params = s_params,
            },
         };

         node->as_funcdef.label = declarator->as_declarator.id;
         node->as_funcdef.declarator = &declarator->as_declarator;
         node->as_funcdef.compound_stmt = stmts;
         node->as_funcdef.ctype = type;

         result = node;
      }
   }
   else {
      backtrack(p, bt);
   }
   return result;
}

AstNode*
parseTranslationUnit(Parser* p) {
   // Return a list of declarations and function definitions.

   AstNode* result = makeAstNode(p->arena, Ast_TRANSLATION_UNIT, 0,0);

   struct AstNodeTU* tu_node = &result->as_tu;

   AstNode* cur = NULL;
   while (true) {
      if (   (cur = parseFunctionDefinition(p))
          || (cur = parseDeclaration(p))) {     // TODO: Top level declarations.

         tu_node->node = cur;
         tu_node = tu_node->next = AllocType(p->arena, struct AstNodeTU);
      } else {
         break;
      }
   }

   return result;
}
