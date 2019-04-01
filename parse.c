// A function that takes a Parser and returns an AstNode*
typedef AstNode* ParseFunction(Parser*);

void
initParser(Parser* p) {
   p->flags = ParserFlag_FANSI;
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
      t->number.tok  = tok;
   }
   else if (tok->type == TType_ID) {
      t       = newNode(p->arena);
      t->type = Ast_ID;
      t->id.tok  = tok;
   }
   else if (tok->type == TType_STRING_LITERAL) {
      NotImplemented("String literal");
   } else if (tok->type == TType_KEYWORD ||
              tok->type == TType_PUNCTUATOR) {
      // Don't do anything
   } else {
      NotImplemented("Token primary expression");
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
            left->child = right;
         }
         else {
            AstNode* args = argumentExpressionList(p);
            if (nextPunctuator(p, ')')) {
               right = left;
               left = makeAstNode(p->arena, Ast_FUNCCALL, 0,0);
               left->child = right;
               left->child->next = args;
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
         right->id.tok = id;
         AstNode* child = left;
         left = makeAstNode(p->arena, Ast_STRUCT_MEMBER_ACCESS, 0,0);
         left->child = child;
         left->child->next = right;
      }
      else if (nextPunctuator(p, INCREMENT)) {
         AstNode* expr = left;
         left = makeAstNode(p->arena, Ast_POSTFIX_INC, 0, 0);
         left->single_expr.expr = expr;
      }
      else if (nextPunctuator(p, DECREMENT)) {
         AstNode* expr = left;
         left = makeAstNode(p->arena, Ast_POSTFIX_DEC, 0, 0);
         left->single_expr.expr = expr;
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
      t->single_expr.expr = postfix;
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
            AstNode* child = left;
            left            = makeAstNode(p->arena, node_type, 0,0);
            left->child = child;
            left->child->next = right;
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
            AstNode* child = left;
            left            = makeAstNode(p->arena, node_type, 0,0);
            left->child = child;
            left->child->next = right;
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
         AstNode* child = left;
         left = makeAstNode(p->arena, t, left, right);
         left->child = child;
         left->child->next = right;

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
         AstNode* child = left;
         left = makeAstNode(p->arena, eq->value == EQUALS? Ast_EQUALS : Ast_NOT_EQUALS, 0,0 );
         left->child = child;
         left->child->next = right;
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
         left                = makeAstNode(p->arena, Ast_LOGICAL_AND, left, right);
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
         left                = makeAstNode(p->arena, node_type, left, right);
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

      t->assignment_expr.op = op;
      t->assignment_expr.lhs = unary;
      t->assignment_expr.rhs = assignment;
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

   struct AstNodeArgument* arg = &res->arg_expr_list;

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

AstNode*
parseStructDeclarationList(Parser* p) {
   AstNode* decls = NULL;
   AstNode** decls_iter = &decls;
   // Parse struct declarations
   while (true) {
      // Parse a list of type specifiers
      AstNode* specs = NULL;
      AstNode** iter = &specs;
      while (true) {
         Ctype ctype = Zero;
         parseTypeSpecifier(p, p->token, &ctype);
         if (ctype.type == Type_NONE) {
            break;
         }
         nextToken(p);
         AstNode* spec = makeAstNodeWithLineNumber(p->arena, Ast_DECLARATION_SPECIFIER, NULL, NULL, p->token->line_number);
         spec->decl_specifier.ctype = ctype;
         (*iter) = spec;
         iter = &spec->next;
      }
      if (!specs) {
         goto end;
      }
      // Parse struct declarator list
      AstNode* decl = NULL;
      AstNode** decl_iter = &decl;
      while (true) {
         *decl_iter = parseDeclarator(p);
         // TODO: Bit field colon in struct declarator.
         if (!*decl_iter) {
            break;
         }
         decl_iter = &(*decl_iter)->next;
      }
      if (!decl) {
         parseError(p, "Expected declarator in struct declaration.");
      }

      expectPunctuator(p, ';');

      *decls_iter = makeAstNode(p->arena, Ast_DECLARATION, specs, decl);
      (*decls_iter)->child = specs;
      (*decls_iter)->child->next = decl;
      decls_iter = &(*decls_iter)->next;
   }
end:

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
               u64* bits = aggrGet(&p->sizes, id->cast.string);
               Assert (bits);
               out->aggr.bits = *bits;
            }
         }
         if (decl_list) {
            for (AstNode* decl = decl_list;
                 decl;
                 decl = decl->next) {
               AstNode* spec = decl->child;
               out->aggr.bits += typeBits(&spec->ctype);
               out->aggr.bits = AlignPow2(out->aggr.bits, 8);
            }
            if ( !aggrGet(&p->sizes, id->cast.string) ) {
               aggrInsert(&p->sizes, id->cast.string, out->aggr.bits);
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
      case Keyword__Bool: {
      } //break;
      case Keyword__Complex: {
      } //break;
      case Keyword__Imaginary: {
         NotImplemented("Type Specifier.");
      } //break;
      default: {
         // Not a type specifier.
      } break;
   }
}


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
      result->ctype = ctype;
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
            param = &result->parameter;
         }
         else {
            param->next = AllocType(p->arena, struct AstNodeParameter);
            param = param->next;
         }

         param->decl_specifier = decl_spec;

         AstNode* declarator = parseOrBacktrack(parseDeclarator, p);
         if (declarator) {
            param->declarator = declarator;
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
      node->id.tok = t;
      AstNode* params = NULL;
      if (nextPunctuator(p, '(')) {
         params = parseOrBacktrack(parameterTypeList, p);
         if (!nextPunctuator(p, ')')) {
            parseError(p, "Expected ) in declarator after parameter list");
         }
      }
      r = makeAstNodeWithLineNumber(p->arena, Ast_DECLARATOR, node, NULL, t->line_number);
      r->declarator.id = t->cast.string;
      r->declarator.params = params;
      r->declarator.pointer_stars = pointer_stars;
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
   // TODO: Declaration lists
   if (specifiers) {
      AstNode* declarator = parseDeclarator(p);

      if (declarator) {
         AstNode* initializer = NULL;

         if (nextPunctuator(p, '=')) {
            initializer = parseInitializer(p);
         }
         noneIfNull(p->arena, &initializer);
         declarator->next = initializer;
      }

      expectPunctuator(p, ';');

      noneIfNull(p->arena, &declarator);
      result = makeAstNode(p->arena, Ast_DECLARATION, 0,0);
      result->child = specifiers;
      result->child->next = declarator;
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
      stmt->single_expr.expr = expr;
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
   AstNode* first_stmt = NULL;
   Token* tok = marktrack(p);
   AstNode* compound_stmt = NULL;

   if (nextPunctuator(p, '{')) {
      AstNode** cur = &first_stmt;
      do {
         if (*cur) {
            cur = &(*cur)->next;
         }
         *cur = parseOrBacktrack(parseDeclaration, p);
         if (!*cur) {
            *cur = parseOrBacktrack(parseStatement, p);
         }
         if (!*cur) {
            if (nextPunctuator(p, '}')) {
               compound_stmt = makeAstNode(p->arena, Ast_COMPOUND_STMT, NULL, NULL);
               compound_stmt->child = first_stmt;
            }
         }
      } while (*cur);
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
            stmt->if_.condition = cond;
            stmt->if_.then = then;

            if (nextKeyword(p, Keyword_else)) {
               AstNode* else_ = parseStatement(p);
               stmt->if_.else_ = else_;
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
            AstNode* declarations = makeAstNode(p->arena, Ast_NONE, 0,0);
            AstNode* control_before = expr;
            AstNode* after = makeAstNode(p->arena, Ast_NONE, 0,0);
            AstNode* body = statement;

            declarations->next = control_before;
            control_before->next = after;
            after->next = body;
            body->next = NULL;

            stmt = makeAstNode(p->arena, Ast_ITERATION, declarations, NULL);
            stmt->child = declarations;
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

         noneIfNull(p->arena, &declaration);
         noneIfNull(p->arena, &control);
         noneIfNull(p->arena, &after);

         declaration->next = control;
         control->next = after;
         after->next = body;
         stmt = makeAstNode(p->arena, Ast_ITERATION, declaration, NULL);
         stmt->child = declaration;
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

         struct AstNodeParameter* param = declarator->declarator.params ? &declarator->declarator.params->parameter : NULL;

         Ctype* s_params = NULL;

         while (param) {
            u32 stars = param->declarator->declarator.pointer_stars;
            Ctype* type = resolveTypeAndDeclarator(p->arena, &param->decl_specifier->decl_specifier.ctype, stars);
            bufPush(s_params, *type);
            param = param->next;
         }

         Ctype* return_type = resolveTypeAndDeclarator(p->arena, &declaration_specifier->ctype, declarator->declarator.pointer_stars);

         Ctype type = {
            .type = Type_FUNC,
            .func = (struct CtypeFunc){
               .return_type = return_type,
               .s_params = s_params,
            },
         };

         node->funcdef.label = declarator->declarator.id;
         node->funcdef.declarator = declarator;
         node->funcdef.compound_stmt = stmts;
         node->funcdef.ctype = type;

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
   //
   AstNode* result = NULL;
   AstNode** cur = &result;
   while (true) {
      if (   (*cur = parseFunctionDefinition(p))
          || (*cur = parseDeclaration(p))) {     // TODO: Top level declarations.
         cur = &((*cur)->next);
      } else {
         break;
      }
   }

   return result;
}
