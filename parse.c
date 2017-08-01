

typedef struct Parser_s {
   Token* token;  // The next token to parse.
   Arena* arena;
   AstNode* tree;
   char* file_name;
} Parser;


// A function that takes a Parser and returns an AstNode*
typedef AstNode* ParseFunction(Parser*);

AstNode*
newNode(Arena* a) {
   AstNode* r = AllocType(a, AstNode);
   return r;
}

void
parseError(char* msg, ...) {
   va_list args;
   va_start(args, msg);
   char buffer[LINE_MAX] = {0};
   vsnprintf(buffer, LINE_MAX, msg, args);
   fprintf(stderr, "Syntax error: %s\n", buffer);
   va_end(args);
   exit(1);
}

b32
peekPunctuator(Parser* p, int c) {
   b32 result = false;
   if (c < 128) {
      result = p->token->type == TType_PUNCTUATOR && p->token->value.character == c;
   } else {
      result = p->token->type == TType_PUNCTUATOR_MULTICHAR && p->token->value.character == c;
   }
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
nextKeyword(Parser* p, int keyword) {
   Token* tok = nextToken(p);
   if (tok->type == TType_KEYWORD && tok->value.integer == keyword) {
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
      parseError("%s: %d Expected '%c'", p->file_name, p->token->line_number, punctuator);
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
   if (tok->type == TType_NUMBER) {
      t       = newNode(p->arena);
      t->type = Ast_NUMBER;
      t->tok  = tok;
   }
   else if (tok->type == TType_ID) {
      t       = newNode(p->arena);
      t->type = Ast_ID;
      t->tok  = tok;
   }
   // TODO: other constants, string literals
   else {
   }
   if (t) {
      t->line_number = tok->line_number;
   }
   return t;
}

AstNode*
postfixExpr(Parser* p) {
   AstNode*  t = NULL;
   if ((t = primaryExpr(p), t)) {
      Token* tok = nextToken(p);
      if (tok->type == TType_PUNCTUATOR && tok->value.character == '[') {
         parseError ("I don't know how to continue");
      }
      else if (tok) {
         backtrack(p, tok);
         return t;
      }
   }
   return t;
}

AstNode*
unaryExpr(Parser* p) {
   AstNode* t = postfixExpr(p);
   return t;
}

AstNode*
castExpr(Parser* p) {
   AstNode* t = NULL;
   t = unaryExpr(p);
   return t;
}

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
            int   node_type = optok->value.character == '*' ? Ast_MUL : Ast_DIV;
            left            = makeAstNode(p->arena, node_type, left, right);
         } else {
            parseError("Expected expression after '*'");
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
            int   node_type = optok->value.character == '+' ? Ast_ADD : Ast_SUB;
            left            = makeAstNode(p->arena, node_type, left, right);
         } else {
            parseError("Expected expression after '+'");
         }
      }
   }
   return left;
}


AstNode*
inclusiveOrExpr(Parser* p) {
   // TODO: Implement bit or.
   return additiveExpr(p);
}

AstNode*
logicalAndExpr(Parser* p) {
   AstNode*       left = inclusiveOrExpr(p);
   if (left) {
      while (p->token->type == TType_PUNCTUATOR && p->token->value.character == LOGICAL_AND) {
         nextToken(p);          // Pop the logical and.
         AstNode* right      = inclusiveOrExpr(p);
         if (!right) { parseError("Expected expression after `&&`."); }
         left                = makeAstNode(p->arena, Ast_LOGICAL_AND, left, right);
      }
   }
   return left;
}

AstNode*
logicalOrExpr(Parser* p) {
   AstNode*       left = logicalAndExpr(p);
   if (left) {
      while (p->token->type == TType_PUNCTUATOR && p->token->value.character == LOGICAL_OR) {
         nextToken(p);          // Pop the logical or
         AstNode* right      = logicalAndExpr(p);
         if (!right) { parseError("Expected expression after `||`"); }
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
         parseError("Expected expression after ? token.");
      }
      else {
         bt = marktrack(p);
         if (!nextPunctuator(p, ':')) {
            backtrack(p, bt);
         }
         else {
            AstNode* else_expr = parseExpression(p);
            if (!else_expr) {
               parseError("Expected expression after `:`");
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

AstNode*
assignmentExpr(Parser* p) {
   AstNode* t = conditionalExpr(p);
   // TODO: unaryExpr assignmentOperator assignmentExpr
   return t;
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
         if (!right) { parseError("Expected expression after relational operator."); }
         AstType t = Ast_NONE;
         // TODO: Ast types should reuse keyword and punctuator values.
         switch (op->value.character) {
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
         left = makeAstNode(p->arena, t, left, right);
      }
   }
   return left;
}

AstNode*
equalityExpression(Parser* p) {
   AstNode* left                   = parseOrBacktrack(relationalExpression, p);
   // TODO: This left-recursion elimination pattern is repeating a lot.
   // Would it be a good tradeoff to abstract it away?
   if (left) {
      while (nextPunctuator(p, EQUALS)) {
         AstNode* right = relationalExpression(p);
         if (!right) { parseError("Expected expression after '=='."); }
         left = makeAstNode(p->arena, Ast_EQUALS, left, right);
      }
   }

   return left;
}

AstNode*
parseExpression(Parser* p) {
   // AstNode* t = additiveExpr(p);
   AstNode* t = equalityExpression(p);
   return t;
}

// ==== Declarations ====

AstNode*
parseDeclarationSpecifiers(Parser* p) {
   // One or more of:
   //   storage-class-specifier
   //   type-specifier
   //   function specifier
   Token* t = nextToken(p);
   Token* bt = t;
   AstNode* result = NULL;

   if (t->type == TType_KEYWORD) {

      result = makeAstNodeWithLineNumber(p->arena, Ast_KEYWORD, NULL, NULL, t->line_number);
      int v = t->value.integer;
      // Storage class specifiers
      if (v == Keyword_typedef ||
          v == Keyword_extern ||
          v == Keyword_static ||
          v == Keyword_auto ||
          v == Keyword_register) {

      }
      // Type specifiers
      else if (t->value.integer == Keyword_int) {

      }
      else {
         result = NULL;
         backtrack(p, bt);
      }
   }

   return result;
}

AstNode*
parseDeclarator(Parser* p) {
   AstNode* r = NULL;
   Token* t = nextToken(p);
   if (t->type == TType_ID) {
      r = makeAstNodeWithLineNumber(p->arena, Ast_ID, NULL, NULL, t->line_number);
      r->tok = t;
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
   AstNode* node = assignmentExpr(p);
   return node;
}

AstNode*
parseDeclaration(Parser* p) {
   AstNode* result = NULL;
   AstNode* specifiers = parseDeclarationSpecifiers(p);
   if (specifiers) {
      AstNode* identifier = parseDeclarator(p);
      AstNode* initializer = NULL;

      Token* bt = marktrack(p);
      if (nextPunctuator(p, '=')) {
         initializer = parseInitializer(p);
      }
      else {
         backtrack(p, bt);
      }
      expectPunctuator(p, ';');
      result = makeAstNode(p->arena, Ast_DECLARATION, identifier, initializer);
   }
   return result;
}

// ==== Statements and blocks ====

AstNode*
parseJumpStatement(Parser* p) {
   AstNode* stmt = NULL;
   Token* t = nextToken(p);
   if (t->type == TType_KEYWORD && t->value.integer == Keyword_return) {
      AstNode* expr = parseOrBacktrack(parseExpression, p);
      stmt = makeAstNode(p->arena, Ast_RETURN, expr, NULL);
      expectPunctuator(p, ';');
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
            cur = &(*cur)->sibling;
         }
         *cur = parseOrBacktrack(parseDeclaration, p);
         if (!*cur) {
            *cur = parseOrBacktrack(parseStatement, p);
         }
      } while (*cur);
      if (nextPunctuator(p, '}')) {
         compound_stmt = makeAstNode(p->arena, Ast_COMPOUND_STMT, first_stmt, NULL);
      } else {
         backtrack(p, tok);
      }
   } else {
      backtrack(p, tok);
   }

   return compound_stmt;
}

AstNode*
parseStatement(Parser* p) {
   AstNode* stmt = NULL;
   // TODO: Parse the different kinds of statements in the correct order.
   // label
   // compound
   // expression
   //
   if ((stmt = parseCompoundStatement(p)) != NULL) {

   }
   else if (((stmt = parseOrBacktrack(parseExpression, p)) != NULL && nextPunctuator(p, ';')) ||
            (stmt = parseOrBacktrack(parseJumpStatement, p)) != NULL) {
   }
   else if (nextKeyword(p, Keyword_if)) {
      if (nextPunctuator(p, '(')) {
         // TODO: There should be checking of `if` conditions.
         // For instance, emit a warning when using = instead of ==.
         AstNode* cond = parseExpression(p);
         expectPunctuator(p, ')');
         AstNode* then = parseStatement(p);
         if (then) {
            stmt = makeAstNode(p->arena, Ast_IF, cond, then);
         } else {
            parseError("Expected else clause after if.");
         }
      }
   }
   // TODO:
   //   iteration-statement
   return stmt;
}

AstNode*
parseFunctionDefinition(Parser* p) {
   AstNode* result = NULL;

   AstNode* declaration_specifier = NULL;
   AstNode* declarator = NULL;

   if ((declaration_specifier = parseDeclarationSpecifiers(p)) != NULL &&
       (declarator = parseDeclarator(p)) != NULL) {

      AstNode* funcdef = makeAstNode(p->arena, Ast_FUNCDEF, declaration_specifier, declarator);

      // TODO: Support something other than no-params
      nextPunctuator(p, '(');
      nextPunctuator(p, ')');

      parseOrBacktrack(parseDeclarationList, p);
      AstNode* stmts = parseCompoundStatement(p);
      declarator->sibling = stmts;

      result = funcdef;
   }
   return result;

}

AstNode*
parseTranslationUnit(Parser* p) {
   // How to parse a function declaration
   // 1. declaration specifier
   // 2. declarator
   // 3. declaration list (one or more declarations)
   // 4. compound statement

   // Return a list of declarations and function definitions.
   //
   AstNode* result = NULL;
   AstNode** cur = &result;
   while (true) {
      *cur = parseFunctionDefinition(p);
      if (*cur) {
         cur = &((*cur)->sibling);
      } else {
         break;
      }
   }

   return result;
}
