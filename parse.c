
typedef struct Parser_s {
   Token* token;  // The next token to parse.
   Arena* arena;
   AstNode* tree;
} Parser;

AstNode*
newNode(Arena* a) {
   AstNode* r = AllocType(a, AstNode);
   return r;
}

void
parseError(char* msg) {
   fprintf(stderr, "Parse error: %s\n", msg);
   exit(1);
}

b32
peekCharPunctuator(Parser* p, char c) {
   b32 result = p->token->type == TokenType_PUNCTUATOR && p->token->value.character == c;
   return result;
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
nextCharPunctuator(Parser* p, char c) {
   if (peekCharPunctuator(p, c)) {
      return nextToken(p);
   }
   else {
      return NULL;
   }
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

// ==== Expressions ====

// Forward declaration of parseExpression.
AstNode* parseExpression(Parser* p);

AstNode*
primaryExpr(Parser* p) {
   AstNode* t = NULL;
   Token* tok = nextToken(p);
   if (!tok) { return false; }
   if (tok->type == TokenType_NUMBER) {
      t = newNode(p->arena);
      t->val = Ast_NUMBER;
      t->tok = tok;
   }
   // TODO: There are more primary expression terminals.
   else {

   }
   return t;
}

AstNode*
postfixExpr(Parser* p) {
   AstNode* t = NULL;
   if ((t = primaryExpr(p), t)) {
      Token* tok = nextToken(p);
      if (tok && tok->value.character == '[') {
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
   // Token* tok = p->token;
   AstNode* left = castExpr(p);
   //AstNode* t1 = multiplicativeExprPrime(p);
   if (left) {
      while (peekCharPunctuator(p, '*') || peekCharPunctuator(p, '/')) {
         // Pop the *
         Token* optok = nextToken(p);
         // Another cast expression
         AstNode* right = castExpr(p);
         if (right) {
            int node_type = optok->value.character == '*' ? Ast_MUL : Ast_DIV;
            left = makeAstNode(p->arena, node_type, left, right);
         } else {
            parseError("Expected expression after '*'");
         }
      }

   }
   return left;
}

AstNode*
additiveExpr(Parser* p) {
   // Token *tok = p->token;
   AstNode* left = multiplicativeExpr(p);
   if (left) {
      while (peekCharPunctuator(p, '+') || peekCharPunctuator(p, '-')) {
         // Pop the + or the -
         Token* optok = nextToken(p);
         // Pop another multiplicative expression.
         AstNode* right = multiplicativeExpr(p);
         if (right) {
            int node_type = optok->value.character == '+' ? Ast_ADD : Ast_SUB;
            left = makeAstNode(p->arena, node_type, left, right);
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
   AstNode* left = inclusiveOrExpr(p);
   if (left) {
      while (p->token->type == TokenType_PUNCTUATOR && p->token->value.character == LOGICAL_AND) {
         nextToken(p);  // Pop the logical and.
         AstNode* right = inclusiveOrExpr(p);
         if (!right) { parseError("Expected expression after `&&`."); }
         left = makeAstNode(p->arena, Ast_LOGICAL_AND, left, right);
      }
   }
   return left;
}

AstNode*
logicalOrExpr(Parser* p) {
   AstNode* left = logicalAndExpr(p);
   if (left) {
      while (p->token->type == TokenType_PUNCTUATOR && p->token->value.character == LOGICAL_OR) {
         nextToken(p); // Pop the logical or
         AstNode* right = logicalAndExpr(p);
         if (!right) { parseError("Expected expression after `||`"); }
         int node_type = Ast_LOGICAL_OR;
         left = makeAstNode(p->arena, node_type, left, right);
      }
   }
   return left;
}

AstNode*
conditionalExpr(Parser* p) {
   AstNode* t = logicalOrExpr(p);
   Token* bt = marktrack(p);
   if (nextCharPunctuator(p, '?')) {
      AstNode* then_expr = parseExpression(p);
      if (!then_expr) {
         parseError("Expected expression after ? token.");
      }
      else {
         bt = marktrack(p);
         if (!nextCharPunctuator(p, ':')) {
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
parseExpression(Parser* p) {
   AstNode* t = additiveExpr(p);
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

   if (t->type == TokenType_KEYWORD) {

      result = makeAstNode(p->arena, Ast_KEYWORD, NULL, NULL);
      int v = t->value.integer;
      // Storage class specifiers
      if (   v == Keyword_typedef
             || v == Keyword_extern
             || v == Keyword_static
             || v == Keyword_auto
             || v == Keyword_register) {

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
   if (t->type == TokenType_ID) {
      r = makeAstNode(p->arena, Ast_ID, NULL, NULL);
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
   AstNode* node = assignmentExpr(p);
   return node;
}

AstNode*
parseDeclaration(Parser* p) {
   AstNode* result = NULL;
   AstNode* specifiers = parseDeclarationSpecifiers(p);
   if (specifiers) {
      AstNode* identifier = parseDeclarator(p);
      result = identifier;

      Token* bt = marktrack(p);
      if (nextCharPunctuator(p, '=')) {
         AstNode* initializer = parseInitializer(p);
         result = initializer;
      }
      else {
         backtrack(p, bt);
      }
      if (!nextCharPunctuator(p, ';')) {
         parseError("Expected semicolon at the end of declaration.");
      }
   }
   return result;
}

// ==== Statements and blocks ====

AstNode*
parseStatement(Parser* p) {
   AstNode* result = NULL;
   result = parseExpression(p);
   nextCharPunctuator(p, ';');
   return result;
}

#define THIS_IS_MY_DEFINE "Hello there!"

AstNode*
parseCompoundStatement(Parser* p) {
   if (nextCharPunctuator(p, '{')) {
      AstNode* block_elem = NULL;
      do {
         Token* bt = marktrack(p);
         block_elem = parseDeclaration(p);
         if (!block_elem) {
            backtrack(p, bt);
            block_elem = parseStatement(p);
            if (!block_elem) {
               backtrack(p, bt);
            }
         }
      } while (block_elem);
      if (nextCharPunctuator(p, '}')) {

      } else {
         parseError("Expected '}' at the end of compound statement.");
      }
   } else {
      parseError("Expected '{'");
   }
   return NULL;
}

AstNode*
parseTranslationUnit(Parser* p) {
   // How to parse a function declaration
   // 1. declaration specifier
   // 2. declarator
   // 3. declaration list (one or more declarations)
   // 4. compound statement
   AstNode* result = NULL;

   AstNode* declaration_specifier = NULL;
   AstNode* declarator = NULL;

   if ((declaration_specifier = parseDeclarationSpecifiers(p), declaration_specifier) &&
       (declarator = parseDeclarator(p), declarator)) {
      AstNode* funcdef = makeAstNode(p->arena, Ast_FUNCDEF, declaration_specifier, declarator);

      // TODO: Support something other than no-params
      nextCharPunctuator(p, '(');
      nextCharPunctuator(p, ')');

      Token* bt = marktrack(p);

      AstNode* declaration_list = parseDeclarationList(p);
      if (!declaration_list) {
         backtrack(p, bt);
      }
      parseCompoundStatement(p);

      result = funcdef;
   }
   return result;

}
