
// Forward declaration. Defined in codegen.c
typedef struct RegisterValue_s RegisterValue;


#define CTYPE_HASHMAP_SIZE 128
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

   Break;

   fprintf(stderr, "Syntax error: %s\n", buffer);
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
   while (left) {
      AstNode* right = NULL;

      if (nextPunctuator(p, '[')) {
         NotImplemented("Array postfix expression");
      }
      else if (nextPunctuator(p, '(')) {
         if (nextPunctuator(p, ')')) {
            right = left;
            left = makeAstNode(p->arena, Ast_FUNCCALL, right, NULL);
         }
         else {
            AstNode* params = argumentExpressionList(p);
            if (nextPunctuator(p, ')')) {
               right = left;
               left = makeAstNode(p->arena, Ast_FUNCCALL, right, params);
            }
            else {
               parseError("Expected ) in function call.");
            }
         }
      }
      else if (nextPunctuator(p, '.')) {
         NotImplemented("Dot postfix expression");
      }
      else if (nextPunctuator(p, ARROW)) {
         NotImplemented("Arrow postfix expression");
      }
      else if (nextPunctuator(p, INCREMENT)) {
         NotImplemented("Increment postfix expression");
      }
      else if (nextPunctuator(p, DECREMENT)) {
         NotImplemented("Decrement postfix expression");
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

   AstNode* t = postfixExpr(p);
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
            int   node_type = optok->cast.character == '+' ? Ast_ADD : Ast_SUB;
            left            = makeAstNode(p->arena, node_type, left, right);
         } else {
            parseError("Expected expression after '+'");
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
         if (!right) { parseError("Expected expression after relational operator."); }
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
         left = makeAstNode(p->arena, t, left, right);
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
         if (!right) { parseError("Expected expression after equality operator."); }

         left = makeAstNode(p->arena, eq->value == EQUALS? Ast_EQUALS : Ast_NOT_EQUALS, left, right);
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
      while (peekPunctuator(p, LOGICAL_OR)) {
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
   if ((unary = unaryExpr(p)) && (op = assignmentOperator(p)) &&  (assignment = assignmentExpression(p))) {
      t = makeAstNode(p->arena, Ast_ASSIGN_EXPR, unary, assignment);
      t->tok = op;
   }
   else {
      backtrack(p, bt);
      t = conditionalExpr(p);
   }

   return t;
}

AstNode*
argumentExpressionList(Parser* p) {
   AstNode* args = NULL;
   while (true) {
      AstNode* assignment = assignmentExpression(p);
      if (!assignment) {
         parseError("Expected argument in expression list");
      }
      else {
         assignment->sibling = args;
         args = assignment;
         if (peekPunctuator(p, ',')) {
            nextToken(p);
         }
         else {
            break;
         }
      }
   }
   return args;
}



AstNode*
parseExpression(Parser* p) {
   AstNode* t = assignmentExpression(p);
   return t;
}

// ==== Declarations ====

Ctype
parseCtypeSpecifier(Token* t) {
   Ctype result = Type_NONE;
   b32 is_type_spec =
           t->value == Keyword_int ||
           t->value == Keyword_char ||
           t->value == Keyword_float ||
           t->value == Keyword_long ||
           t->value == Keyword_short ||
           t->value == Keyword__Bool ||
           t->value == Keyword__Complex ||
           t->value == Keyword__Imaginary
           ;
   if (is_type_spec) {
      switch (t->value) {
         case Keyword_int: {
            result = Type_INT;
         } break;
         case Keyword_char: {
            result = Type_CHAR;
         } break;
         case Keyword_float: {

         } //break;
         case Keyword_long: {

         } //break;
         case Keyword_short: {

         } //break;
         case Keyword__Bool: {

         } //break;
         case Keyword__Complex: {

         } //break;
         case Keyword__Imaginary: {
            NotImplemented("Type Specifier.");
         } //break;
      }
   }
   return result;
}

AstNode*
parseDeclarationSpecifiers(Parser* p) {
   // One or more of:
   //   storage-class-specifier
   //   type-specifier
   //   function specifier
   // Token* bt = t;
   AstNode* result = NULL;
   Ctype ctype = Type_NONE;
   i32 line_number = p->token->line_number;

#define MaxSpecifiers 1
   int storage_spec[MaxSpecifiers] = Zero;
   int n_storage_spec = 0;
   int type_qualifiers[MaxSpecifiers] = Zero;
   int n_type_qualifiers = 0;

   while (p->token->type == TType_KEYWORD) {
      Token* t = nextToken(p);
      int v = t->cast.integer;
      // Storage class specifiers
      if (v == Keyword_typedef ||
          v == Keyword_extern ||
          v == Keyword_static ||
          v == Keyword_auto ||
          v == Keyword_register) {
         ArrayPush(storage_spec, v);
      }
      // Ctype specifiers
      else if ((ctype = parseCtypeSpecifier(t), ctype)) {
      }
      else if (v == Keyword_const ||
               v == Keyword_restrict ||
               v == Keyword_volatile) {
         ArrayPush(type_qualifiers, v);
      }
      else if (v == Keyword_struct ||
               v == Keyword_union) {
         NotImplemented("Struct / union specifier");
      }
      else {
         backtrack(p, t);
         break;
      }
   }
   if (ctype != Type_NONE) {
      result = makeAstNodeWithLineNumber(p->arena, Ast_DECLARATION_SPECIFIER, NULL, NULL, line_number);
      result->ctype = ctype;
   }

   return result;
}

// Forward declaration to fix circular dependency with parseDeclarator
AstNode* parseDeclarator(Parser* p);

AstNode*
parameterTypeList(Parser* p) {
   AstNode* result = NULL;

   // Parse parameter, right to left.
   while (true) {
      AstNode* decl_spec = parseOrBacktrack(parseDeclarationSpecifiers, p);
      if (decl_spec) {
         AstNode* declarator = parseOrBacktrack(parseDeclarator, p);
         if (declarator) {
            AstNode* new = makeAstNode(p->arena, Ast_PARAMETER, decl_spec, declarator);
            new->sibling = result;
            result = new;

            if (peekPunctuator(p, ',')) {
               // There is another parameter.
               nextToken(p);
            } else {
               break;
            }
         }
      }
      else {
         break; // Empty param list
      }
   }

   return result;
}

AstNode*
parseDeclarator(Parser* p) {
   AstNode* r = NULL;
   Token* t;
   if (nextPunctuator(p, '(')) {
      parseError("Function declarators not supported yet");
   }
   else if (nextPunctuator(p, '*')) {
      parseError("pointer declarators not supported yet");
   }
   else if ((t = nextToken(p)) && (t->type == TType_ID)) {

      AstNode* id = makeAstNodeWithLineNumber(p->arena, Ast_ID, NULL, NULL, t->line_number);
      id->tok = t;
      if (nextPunctuator(p, '(')) {
         AstNode* params = parseOrBacktrack(parameterTypeList, p);
         if (params) {
            id->sibling = params;
         }
         if (!nextPunctuator(p, ')')) {
            parseError("Expected ) in declarator");
         }
      }
      r = makeAstNodeWithLineNumber(p->arena, Ast_DECLARATOR, id, NULL, t->line_number);
   }
   else {
      parseError("unhandled error");
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

      Token* bt = marktrack(p);
      if (nextPunctuator(p, '=')) {
         initializer = parseInitializer(p);
      }
      else {
         backtrack(p, bt);
      }
      expectPunctuator(p, ';');

      result = makeAstNode(p->arena, Ast_DECLARATION, specifiers, declarator);
      declarator->sibling = initializer;
   }
   return result;
}

// ==== Statements and blocks ====

AstNode*
parseJumpStatement(Parser* p) {
   AstNode* stmt = NULL;
   Token* t = nextToken(p);
   if (t->type == TType_KEYWORD && t->value == Keyword_return) {
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
         if (!*cur) {
            if (nextPunctuator(p, '}')) {
               compound_stmt = makeAstNode(p->arena, Ast_COMPOUND_STMT, first_stmt, NULL);
            }
         }
      } while (*cur);
   } else {
      backtrack(p, tok);
   }

   return compound_stmt;
}

AstNode*
parseStatement(Parser* p) {
   AstNode* stmt = NULL;
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
            if (nextKeyword(p, Keyword_else)) {
               AstNode* els = parseStatement(p);
               then->sibling = els;
            }
         } else {
            parseError("Expected else clause after if.");
         }
      }
   }
   // === Iteration ===
   else if (nextKeyword(p, Keyword_while)) {
      if (nextPunctuator(p, '(')) {
         AstNode* expr = parseExpression(p);
         if (!expr) {
            parseError("expected expression inside parens");
         }
         if (!nextPunctuator(p, ')')) {
            parseError("Expected ) after while");
         }
         else {
            // TODO: Parse assignment expression
            AstNode* statement = parseStatement(p);
            if (!statement) {
               parseError("expected statement after while");
            }
            AstNode* declarations = makeAstNode(p->arena, Ast_NONE, 0,0);
            AstNode* control_before = expr;
            AstNode* after = makeAstNode(p->arena, Ast_NONE, 0,0);
            AstNode* body = statement;

            declarations->sibling = control_before;
            control_before->sibling = after;
            after->sibling = body;
            body->sibling = NULL;

            stmt = makeAstNode(p->arena, Ast_ITERATION, declarations, NULL);
         }
      }
   }
   else if (nextKeyword(p, Keyword_for)) {
      NotImplemented("For loop");
   }
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
