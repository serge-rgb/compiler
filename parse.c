
// Forward declaration. Defined in codegen.c
typedef struct RegisterValue_s RegisterValue;

typedef struct SymEntry_s {
   Ctype ctype;
   RegisterValue* regval;
} SymEntry;

#define HashmapName     SymTable
#define HashmapPrefix   sym
#define HashmapKey      char*
#define HashmapValue    SymEntry
#define HashFunction    hashStrPtr
#define KeyCompareFunc  compareStringKey
#include "hashmap.inl"

#define CTYPE_HASHMAP_SIZE 128
typedef struct Parser_s {
   Token* token;  // The next token to parse.
   Arena* arena;
   AstNode* tree;
   char* file_name;
   SymTable symbol_table;
} Parser;

// A function that takes a Parser and returns an AstNode*
typedef AstNode* ParseFunction(Parser*);

AstNode*
newNode(Arena* a) {
   AstNode* r = AllocType(a, AstNode);
   return r;
}

void
setTypeForId(Parser* p, char* id, Ctype* ctype) {
   SymEntry e = { .ctype = *ctype, .regval = NULL };
   symInsert(&p->symbol_table, id, e);
}

void
parseError(char* msg, ...) {
   va_list args;
   va_start(args, msg);
   char buffer[LINE_MAX] = {0};
   vsnprintf(buffer, LINE_MAX, msg, args);

   BreakHere;

   fprintf(stderr, "Syntax error: %s\n", buffer);
   va_end(args);
   exit(1);
}

b32
peekPunctuator(Parser* p, int c) {
   b32 result = false;
   Assert(c < 256);
   result = p->token->type == TType_PUNCTUATOR && p->token->value.character == c;
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
   // TODO(long): other constants, string literals
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

AstNode*
postfixExpr(Parser* p) {
   AstNode* left = primaryExpr(p);
   while (left) {
      AstNode* right = NULL;

      if (nextPunctuator(p, '[')) {
         Assert(!"NOT IMPL");
      }
      else if (nextPunctuator(p, '(')) {
         if (nextPunctuator(p, ')')) {
            right = left;
            left = makeAstNode(p->arena, Ast_FUNCCALL, left, NULL);
         }
         else {
            parseError("Expected ) in function call.");
         }
      }
      else if (nextPunctuator(p, '.')) {
         Assert(!"NOT IMPL");
      }
      else if (nextPunctuator(p, ARROW)) {
         Assert(!"NOT IMPL");
      }
      else if (nextPunctuator(p, INCREMENT)) {
         Assert(!"NOT IMPL");
      }
      else if (nextPunctuator(p, DECREMENT)) {
         Assert(!"NOT IMPL");
      }
      if (!right) {
         break;
      }
   }
   return left;
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

/**
 * TODO(medium): Write documentation about the way we handle left recursion in the C grammar.
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
   // TODO(long): Implement bit or.
   return additiveExpr(p);
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

AstNode*
assignmentExpr(Parser* p) {
   AstNode* t = conditionalExpr(p);
   // TODO(long): unaryExpr assignmentOperator assignmentExpr
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
         // TODO(medium): Ast types should reuse keyword and punctuator values.
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
   // TODO(long): This left-recursion elimination pattern is repeating a lot.
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

Ctype*
parseCtypeSpecifier(Token* t) {
   Ctype* result = NULL;
   b32 is_type_spec =
           t->value.integer == Keyword_int ||
           t->value.integer == Keyword_char ||
           t->value.integer == Keyword_float ||
           t->value.integer == Keyword_long ||
           t->value.integer == Keyword_short ||
           t->value.integer == Keyword__Bool ||
           t->value.integer == Keyword__Complex ||
           t->value.integer == Keyword__Imaginary
           ;
   if (is_type_spec) {
      switch (t->value.integer) {
         case Keyword_int: {
            static Ctype type_int = {0};
            type_int.type = Type_INT;
            return &type_int;
         } break;
         case Keyword_char: {
            static Ctype type_char = {0};
            type_char.type = Type_CHAR;
            return &type_char;
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
            Assert(!"Ctype specifier not implemented.");
         } //break;
      }
   }
   // TODO(large): User defined types.
   return result;
}

AstNode*
parseDeclarationSpecifiers(Parser* p, Ctype** out_type) {
   // One or more of:
   //   storage-class-specifier
   //   type-specifier
   //   function specifier
   Token* t = nextToken(p);
   Token* bt = t;
   AstNode* result = NULL;
   Ctype* type = NULL;

   if (t->type == TType_KEYWORD) {

      result = makeAstNodeWithLineNumber(p->arena, Ast_TYPE_SPECIFIER, NULL, NULL, t->line_number);
      int v = t->value.integer;
      // Storage class specifiers
      if (v == Keyword_typedef ||
          v == Keyword_extern ||
          v == Keyword_static ||
          v == Keyword_auto ||
          v == Keyword_register) {

      }
      // Ctype specifiers
      else if ((type = parseCtypeSpecifier(t), type)) {
         Assert(!*out_type);
         *out_type = type;
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
   // TODO(long): Initializers.
   AstNode* node = assignmentExpr(p);
   return node;
}

AstNode*
parseDeclaration(Parser* p) {
   AstNode* result = NULL;
   Ctype* type = NULL;
   AstNode* specifiers = parseDeclarationSpecifiers(p, &type);
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

      AstNode* ast_type = makeAstNodeWithLineNumber(p->arena, Ast_TYPE, NULL, NULL, specifiers->line_number);
      ast_type->ctype = type;
      identifier->sibling = initializer;
      identifier->child = ast_type;
      setTypeForId(p, identifier->tok->value.string, ast_type->ctype);
      result = makeAstNode(p->arena, Ast_DECLARATION, identifier, NULL);
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
         if (!*cur) {
            if (nextPunctuator(p, '}')) {
               compound_stmt = makeAstNode(p->arena, Ast_COMPOUND_STMT, first_stmt, NULL);
            } else {
               parseError("Unrecognized token.");
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
   // TODO(medium): Parse the different kinds of statements in the correct order.
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
         // TODO(long): There should be checking of `if` conditions.
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
   // TODO(long): iteration-statement
   return stmt;
}

AstNode*
parseFunctionDefinition(Parser* p) {
   AstNode* result = NULL;

   AstNode* declaration_specifier = NULL;
   AstNode* declarator = NULL;

   Ctype* type = NULL;
   if ((declaration_specifier = parseDeclarationSpecifiers(p, &type)) != NULL &&
       (declarator = parseDeclarator(p)) != NULL) {

      AstNode* funcdef = makeAstNode(p->arena, Ast_FUNCDEF, declaration_specifier, declarator);

      // TODO(long): Support something other than no-params
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
