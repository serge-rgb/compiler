
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

Token*
nextToken(Parser* p) {
    Token* result = NULL;
    if (p->token) {
        result = p->token;
        p->token = p->token->next;
    }
    return result;
}

void
backtrack(Parser* p, Token* t) {
    p->token = t;
}


// ==== Expressions ====

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

static
b32
peekCharPunctuator(Parser* p, char c) {
    b32 result = p->token->type == TokenType_PUNCTUATOR && p->token->value.character == c;
    return result;
}

static
Token*
nextCharPunctuator(Parser* p, char c) {
    if (peekCharPunctuator(p, c)) {
        return nextToken(p);
    }
    else {
        return NULL;
    }
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
    Token* bt = p->token;
    Token* t = nextToken(p);
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
parseDeclaration(Parser* p) {
    return NULL;
}

// ==== Statements and blocks ====

AstNode*
parseStatement(Parser* p) {
    AstNode* result = NULL;
    result = parseExpression(p);
    nextCharPunctuator(p, ';');
    return result;
}

AstNode*
parseCompoundStatement(Parser* p) {
    if (nextCharPunctuator(p, '{')) {
        AstNode* block_elem = NULL;
        do {
            Token* bt = p->token;
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

    if ((declaration_specifier = parseDeclarationSpecifiers(p), declaration_specifier)
        && (declarator = parseDeclarator(p), declarator)) {
        AstNode* funcdef = makeAstNode(p->arena, Ast_FUNCDEF, declaration_specifier, declarator);

        // TODO: Support something other than no-params
        nextCharPunctuator(p, '(');
        nextCharPunctuator(p, ')');

        Token* bt = p->token;

        AstNode* declaration_list = parseDeclarationList(p);
        if (!declaration_list) {
            backtrack(p, bt);
        }
        parseCompoundStatement(p);
        result = funcdef;
    }
    return result;

}
