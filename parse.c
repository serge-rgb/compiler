
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

static Token sentinel;
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
    if (t && p->token->type == TokenType_NONE) {
        printf("Expression accepted\n");
    } else {
        printf("Could not accept expression. Ended at token: ");
        tokenPrint(p->token);
    }
    return t;
}


AstNode*
parseDeclarationSpecifiers(Parser* p) {
    // One or more of:
    //   storage-class-specifier
    //   type-specifier
    //   function specifier
    Token* t = p->token;
    if (t->type == TokenType_KEYWORD ) {

    }
    return NULL;
}

AstNode*
parseDeclarator(Parser* p) {
    return NULL;
}

AstNode*
parseTranslationUnit(Parser* p) {
    // How to parse a function declaration
    // 1. declaration specifier
    // 2. declarator
    // 3. declaration list (one or more declarations)
    // 4. compound statement
    AstNode* declaration_specifier = NULL;
    AstNode* declarator = NULL;
    if ((declaration_specifier = parseDeclarationSpecifiers(p), declaration_specifier) &&
        (declarator = parseDeclarator(p), declarator)
        ) {

    }
    return NULL;

}
