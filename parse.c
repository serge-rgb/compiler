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

Tree*
primaryExpr(Parser* p) {
    Tree* t = NULL;
    Token* tok = nextToken(p);
    if (!tok) { return false; }
    if (tok->type == TokenType_NUMBER) {
        t = newNode(p->arena);
    }
    // TODO: There are more primary expression terminals.
    else {

    }
    return t;
}

Tree*
postfixExpr(Parser* p) {
    Tree* t = NULL;
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

Tree*
unaryExpr(Parser* p) {
    Tree* t = NULL;
    t = postfixExpr(p);
    return t;
}

Tree*
castExpr(Parser* p) {
    Tree* t = NULL;
    t = unaryExpr(p);
    return t;
}


Tree*
multiplicativeExprPrime(Parser* p) {
    Tree* t0 = NULL;
    Tree* t1 = NULL;
    Token* tok = p->token;
    if (!tok) { return NULL; }
    if (nextToken(p)->value.character == '*'
        && (t0 = castExpr(p), t0)
        && (t1 = multiplicativeExprPrime(p)), t1) {
        return ast3(AstNode_MUL, t0, t1);
    }
    backtrack(p, tok);
    return AstNode_EPSILON;
}

b32
multiplicativeExpr(Parser* p) {
    Token* tok = p->token;
    AstNode* t0 = castExpr(p);
    AstNode* t1 = multiplicativeExprPrime(p);
    if (t0 /*&& multiplicativeExprPrime(p)*/) {
        if (t == AstNode_EPSILON) {
            return t0;
        }

        return true;
    }
    return false;
}

b32
additiveExprPrime(Parser* p) {
    Token* tok = p->token;
    if (tok && nextToken(p)->value.character == '+'
        && multiplicativeExpr(p)
        && additiveExprPrime(p)) {
        return true;
    }
    backtrack(p, tok);
    return true;
}

b32
additiveExpr(Parser* p) {
    Token *tok = p->token;
    AstNode* left = NULL;
    if ((left = multiplicativeExpr(p), left)) {
        while (p->token->TokenType_PUNCTUATOR && p->token.value.character == '+') {
            // Pop the +
            nextToken(p);
            // Pop another multiplicative expression.
            multiplicativeExpr(p);
            // Look ahead, if there's another plus in the horizon, make a new tree. Otherwise finish this one.

        }

        && additiveExprPrime(p)) {
        return true;
    }
    return false;
}

void
parseExpression(Token* token) {
    Parser p = {0};
    Arena tmp_parser_arena = {0};
    p.arena = &tmp_parser_arena;
    p.token = token;
    b32 res = additiveExpr(&p);
    if (res && p.token->type == TokenType_NONE) {
        printf("Expression accepted\n");
    }
}
/*
void
parseTranslationUnit(void) {
    // Let's start by parsing an expression.
    parseExpression();
}
*/
