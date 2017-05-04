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

b32
primaryExpr(Parser* p) {
    Token* t = nextToken(p);
    if (!t) { return false; }
    if (t->type == TokenType_NUMBER) {
        p->tree = newNode(p->arena);
        return true;
    }
    // TODO: There are more primary expression terminals.
    else {

    }
    return false;
}

b32
postfixExpr(Parser* p) {
    if (primaryExpr(p)) {
        Token* t = nextToken(p);
        if (t && t->value.character == '[') {
            parseError ("I don't know how to continue");
        }
        else if (t) {
            backtrack(p, t);
            return true;
        }
    }
    return false;
}

b32
unaryExpr(Parser* p) {
    if (postfixExpr(p)) {
        return true;
    }
    return false;
}

b32
castExpr(Parser* p) {
    if (unaryExpr(p)) {
        return true;
    }
    return false;
}


b32
multiplicativeExprPrime(Parser* p) {
    Token* t = p->token;
    if (!t) { return false; }
    if (nextToken(p)->value.character == '*'
        && castExpr(p)
        && multiplicativeExprPrime(p)) {
        return true;
    }
    backtrack(p, t);
    return true;
}

b32
multiplicativeExpr(Parser* p) {
    Token* t = p->token;
    if (castExpr(p) && multiplicativeExprPrime(p)) {
        return true;
    }
    return false;
}

b32
additiveExprPrime(Parser* p) {
    Token* t = p->token;
    if (t && nextToken(p)->value.character == '+'
        && multiplicativeExpr(p)
        && additiveExprPrime(p)) {
        return true;
    }
    backtrack(p, t);
    return true;
}

b32
additiveExpr(Parser* p) {
    Token *t = p->token;
    if (multiplicativeExpr(p)
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
