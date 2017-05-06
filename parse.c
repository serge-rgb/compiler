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
    if (left /*&& multiplicativeExprPrime(p)*/) {
        while (p->token->type == TokenType_PUNCTUATOR && p->token->value.character == '*') {
            // Pop the *
            nextToken(p);
            // Another cast expression
            AstNode* right = castExpr(p);
            if (right) {
                return makeAstNode(p->arena, Ast_MUL, left, right);
            } else {
                parseError("Expected expression after '*'");
            }
        }

        return left; // TODO: Build up the tree
    }
    return NULL;
}

static
b32
peekAddToken(Parser* p) {
    b32 r = p->token->type == TokenType_PUNCTUATOR && p->token->value.character == '+';
    return r;
}

AstNode*
additiveExpr(Parser* p) {
    // Token *tok = p->token;
    AstNode* left = NULL;
    if ((left = multiplicativeExpr(p), left)) {
        if (!peekAddToken(p)) {
            return left;
        }
        while (peekAddToken(p)) {
            // Pop the +
            nextToken(p);
            // Pop another multiplicative expression.
            AstNode* right = multiplicativeExpr(p);
            if (right) {
                return makeAstNode(p->arena, Ast_ADD, left, right);
            } else {
                parseError("Expected expression after '+'");
            }
        }
    }
    return false;
}

void
parseExpression(Token* token) {
    Parser p = {0};
    Arena tmp_parser_arena = {0};
    p.arena = &tmp_parser_arena;
    p.token = token;
    AstNode* t = additiveExpr(&p);
    if (t && p.token->type == TokenType_NONE) {
        printf("Expression accepted\n");
    } else {
        printf("Could not accept expression. Ended at stroke: ");
        tokenPrint(p.token);
    }

}
/*
void
parseTranslationUnit(void) {
    // Let's start by parsing an expression.
    parseExpression();
}
*/
