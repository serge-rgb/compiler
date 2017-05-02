Token* g_parse_token;

void
parseError(char* msg) {
    fprintf(stderr, "Parse error: %s\n", msg);
    exit(1);
}

Token*
nextToken(void) {
    Token* result = g_parse_token;
    if (g_parse_token) {
        g_parse_token = g_parse_token->next;
    }
    return result;
}

void
parseExpression(void) {
    Token* t = nextToken();
    // TODO: Literals, Constants, Identifiers, other expressions.
    if (t->type == TokenType_NUMBER) {
        int vala = t->value.integer;
        t = nextToken();
        if (t->type == TokenType_PUNCTUATOR) {
            t = nextToken();
            if (t->type == TokenType_NUMBER) {
                int valb = t->value.integer;
                char buffer[128] = {0};
                sprintf_s(buffer, 128, "mov rax, %d\n", vala);
                codegenEmit(buffer);
                sprintf_s(buffer, 128, "add rax, %d\n", valb);
                codegenEmit(buffer);
            }
            else {
                parseError("Expected another integer in sum.");
            }

        } else {
            parseError("Expected operator in expression.");
        }
    } else {
        parseError("Expected number.");
    }
}

void
parseTranslationUnit(void) {
    // Let's start by parsing an expression.
    parseExpression();
}
