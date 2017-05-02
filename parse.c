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
        t = nextToken();
        if (t->type == TokenType_PUNCTUATOR) {

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
