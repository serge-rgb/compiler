typedef enum TokenType_n {
    TokenType_NONE,
    TokenType_PUNCTUATOR,
    TokenType_STRING_LITERAL,
    TokenType_NUMBER,
    TokenType_ID,
} TokenType;

typedef struct Token_s Token;
struct Token_s {
    TokenType type;
    union {
        char  character;
        char* string;
    } value;

    Token*    next;
};

void
lexerError(char* msg) {
    fprintf(stderr, "Syntax error: %s\n", msg);
    exit(1);
}

b32
isWhitespace(char c) {
    if (   c == ' '
           || c == '\t'
           || c == '\n'
           || c == '\r'
           ) {
        return true;
    }
    return false;
}

b32
isPunctuator(char c) {
    b32 is_punctuator =
        c == ';' ||
        c == '[' ||
        c == ']' ||
        c == '{' ||
        c == '}' ||
        c == '(' ||
        c == ')' ||
        c == '*' ||
        c == ':' ||
        c == ',';
    // TODO: ... is a three-character punctuator
    return is_punctuator;
}

void
skipWhitespace(Buffer* b) {
    while (isWhitespace(*b->current)) {
        ++b->current;
    }
}

b32
isDigit(char c) {
    b32 is_digit =
        c == '0' ||
        c == '1' ||
        c == '2' ||
        c == '3' ||
        c == '4' ||
        c == '5' ||
        c == '6' ||
        c == '7' ||
        c == '8' ||
        c == '9';
    return is_digit;
}

b32 isKeyword(Buffer* b) {
    b32 is_keyword = false;
    return is_keyword;
}

ErrorCode
parseInt(char* str, int* out_int) {
    ErrorCode result = SUCCESS;
    if (!out_int) {
        return ERROR_PARSE_INT;
    }
    int val = 0;
    while (*str != '\0') {
        char c = *str;
        if (c-'0' < 0 || c-'0' > 9) {
            result = ERROR_PARSE_INT;
            break;
        }
        val = (val * 10) + c-'0';
        str++;
    }
    *out_int = val;
    return result;
}

TokenType
identifyToken(Buffer* b) {
    TokenType type = TokenType_NONE;
    // If it starts with a digit, it's numerical.
    if (0) {}
    else if (isDigit(*b->current)) {
        type = TokenType_NUMBER;
    }
    else if (isKeyword(b)) {
        // TODO: Keywords
    }
    else {
        type = TokenType_ID;
    }
    return type;
}

Token
getToken(Arena* a, Buffer* buffer) {
    Token t = {0};
    skipWhitespace(buffer);
    if (buffer->current >= buffer->end) {
        return t;
    }
    else if (isPunctuator(*buffer->current)) {
        t.type = TokenType_PUNCTUATOR;
        t.value.character = *buffer->current++;
    }
    else if (*buffer->current == '\"') {
        // We are inside a string. Parse until we get the end of the string.
        // TODO: Escape characters.
        t.type = TokenType_STRING_LITERAL;
        ++buffer->current;
        while (*buffer->current++ != '\"') {
            if (buffer->current > buffer->end) {
                // TODO: File parsing information.
                lexerError("Expected \" while parsing string literal.");
            }
        }
        t.value.character = 'S';
    }
    // TODO: Operators
    // TODO: Constants
    // The rest are keywords and identifiers.
    else {
        Buffer token_buffer = {0};
        token_buffer.current = buffer->current;
        //char* start = buffer;
        while (!(isWhitespace(*buffer->current) || isPunctuator(*buffer->current))) {
            ++buffer->current;

            if (buffer->current >= buffer->end) {
                // TODO: File parsing information.
                break;
            }
        }
        token_buffer.end = buffer->current;
        t.type = identifyToken(&token_buffer);

        t.value.character = *token_buffer.current;
        char* str = getStringFromBuffer(a, &token_buffer);
        if (t.type == TokenType_NUMBER) {
            int val = 0;
            ErrorCode err = parseInt(str, &val);
            if (err == SUCCESS)
                printf("parsed int with value %d\n", val);
            else
                printf("Int parsing failed.\n");

        }
        t.value.string = str;
    }
    return t;
}

Token*
tokenize(Arena* a, char* buffer, size_t buffer_len) {
    // Munch until whitespace
    // Check if it's a keyword
    // Else, it's an identifier

    Buffer b = {0};
    b.current = buffer;
    b.end = buffer + buffer_len;

    Token* t = AllocType(a, Token);
    Token* tokens = t;
    while (b.current < b.end) {
        *t = getToken(a, &b);
        if (t->type != TokenType_NONE) {
            t->next = AllocType(a, Token);
            if (t->type == TokenType_PUNCTUATOR)
                printf("Token of type %d is %c\n", t->type, t->value.character);
            else if (t->type == TokenType_ID)
                printf("Token of type %d is %s\n", t->type, t->value.string);
            else if (t->type == TokenType_STRING_LITERAL)
                printf("Token of type %d is STRING\n", t->type);
            else
                printf("Token of type %d is %c\n", t->type, t->value.character);
            t = t->next;
        }
    }

    printf("\n");
    return tokens;
}
