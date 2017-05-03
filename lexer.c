char* g_keywords[] = {
#define X(keyword) #keyword,
     #include "keywords.inl"
#undef X
};

typedef enum TokenType_n {
    TokenType_NONE,
    TokenType_PUNCTUATOR,
    TokenType_PUNCTUATOR_MULTICHAR,
    TokenType_STRING_LITERAL,
    TokenType_NUMBER,
    TokenType_ID,
    TokenType_KEYWORD,
} TokenType;

typedef enum Punctuator_n {
    Punctuator_BEGIN = /*...*/  128,  // ASCII codes are reserved for single-char tokens.
    #define X(op, name) name,
        #include "punctuators.inl"
    #undef X
    Punctuator_END,
} Punctuator;

char* g_punctuator_strings[] = {
    #define X(op, name) #op,
        #include "punctuators.inl"
    #undef X
};

size_t
indexOfPunctuator(Punctuator p) {
    size_t idx = p - 1 - Punctuator_BEGIN;
    return idx;
}

typedef struct Token_s Token;
struct Token_s {
    TokenType type;
    union {
        u8    character;
        char* string;
        int   integer;
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
    if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
        return true;
    }
    return false;
}

// Returns 0 if it is not a punctuator. Otherwise, it returns the token code of the mult-char punctuator.
int
isPunctuator(Buffer* b) {
    int result = 0;
    char c = *b->current;

    // Check for single-char punctuators.
    b32 is_punctuator =
        c == ';' || c == '[' || c == ']' || c == '{' || c == '}' ||
        c == '(' || c == ')' || c == '*' || c == ':' || c == '.' ||
        c == '^' || c == '/' || c == '%' || c == '<' || c == '>' ||
        c == '|' || c == '?' || c == '=' || c == '~' || c == '!' ||
        c == ',' || c == '&' || c == '+' || c == '-';

    if (is_punctuator) {
        result = c;
    }

    // Now check for multi-char punctuators.
    char c1 = *(b->current + 1);
    char c2 = *(b->current + 2);
    for (Punctuator i = Punctuator_BEGIN + 1; i < Punctuator_END; ++i) {
        char* str = g_punctuator_strings[indexOfPunctuator(i)];
        size_t op_len = strlen(str);
        if (c == str[0]) {
            if (op_len == 2) {
                if (c1 == str[1]) {
                    result = i;
                    break;
                }
            }
            else if (op_len == 3) {
                if (c1 == str[1] && c2 == str[2]) {
                    result = i;
                    break;
                }
            }
            else {
                INVALID_CODE_PATH;
            }
        }
    }

    return result;
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
        c == '0' || c == '1' || c == '2' || c == '3' || c == '4' ||
        c == '5' || c == '6' || c == '7' || c == '8' || c == '9';
    return is_digit;
}

b32
isKeyword(Buffer* b) {
    b32 is_keyword = false;

    size_t num_keywords = ArrayCount(g_keywords);

    // Add a NULL terminator so that the buffer can be used as a string.

    char end_char = *b->end;
    *b->end = '\0';

    for (size_t i = 0; i < num_keywords; ++i) {
        if (stringsAreEqual(g_keywords[i], b->current)) {
            is_keyword = true;
            break;
        }
    }

    *b->end = end_char;

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

    // TODO - There is a specified ordering to parsing.. Keywords come
    // first, then identifiers... Set the correct order, or prove that
    // this is equivalent.

    // If it starts with a digit, it's numerical.
    if (isDigit(*b->current)) {
        type = TokenType_NUMBER;
        // TODO. Different kinds of numbers..
    }
    else if (isKeyword(b)) {
        // TODO: Keywords
        type = TokenType_KEYWORD;
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
    else if (isPunctuator(buffer)) {
        int punctuator_token = isPunctuator(buffer);
        if (punctuator_token && punctuator_token < ASCII_MAX) {
            t.type = TokenType_PUNCTUATOR;
            t.value.character = *buffer->current++;
        }
        else {
            t.type = TokenType_PUNCTUATOR_MULTICHAR;
            Assert(punctuator_token < 255);
            t.value.character = (u8)punctuator_token;
            // Advance the buffer by the length of othe operator.
            char* punctuator_str = g_punctuator_strings[indexOfPunctuator(punctuator_token)];
            size_t len = strlen(punctuator_str);
            buffer->current += len;
        }
    }
    else if (*buffer->current == '\"') {
        // We are inside a string. Parse until we get the end of the string.
        // TODO: Escape characters.
        t.type = TokenType_STRING_LITERAL;
        Buffer token_buffer = {0};
        ++buffer->current;
        token_buffer.current = buffer->current;
        while (*buffer->current++ != '\"') {
            if (buffer->current > buffer->end) {
                // TODO: File parsing information.
                lexerError("Expected \" while parsing string literal.");
            }
        }
        token_buffer.end = buffer->current - 1;
        char* str = getStringFromBuffer(a, &token_buffer);
        t.value.string = str;
    }
    // TODO: Operators
    // TODO: Constants
    // The rest are keywords and identifiers.
    else {
        Buffer token_buffer = {0};
        token_buffer.current = buffer->current;
        //char* start = buffer;
        while (!(isWhitespace(*buffer->current) || isPunctuator(buffer))) {
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
            if (err != SUCCESS) {
                char msg[1024] = {0};
                sprintf_s(msg, 1024, "Error while parsing integer. Token string %s", str);
                lexerError(msg);
            }
            t.value.integer = val;
        } else {
            t.value.string = str;
        }
    }
    return t;
}

void
tokenPrint(Token* token) {
    printf("[");
    switch(token->type) {
        case TokenType_PUNCTUATOR: {
            printf("PUNCTUATOR %c", (char)token->value.character);
        } break;
        case TokenType_PUNCTUATOR_MULTICHAR: {
            printf("PUNCTUATOR: %s", g_punctuator_strings[indexOfPunctuator(token->value.character)]);
        } break;
        case TokenType_NUMBER: {
            // TODO: Support floating point..
            printf("NUMBER: %i", token->value.integer);
        } break;
        case TokenType_ID: {
            printf("ID: %s", token->value.string);
        } break;
        case TokenType_STRING_LITERAL: {
            printf("STRING: \"%s\"", token->value.string);
        } break;
            /*case TokenType_KEYWORD: {
            printf("KEYWORD: %s", token->value.string);
            } break;*/
        default: {

            printf("This token type is not printable yet. Type %d ", token->type);
        } break;
    }
    printf("]\n");
}

Token*
tokenize(Arena* a, char* buffer, size_t buffer_len) {
    Buffer b = {0};
    b.current = buffer;
    b.end = buffer + buffer_len;

    Token* t = AllocType(a, Token);
    Token* tokens = t;
    while (b.current < b.end) {
        *t = getToken(a, &b);
        if (t->type != TokenType_NONE) {
            t->next = AllocType(a, Token);
            t = t->next;
        }
    }

    return tokens;
}
