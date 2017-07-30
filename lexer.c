char* g_keywords[] = {
#define X(keyword) #keyword,
#include "keywords.inl"
#undef X
};

typedef enum Keyword_n {
#define X(keyword) Keyword_ ## keyword,
#include "keywords.inl"
#undef X
} Keyword;

typedef enum TokenType_n {
   TokenType_NONE,
   TokenType_PUNCTUATOR,
   TokenType_PUNCTUATOR_MULTICHAR,
   TokenType_STRING_LITERAL,
   TokenType_NUMBER,
   TokenType_ID,
   TokenType_KEYWORD = 0xF000,
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

   u64 line_number;

   Token*    next;
};

typedef struct FileStream_s {
   // TODO: Use a buffer to minimize calls to fwrite and friends.
   FILE* fd;
   u64 line_number;
} FileStream;

b32
fileStreamInit(FileStream* fs, char* fname) {
   FILE* fd = fopen(fname, "r");
   b32 result = false;
   if (fd) {
      fs->fd = fd;
      fs->line_number = 1;
      result = true;
   }
   return result;
}

b32
fileStreamHasContent(FileStream* fs) {
   b32 result = false;
   char buffer = 0;
   fread(&buffer, 1, 1, fs->fd);
   if (!feof(fs->fd)) {
      fseek(fs->fd, -1, SEEK_CUR);
      result = true;
   }
   return result;
}

char
fileStreamRead(FileStream* fs) {
   char result = 0;
   if (fileStreamHasContent(fs)) {
      size_t read = fread(&result, sizeof(char), 1, fs->fd);
      Assert(read);
      fs->line_number += (result == '\n');
   }
   else {
      Assert(!"We need to read from the file and fill the buffer again.");
   }
   return result;
}

char
fileStreamPeek(FileStream* fs) {
   char result;
   size_t read = fread(&result, sizeof(char), 1, fs->fd);
   Assert(read);
   fseek(fs->fd, -1, SEEK_CUR);
   return result;
}

void fileStreamClose(FileStream* fs) {
   if (fs->fd) {
      fclose(fs->fd);
   }
   else {
      fprintf(stderr, "WARNING: Trying to close NULL file descriptor.");
   }
}


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

// Returns 0 if it is not a punctuator. Otherwise, it returns the
// token code of the mult-char punctuator.
int
isPunctuator(FileStream* fs) {
   int result = 0;

   char c = fileStreamRead(fs);

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

   // TODO: Implement 2-char look-ahead in FileStream.

   // Now check for multi-char punctuators.
   char c1, c2;
   size_t read = 0;
   read += fread(&c1, sizeof(char), 1, fs->fd);
   read += fread(&c2, sizeof(char), 1, fs->fd);
   fseek(fs->fd, -read - 1, SEEK_CUR);
   if (read == 2) for (Punctuator i = Punctuator_BEGIN + 1; i < Punctuator_END; ++i) {
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
skipWhitespace(FileStream* fs) {
   while (fileStreamHasContent(fs) && isWhitespace(fileStreamPeek(fs))) {
      fileStreamRead(fs);
   }
}

b32
isDigit(char c) {
   b32 is_digit =
      c == '0' || c == '1' || c == '2' || c == '3' || c == '4' ||
      c == '5' || c == '6' || c == '7' || c == '8' || c == '9';
   return is_digit;
}

i32
identifyKeyword(Buffer* b) {
   size_t num_keywords = ArrayCount(g_keywords);

   // Add a NULL terminator so that the buffer can be used as a string.

   char end_char = *b->end;
   *b->end = '\0';
   i32 keyword = -1;

   for (size_t i = 0; i < num_keywords; ++i) {
      if (stringsAreEqual(g_keywords[i], b->current)) {
         keyword = (int)i;  // 1 points to the first keyword.
         break;
      }
   }

   *b->end = end_char;

   return keyword;
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

void
identifyToken(Buffer* b, Token* out) {
   i32 kw = -1;

   // TODO: - There is a specified ordering to parsing.. Keywords come
   // first, then identifiers... Set the correct order, or prove that
   // this is equivalent.

   // If it starts with a digit, it's numerical.
   if (isDigit(*b->current)) {
      out->type = TokenType_NUMBER;
      // TODO: Different kinds of numbers..
   }
   else if ((kw = identifyKeyword(b), kw != -1)) {
      out->type = TokenType_KEYWORD | kw;
      out->value.integer = kw;
   }
   else {
      out->type = TokenType_ID;
   }
}

Token
getToken(Arena* a, FileStream* fs) {
   Token t = {0};
   skipWhitespace(fs);
   int punctuator_token;  // Initialized in 'if' condition.
   if (!fileStreamHasContent(fs)) {
      return t;
   }
   else if ((punctuator_token = isPunctuator(fs))) {
      if (punctuator_token && punctuator_token < ASCII_MAX) {
         t.type = TokenType_PUNCTUATOR;
         t.value.character = fileStreamRead(fs);
      }
      else {
         t.type = TokenType_PUNCTUATOR_MULTICHAR;
         Assert(punctuator_token < 255);
         t.value.character = (u8)punctuator_token;
         // Advance the buffer by the length of othe operator.
         char* punctuator_str = g_punctuator_strings[indexOfPunctuator(punctuator_token)];
         size_t len = strlen(punctuator_str);
         fseek(fs->fd, len, SEEK_CUR);
      }
   }
   else if (fileStreamPeek(fs) == '\"') {
      // We are inside a string. Parse until we get the end of the string.
      // TODO: Escape characters.
      t.type = TokenType_STRING_LITERAL;
      Buffer token_buffer = {0};
      fseek(fs->fd, 1, SEEK_CUR);
      char* tmp = "String literals not working atm ;)";
      token_buffer.current = tmp;
      while (fileStreamRead(fs) != '\"') {
         if (!fileStreamHasContent(fs)) {
            // TODO: File parsing information.
            lexerError("Expected \" while parsing string literal.");
         }
      }
      token_buffer.end = tmp + strlen(tmp);
      char* str = getStringFromBuffer(&token_buffer);
      t.value.string = str;
   }
   // TODO: Operators
   // TODO: Constants
   // The rest are keywords and identifiers.
   else {
      Buffer token_buffer = {0};
      char tmptoken[128] = {0};
      token_buffer.current = tmptoken;
      token_buffer.end = tmptoken;
      //char* start = buffer;
      while (!(isWhitespace(fileStreamPeek(fs)) || isPunctuator(fs))) {
         *token_buffer.end++ = fileStreamRead(fs);
      }
      identifyToken(&token_buffer, &t);

      char* str = getStringFromBuffer(&token_buffer);
      if (t.type == TokenType_NUMBER) {
         int val = 0;
         ErrorCode err = parseInt(str, &val);
         if (err != SUCCESS) {
            char msg[1024] = {0};
            PrintString(msg, 1024, "Error while parsing integer. Token string %s", str);
            lexerError(msg);
         }
         t.value.integer = val;
      }
      else if (t.type & TokenType_KEYWORD) {
         t.type = TokenType_KEYWORD;
      }
      else if (t.type == TokenType_ID)  {
         t.value.string = str;
      }
   }
   t.line_number = fs->line_number;
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
      case TokenType_KEYWORD: {
         printf("KEYWORD %s", g_keywords[token->value.integer]);
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
      default: {
         printf("This token type is not printable yet. Type %d ", token->type);
      } break;
   }
   printf("]\n");
}

Token*
tokenize(Arena* a, FileStream* fs) {
   Token* t = AllocType(a, Token);
   Token* tokens = t;
   while (fileStreamHasContent(fs)) {
      *t = getToken(a, fs);
      if (t->type != TokenType_NONE) {
         t->next = AllocType(a, Token);
         t = t->next;
      }
   }

   return tokens;
}
