char* g_keywords[] = {
   #define X(keyword) #keyword,
   #include "keywords.inl"
   #undef X
};

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

struct FileStream {
   FILE* fd;
   u64 line_number;
} typedef FileStream;

b32
fileStreamInit(FileStream* fs, char* fname) {
   // TODO: Handle different types of file encodings. (See C spec)
   FILE* fd = fopen(fname, "rb");
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

void
lexerError(char* msg) {
   fprintf(stderr, "Syntax error: %s\n", msg);
   Assert(!"Lexer error");
   exit(1);
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
      lexerError("Trying to read past end of file.");
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

b32
fileStreamPeek2(FileStream* fs, char out[2]) {
   size_t read = fread(out, sizeof(char), 2, fs->fd);
   fseek(fs->fd, -(int)read, SEEK_CUR);
   return read == 2;
}

void fileStreamClose(FileStream* fs) {
   if (fs->fd) {
      fclose(fs->fd);
   }
   else {
      fprintf(stderr, "WARNING: Trying to close NULL file descriptor.");
   }
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
   int read = 0;
   read += (int)fread(&c1, sizeof(char), 1, fs->fd);
   read += (int)fread(&c2, sizeof(char), 1, fs->fd);
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
            InvalidCodePath;
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
   b32 is_digit = (c-'0' >= 0) && (c-'0') <= 9;
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

enum NumberTokenType {
   NumberToken_OCTAL = (1<<0),
   NumberToken_DECIMAL = (1<<1),
   NumberToken_HEX = (1<<2),
} typedef NumberTokenType;

ErrorCode
parseNumber(NumberTokenType type, char* str, u64* out) {
   u64 base = 10;
   switch (type) {
      case NumberToken_OCTAL: {
         base = 8;
      }
      case NumberToken_HEX: {
         base = 16;
      }
   }

   ErrorCode result = Ok;
   int val = 0;
   while (*str != '\0') {
      char c = *str;
      if (c-'0' < 0 || c-'0' > 9) {
         if (type != NumberToken_HEX || (c-'A' < 0 || c-'A' > 5)) {
            result = IntParse;
         }
         break;
      }
      if (type == NumberToken_OCTAL && (c-'7') > 0) {
         result = IntParse;
         break;
      }
      u64 digit_val = 0;
      if (c >= '0' && c <= '9') {
         digit_val = c-'0';
      }
      if (c >= 'A' && c <= 'F') {
         digit_val = 10 + c-'A';
      }
      val = (val * base) + digit_val;
      str++;
   }
   *out = val;
   return result;
}

void
identifyToken(Buffer* b, Token* out) {
   i32 kw = -1;

   // TODO: - There is a specified ordering to parsing. Keywords come
   // first, then identifiers... Set the correct order, or prove that
   // this is equivalent.

   // If it starts with a digit, it's numerical.
   if (isDigit(*b->current)) {
      out->type = TType_NUMBER;
      // TODO: Different kinds of numbers..
   }
   else if ((kw = identifyKeyword(b), kw != -1)) {
      out->type = TType_KEYWORD | kw;
      out->value = kw;
   }
   else {
      out->type = TType_ID;
   }
}


void
lexNumberExpansion(FileStream* fs, NumberTokenType number_type, u64 integer, Token* t) {
   char sign = 0; // TODO: negative floats
   if (number_type == NumberToken_OCTAL) {
      lexerError("Floating point numbers can't be octal");
   }
   else if (number_type == NumberToken_HEX) {
      NotImplemented("Hex floating point");
   }
   else if (number_type == NumberToken_DECIMAL) {

   }

   if (!isDigit(fileStreamPeek(fs))) {
      lexerError("Floating point number must have digit sequence after point.");
   }
   char tmptoken[128] = Zero;
   int tok_i = 0;
   while (isDigit(fileStreamPeek(fs))) {
      tmptoken[tok_i++] = fileStreamRead(fs);
   }

   u64 number = 0;
   if (tok_i > 0 && parseNumber(number_type, tmptoken, &number) != Ok) {
      lexerError("Invalid floating point constant.");
   }

   u64 frac_bits = 0;
   u64 frac = 0;

   // Find the highest power of 10
   // TODO: Check for numbers above highest representable power of 10
   u64 pow = 1;
   while(pow < number) {
      pow *= 10;
   }

   while (number) {
      frac_bits++;
      frac <<= 1;
      number <<= 1;
      if (number >= pow) {
         frac |= 1;
         number -= pow;
      }
      if (frac_bits >= 63) {
         break;
      }
   }

   i64 exponent = 0;
   // Get exponent
   if (integer) {
      exponent = platformFirstBitSet(integer);
      integer &= ~(1 << exponent);
   }
   else {
      exponent = platformFirstBitSet(frac) - frac_bits;
      frac &= ~((u64)1 << platformFirstBitSet(frac));
      // normalize
      if (exponent < 0) {
         frac <<= -exponent;
      }
   }

   t->type = TType_DOUBLE;

   char maybe_suffix = fileStreamPeek(fs);
   if (maybe_suffix == 'f' || maybe_suffix == 'F' || maybe_suffix == 'l' || maybe_suffix == 'L') {
      fileStreamRead(fs);
      if (maybe_suffix == 'f' || maybe_suffix == 'F') {
         t->type = TType_FLOAT;
      }
      else {
         NotImplemented("l or L floating point suffix");
      }
   }

   switch(t->type) {
      case TType_DOUBLE: {
         NotImplemented("doubles");
      } break;
      case TType_FLOAT: {
         u8 exp = (u8)(i8)(127 + exponent);
         u32 bits = exp << 23;
         if (sign) {
            bits |= (1<<31);
         }

         // Move fraction to beginning of mantissa.
         if (frac_bits > 23) {
            frac >>= frac_bits - 23;
         }
         else {
            frac <<= 23 - frac_bits;
         }

         if (integer) {
            // move significand to beginning of mantissa
            if (exponent < 23) {
               integer <<= 23 - exponent;
            }
            else {
               integer >>= exponent - 23;
            }
            frac >>= exponent;
            bits |= integer;
         }
         bits |= frac;

         t->cast.real32 = *(float*)&bits;
      } break;
      default: {
         InvalidCodePath;
      }
   }
}

Token
getToken(Arena* a, FileStream* fs) {
   Token t = {0};
   skipWhitespace(fs);
   int punctuator_token;  // Initialized in 'if' condition.
   char comment[3] = Zero;
   if (!fileStreamHasContent(fs)) {
      return t;
   }
   else if (fileStreamPeek2(fs, comment) && !strcmp(comment, "/*")) {
      while (fileStreamPeek2(fs, comment) && strcmp(comment, "*/")) {
         fileStreamRead(fs);
      }
      for (int i = 0; i < 2; ++i) {
         fileStreamRead(fs);
      }
   }
   else if (fileStreamPeek2(fs, comment) && !strcmp(comment, "//")) {
      while (fileStreamRead(fs) != '\n') {}  // TODO: Robust EOL handling.
   }
   else if ((punctuator_token = isPunctuator(fs)) != 0) {
      t.type = TType_PUNCTUATOR;
      if (punctuator_token && punctuator_token < AsciiMax) {
         fileStreamRead(fs);
         if (punctuator_token == '.' && isDigit(fileStreamPeek(fs))) {
            lexNumberExpansion(fs, NumberToken_DECIMAL, 0, &t);
         }
         else {
            t.cast.character = punctuator_token;
         }
      }
      else if (punctuator_token < 255) {
         t.cast.character = (u8)punctuator_token;
         // Advance the buffer by the length of othe operator.
         char* punctuator_str = g_punctuator_strings[indexOfPunctuator(punctuator_token)];
         size_t len = strlen(punctuator_str);
         fseek(fs->fd, (long)len, SEEK_CUR);
      } else {
         lexerError("Invalid punctuator value.");
      }
   }
   else if (fileStreamPeek(fs) == '\"') {
      // We are inside a string. Parse until we get the end of the string.
      // TODO: Escape characters.
      t.type = TType_STRING_LITERAL;
      Buffer token_buffer = {0};
      fseek(fs->fd, 1, SEEK_CUR);
      char* tmp = "String literals not working atm ;)";
      token_buffer.current = tmp;
      while (fileStreamRead(fs) != '\"') {
         if (!fileStreamHasContent(fs)) {
            lexerError("Expected \" while parsing string literal.");
         }
      }
      token_buffer.end = tmp + strlen(tmp);
      char* str = getString(token_buffer.current);
      t.cast.string = str;
   }
   // TODO: Operators
   // TODO: Constants
   // The rest are keywords and identifiers.
   else {
      Buffer token_buffer = {0};
      // TODO: Stretchy-buffer for tokens...
      char tmptoken[128] = {0};
      token_buffer.current = tmptoken;
      token_buffer.end = tmptoken;

      char peek[3] = Zero;

      NumberTokenType number_type = NumberToken_DECIMAL;

      if (fileStreamPeek2(fs, peek) && stringsAreEqual(peek, "0x")) {
         number_type = NumberToken_HEX;
         for (int i = 0; i < 2; ++i) { fileStreamRead(fs); }
      }

      while (!(isWhitespace(fileStreamPeek(fs)) || isPunctuator(fs))) {
         *token_buffer.end++ = fileStreamRead(fs);
      }
      sz n_str = token_buffer.end - token_buffer.current;
      char* str = getString(token_buffer.current);

      if (number_type == NumberToken_HEX && n_str == 0) {
         lexerError("Incomplete hexadecimal number");
      }

      identifyToken(&token_buffer, &t);

      if (t.type == TType_NUMBER) {
         u64 val = 0;
         Assert(n_str);

         if (str[0] == '0' && n_str > 1) {
            if (number_type != NumberToken_HEX) {
               number_type = NumberToken_OCTAL;
            }
         }

         ErrorCode err = parseNumber(number_type, str, &val);
         if (err != Ok) {
            char msg[1024] = {0};
            PrintString(msg, 1024, "Error while parsing integer. Token string %s", str);
            lexerError(msg);
         }
         else {
            if (fileStreamPeek(fs) == '.') {
               fileStreamRead(fs);
               lexNumberExpansion(fs, number_type, val, &t);
            }
            else {
               t.cast.int32 = val;
            }
         }
      }
      else if (t.type & TType_KEYWORD) {
         t.type = TType_KEYWORD;
      }
      else if (t.type == TType_ID)  {
         t.cast.string = str;
      }
   }
   t.line_number = fs->line_number;
   return t;
}

void
tokenPrint(Token* token) {
   printf("[");
   switch(token->type) {
      case TType_NONE: {
         printf("NONE.");
      } break;
      case TType_PUNCTUATOR: {
         if (token->value < AsciiMax) {
            printf("PUNCTUATOR %c", (char)token->cast.character);
         }
         else {
            printf("PUNCTUATOR: %s", g_punctuator_strings[indexOfPunctuator(token->cast.character)]);
         }
      } break;
      case TType_KEYWORD: {
         printf("KEYWORD %s", g_keywords[token->value]);
      } break;
      case TType_NUMBER: {
         // TODO: Support floating point..
         printf("NUMBER: %i", (int)token->value);
      } break;
      case TType_ID: {
         printf("ID: %s", token->cast.string);
      } break;
      case TType_STRING_LITERAL: {
         printf("STRING: \"%s\"", token->cast.string);
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
      if (t->type != TType_NONE) {
         t->next = AllocType(a, Token);
         t = t->next;
      }
   }

   return tokens;
}
