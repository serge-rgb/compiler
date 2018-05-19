#pragma once


struct AstNode;
struct Ctype {
   enum {
      Type_NONE = 0,

      // Arithmetic types
      Type_ARITH = (1<<0),

      // Integer types
      Type_CHAR = Type_ARITH | (1<<1),
      Type_INT  = Type_ARITH | (1<<2),

      // Real types
      Type_FLOAT = Type_ARITH | (1<<3),

      // Function
      Type_FUNC = (1<<4),
   }; int type;

   int      bits;
   struct AstNode* node;
} typedef Ctype;

