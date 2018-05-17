#pragma once


struct Type {
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

   union {
      // Type_FUNC
      struct {
#define MaxFuncParameters 256
         u32 n_params;
         struct Type* params;
         struct Type* return_;
      };
   } ext;
} typedef Type;

