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
      Type_DOUBLE = Type_ARITH | (1<<4),

      // Function
      Type_FUNC = (1<<4),

      Type_STRUCT = (1<<5),
      Type_UNION = (1<<6),

      Type_POINTER = (1<<7),
      // TODO union
      // TODO array
      // TODO atomic
   } type;


   enum {
      Qual_CONST    = (1<<0),
      Qual_RESTRICT = (1<<1),
      Qual_VOLATILE = (1<<2),
   } qualifiers;

   int      bits;

   union {
      struct CtypeStruct {
         char* tag;
         struct AstNode* decls;
         struct StructMember {
            char* id;
            struct Ctype* ctype;
            u64 offset;
         } * members;
      } struct_;

      struct CtypeFunc {
         struct AstNode* node; // Funcdef ast node.
      } func;

      // CtypeStruct struct_;
      // CtypeFunc func;
   };
} typedef Ctype;


