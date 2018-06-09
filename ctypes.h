#pragma once


struct AstNode;
struct Ctype {
   enum {
      Type_NONE = 0,

      // Arithmetic types
      Type_ARITH = (1<<0),
      Type_PEANO = (1<<8),  // NOTE: Type_INTEGER would be buggy af.
      Type_REAL  = (1<<9),

      // Integer types
      Type_CHAR = Type_ARITH | Type_PEANO | (1<<1),
      Type_INT  = Type_ARITH | Type_PEANO | (1<<2),

      // Real types
      Type_FLOAT = Type_ARITH | Type_REAL | (1<<3),
      Type_DOUBLE = Type_ARITH | Type_REAL | (1<<4),

      Type_FUNC = (1<<4),

      // Aggregates
      Type_STRUCT = (1<<5),
      Type_UNION = (1<<6),

      // Pointers
      Type_POINTER = (1<<7),
      // TODO array
      // TODO atomic
   } type;

   enum {
      Qual_CONST    = (1<<0),
      Qual_RESTRICT = (1<<1),
      Qual_VOLATILE = (1<<2),
   } qualifiers;

   // TODO: Move this into an associated struct.
   union {
      struct CtypeStruct {
         char* tag;
         struct AstNode* decls;
         u64 bits;
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


