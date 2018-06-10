#pragma once


struct AstNode;
struct ExprType;

struct Ctype {
   enum {
      Type_NONE = 0,

      // Arithmetic types
      Type_ARITH = (1<<0),
      Type_PEANO = (1<<1),  // NOTE: Type_INTEGER would be buggy af.
      Type_REAL  = (1<<2),

      // Integer types
      Type_CHAR = Type_ARITH | Type_PEANO | (1<<3),
      Type_INT  = Type_ARITH | Type_PEANO | (1<<4),

      // Real types
      Type_FLOAT = Type_ARITH | Type_REAL | (1<<5),
      Type_DOUBLE = Type_ARITH | Type_REAL | (1<<6),

      Type_FUNC = (1<<7),

      // Structs and unions
      Type_AGGREGATE = (1<<8),

      // Pointers
      Type_POINTER = (1<<9),
      Type_ARRAY = (1<<10),
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
      } aggr;

      struct CtypeFunc {
         struct AstNode* node; // Funcdef ast node.
      } func;

      struct CtypePointer {
         struct ExprType* pointee;
      } pointer;
   };
} typedef Ctype;


