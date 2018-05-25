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
      // TODO finish function

      Type_STRUCT = (1<<5),
      // TODO union
      // TODO pointer
      // TODO array
      // TODO atomic
   }; int type;


   enum {
      Qual_CONST    = (1<<0),
      Qual_RESTRICT = (1<<1),
      Qual_VOLATILE = (1<<2),
   }; int qualifiers;

   int      bits;

   union {
      struct CtypeStruct {
         char* tag;
         struct AstNode* decls;
         struct StructMember {
            char* id;
            u64 offset;
         } * members;
      } struct_;

      struct CtypeFunc {
         struct AstNode* node; // TODO: refactor?
      } func;

      // CtypeStruct struct_;
      // CtypeFunc func;
   };
} typedef Ctype;


