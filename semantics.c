#include "ctypes.h"

b32
isArithmeticType(Ctype ctype) {
   b32 arith = ctype.type & Type_ARITH;
   return arith;
}

b32
isRealType(Ctype* ctype) {
   b32 real = ctype->type & Type_REAL;
   return real;
}

b32
isIntegerType(Ctype* ctype) {
   b32 integer = ctype->type & Type_PEANO;
   return integer;
}

b32
isAggregateType(Ctype* ctype) {
   b32 is_aggr = false;
   if (ctype->type == Type_STRUCT ||
       ctype->type == Type_UNION) {
      is_aggr = true;
   }
   return is_aggr;
}

u64
typeBits(Ctype* ctype) {
   u64 bits = 0;
   switch(ctype->type) {
      // 8 bits
      case Type_CHAR: {
         bits = 8;
      } break;

      // 32 bits
      case Type_FLOAT:
      case Type_INT: {
         bits = 32;
      } break;

      // 64 bits
      case Type_POINTER:
      case Type_DOUBLE: {
         bits = 64;
      } break;

      // N bits
      case Type_UNION:
      case Type_STRUCT: {
         bits = ctype->aggr.bits;
      } break;

      default: {
         NotImplemented("ctype size.");
      }
   }
   return bits;
}

b32
nodeIsExpression(AstNode* node) {
   b32 isExpr = false;
   if (node->type == Ast_MUL || node->type == Ast_DIV ||
       node->type == Ast_ADD || node->type == Ast_SUB ||
       node->type == Ast_EQUALS || node->type == Ast_LESS ||
       node->type == Ast_GREATER || node->type == Ast_LEQ ||
       node->type == Ast_GEQ || node->type == Ast_NOT_EQUALS ||
       node->type == Ast_FUNCCALL || node->type == Ast_ASSIGN_EXPR ||
       node->type == Ast_NUMBER || node->type == Ast_ID ||
       node->type == Ast_POSTFIX_INC || node->type == Ast_POSTFIX_DEC ||
       node->type == Ast_STRUCT_MEMBER_ACCESS) {
      isExpr = true;
   }
   return isExpr;
}

int
pointerSizeBits() {
   return 64;
}

u64
numBytesForType(Ctype ctype) {
   switch (ctype.type) {
      case Type_INT: {
         return 4;
      } break;
      case Type_CHAR: {
         return 1;
      } break;
      case Type_FUNC: {
         return 8*pointerSizeBits();
      } break;
      default: {
         NotImplemented("Handle different size of types");
      }
   }
   return 4;
}


b32
isLiteral(AstNode* node) {
   b32 result = false;
   AstType t = node->type;
   if (t == Ast_NUMBER ) {
      result = true;
   }
   return result;
}

AstNode*
funcDeclarationSpecifier(AstNode* node) {
  Assert (node->type == Ast_FUNCDEF);
  return node->child;
}

AstNode*
funcDeclarator(AstNode* node) {
  Assert (node->type == Ast_FUNCDEF);
  return node->child->next;
}

i32
funcNumParams(AstNode* node) {
  Assert (node->type == Ast_FUNCDEF);
  i32 nparam = 0;
  for(AstNode* param = funcDeclarator(node)->child->next;
      param != NULL;
      param = param->next) {
     ++nparam;
  }
  return nparam;
}
