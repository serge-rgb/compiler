#include "ctypes.h"

b32
isArithmeticType(Type ctype) {
   if (ctype.type == Type_INT ||
       ctype.type == Type_CHAR) {
      return true;
   }
   return false;
}

b32
isIntegerType(Type ctype) {
   if (ctype.type == Type_INT ||
       ctype.type == Type_CHAR) {
      return true;
   }
   return false;
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
       node->type == Ast_POSTFIX_INC || node->type == Ast_POSTFIX_DEC) {
      isExpr = true;
   }
   return isExpr;
}

int
pointerSizeBytes() {
   return 8;
}

u64
numBytesForType(Type ctype) {
   switch (ctype.type) {
      case Type_INT: {
         return 4;
      } break;
      case Type_CHAR: {
         return 1;
      } break;
      case Type_FUNC: {
         return pointerSizeBytes();
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
