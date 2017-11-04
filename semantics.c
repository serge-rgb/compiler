
b32
isArithmeticType(Ctype ctype) {
   if (ctype == Type_INT ||
       ctype == Type_CHAR) {
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
       node->type == Ast_GEQ ||
       node->type == Ast_FUNCCALL ||
       node->type == Ast_NUMBER || node->type == Ast_ID) {
      isExpr = true;
   }
   return isExpr;
}
