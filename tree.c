typedef enum AstLeaf_n {
   Ast_NONE,

   Ast_NUMBER,
   Ast_KEYWORD,
   Ast_ID,

   Ast_FUNCDEF,

   Ast_ADD,
   Ast_SUB,
   Ast_MUL,
   Ast_DIV,

} AstLeaf;

typedef union AstNode_s AstNode;
union AstNode_s {
   struct {
      AstLeaf val;

      AstNode* child;
      AstNode* sibling;
   };
   struct {
      AstLeaf val_;  // Accessed as val

      Token* tok;
   };
};

typedef struct AstMul_s {
   AstNode* left;
   AstNode* right;
} AstMul;

static AstNode* AstNode_MUL;

AstNode*
makeAstNode(Arena* a, AstLeaf val, AstNode* left, AstNode* right) {
   AstNode* n = AllocType(a, AstNode);
   n->val = val;
   n->child = left;
   if (left) {
      Assert(right);
      left->sibling = right;
      right->sibling = NULL;
   }

   return n;
}
