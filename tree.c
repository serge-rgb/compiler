typedef enum AstLeaf_n {
   Ast_NONE,

   Ast_NUMBER,
   Ast_KEYWORD,
   Ast_ID,

   Ast_FUNCDEF,
   Ast_DECLARATION,

   Ast_RETURN,

   Ast_ADD,
   Ast_SUB,
   Ast_MUL,
   Ast_DIV,
   Ast_LOGICAL_AND,
   Ast_LOGICAL_OR,

} AstLeaf;

typedef struct AstNode_s AstNode;
struct AstNode_s {
   AstLeaf val;
   Token* tok;
   AstNode* child;
   AstNode* sibling;
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
      left->sibling = right;
      if (right) { right->sibling = NULL; }
   }

   return n;
}
