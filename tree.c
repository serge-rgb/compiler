typedef enum AstType_n {
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
   Ast_COMPOUND_STMT,
   Ast_STACK_REQ,
} AstType;

typedef struct AstNode_s AstNode;
struct AstNode_s {
   AstType type;
   Token* tok;
   union {
      struct {
         AstNode* child;
         AstNode* sibling;
      };
      struct {
         int stack_req;
      };
   };
};

typedef struct AstMul_s {
   AstNode* left;
   AstNode* right;
} AstMul;

static AstNode* AstNode_MUL;

AstNode*
makeAstNode(Arena* a, AstType type, AstNode* left, AstNode* right) {
   AstNode* n = AllocType(a, AstNode);
   n->type = type;
   n->child = left;
   if (left) {
      Assert(left->sibling == NULL);
      left->sibling = right;
   }

   return n;
}
