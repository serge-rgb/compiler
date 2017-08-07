typedef enum AstType_n {
   Ast_NONE,

   Ast_TYPE_SPECIFIER,

   Ast_NUMBER,
   Ast_KEYWORD,

   Ast_IF,

   Ast_ID,

   Ast_FUNCDEF,
   Ast_FUNCCALL,

   Ast_DECLARATION,

   Ast_RETURN,

   Ast_ADD,
   Ast_SUB,
   Ast_MUL,
   Ast_DIV,
   Ast_LOGICAL_AND,
   Ast_EQUALS,
   Ast_LESS,
   Ast_LEQ,
   Ast_GREATER,
   Ast_GEQ,
   Ast_LOGICAL_OR,
   Ast_COMPOUND_STMT,
} AstType;

typedef struct AstNode_s AstNode;
struct AstNode_s {
   AstType  type;
   Token*   tok;
   AstNode* child;
   AstNode* sibling;

   u64      line_number;
};

typedef struct AstMul_s {
   AstNode* left;
   AstNode* right;
} AstMul;

static AstNode* AstNode_MUL;

AstNode*
makeAstNodeWithLineNumber(Arena* a, AstType type, AstNode* left, AstNode* right, u64 line_number) {
   AstNode* n = AllocType(a, AstNode);
   n->type = type;
   n->child = left;
   if (left) {
      if (right) {
         Assert(left->sibling == NULL);
         left->sibling = right;
      }
   }

   n->line_number = line_number;

   Assert(n->line_number);

   return n;
}

AstNode*
makeAstNode(Arena* a, AstType type, AstNode* left, AstNode* right) {
   u64 line_number = 0;
   if (left) {
      line_number = left->line_number;
   } else {
      fprintf(stderr, "WARNING: Calling makeAstNode with NULL left node. Will emit line number as 0. \n");
   }
   return makeAstNodeWithLineNumber(a, type, left, right, line_number);
}
