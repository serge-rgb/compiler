typedef enum AstLeaf_n {
    Ast_NONE,
    Ast_NUMBER,
    Ast_ADD,
    Ast_MUL,
} AstLeaf;


typedef struct AstNode_s AstNode;
struct AstNode_s {
    AstLeaf val;

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
    left->sibling = right;
    right->sibling = NULL;
    return n;
}
