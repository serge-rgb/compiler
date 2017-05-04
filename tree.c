typedef enum AstLeaf_n {
    Ast_NONE,
    Ast_INT,
} AstLeaf;


typedef struct AstNode_s AstNode;
struct AstNode_s {
    int type;
    Token* token;

    AstNode* child;
    AstNode* sibling;
};

typedef struct AstMul_s {
    AstNode* left;
    AstNode* right;
} AstMul;
