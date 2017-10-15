typedef enum AstType_n {
#define X(node) node,
#include "ast_nodes.inl"
#undef X
} AstType;

char* g_ast_type_string[] = {
#define X(node) #node ,
#include "ast_nodes.inl"
#undef X
};

typedef enum Ctype_n {
   Type_NONE,

   Type_INT,
   Type_CHAR,
} Ctype;

typedef struct AstNode_s AstNode;
struct AstNode_s {
   AstType  type;
   union {
      // When the node corresponds to a token.
      Token*   tok;
      // Otherwise..
      Ctype    ctype;
   };
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

u64
numBytesForType(Ctype ctype) {
   switch (ctype) {
      case Type_INT: {
         return 4;
      } break;
      case Type_CHAR: {
         return 1;
      } break;
      default: {
         Assert(!"Don't know the size of this type.");
      }
   }
   return 4;
}


