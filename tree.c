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

typedef enum CtypeType_n {
   Type_NONE,

   Type_INT,
   Type_CHAR,
} CtypeType;

typedef struct Ctype_s {
   CtypeType type; // Type type type...  type.
} Ctype;


typedef struct AstNode_s AstNode;
struct AstNode_s {
   AstType  type;
   union {
      // When the node corresponds to a token.
      Token*   tok;
      // Otherwise..
      Ctype* ctype;
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
numBytesForType(Ctype* ctype) {
   switch (ctype->type) {
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


void
astPrintIndent(FILE* fd, int indent) {
   for (int i = 0; i < indent; ++i) {
      fprintf(fd, " ");
   }
}

void
astPrintNode(FILE* fd, AstNode* node, int indent) {
   astPrintIndent(fd, indent);

   fprintf(fd, "%s ", g_ast_type_string[node->type]);

   switch (node->type) {
      case Ast_ID: {
         fprintf(fd, " [%s]", node->tok->value.string);
      } break;
      case Ast_NUMBER: {
         fprintf(fd, " [%d]", node->tok->value.integer);
      } break;
      default: {

      } break;
   }

   if (node->child) {
      astPrintNode(fd, node->child, indent + 2);
   }
   for (AstNode* n = node->sibling; n; n = n->sibling) {
      fprintf(fd, "\n");
      astPrintNode(fd, n, indent + 2);
   }
}

/**
 * Print out a lisp-style representation of the AST for a given translation unit.
 **/
void
astDebugOutput(AstNode* node) {
   FILE* fd = fopen("ast.lisp", "w");

   if (fd) {
      astPrintNode(fd, node, 0);
   }

   fclose(fd);
}

