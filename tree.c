char* g_ast_type_string[] = {
#define X(node) #node ,
#include "ast_nodes.inl"
#undef X
};


static AstNode* AstNode_MUL;

AstNode*
makeAstNodeWithLineNumber(Arena* a, AstType type, AstNode* left, AstNode* right, u64 line_number) {
   AstNode* n = AllocType(a, AstNode);
   n->type = type;

   n->line_number = line_number;

   return n;
}

AstNode*
makeAstNode(Arena* a, AstType type, AstNode* left, AstNode* right) {
   u64 line_number = 0;
   // TODO: Pass line numbers to ast nodes
   return makeAstNodeWithLineNumber(a, type, left, right, line_number);
}



