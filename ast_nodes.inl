
X(Ast_NONE)
X(Ast_TYPE_SPECIFIER)   // no family. fills ctype member.
X(Ast_NUMBER)           // no family.
X(Ast_IF)               // children: condition, then, else
X(Ast_ID)               //
X(Ast_FUNCDEF)          // children: declaration_spec, declarator, comp.statement
X(Ast_FUNCCALL)         // children: identifier, arg expr list
X(Ast_PARAMETER)        // children: decl.specifier, declarator
X(Ast_DECLARATION)      // children: decl.specifier, declarator, initializer
X(Ast_DECLARATOR)       // children: identifier,  depends
                                                // param-list
X(Ast_RETURN)           // children: expression
X(Ast_ADD)              // children: left, right
X(Ast_SUB)              // children: left, right
X(Ast_MUL)              // children: left, right
X(Ast_DIV)              // children: left, right
X(Ast_LOGICAL_AND)      // children: left, right
X(Ast_EQUALS)           // children: left, right
X(Ast_NOT_EQUALS)       // children: left, right
X(Ast_LESS)             // children: left, right
X(Ast_LEQ)              // children: left, right
X(Ast_GREATER)          // children: left, right
X(Ast_GEQ)              // children: left, right
X(Ast_LOGICAL_OR)       // children: left, right
X(Ast_COMPOUND_STMT)    // children: stmt,stmt,stmt,stmt,etc
X(Ast_ITERATION)        // children: declaration, control before, after (might be control), body
