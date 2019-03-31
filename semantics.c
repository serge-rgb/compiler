b32
isArithmeticType(Ctype ctype) {
   b32 arith = ctype.type & Type_ARITH;
   return arith;
}

b32
isScalarType(Ctype ctype) {
   b32 scalar = isArithmeticType(ctype) || ctype.type == Type_POINTER || ctype.type == Type_ARRAY;
   return scalar;
}

b32
isRealType(Ctype* ctype) {
   b32 real = ctype->type & Type_REAL;
   return real;
}

b32
isIntegerType(Ctype* ctype) {
   b32 integer = ctype->type & Type_PEANO;
   return integer;
}

b32
isAggregateType(Ctype* ctype) {
   b32 is_aggr = ctype->type == Type_AGGREGATE;
   return is_aggr;
}

b32
isImmediate(RegVar* src) {
   b32 imm = false;
   imm = src->location.type == Location_IMMEDIATE;
   return imm;
}

b32
isDerivedType(Ctype* ctype) {
   b32 is_derived = ctype->type == Type_POINTER || ctype->type == Type_ARRAY;
   return is_derived;
}

u64
typeBits(Ctype* ctype) {
   u64 bits = 0;
   switch(ctype->type) {
      // 8 bits
      case Type_CHAR: {
         bits = 8;
      } break;

      // 32 bits
      case Type_FLOAT:
      case Type_INT: {
         bits = 32;
      } break;

         // 64 bits
      case Type_POINTER:
      case Type_DOUBLE:
      case Type_LONG: {
         bits = 64;
      } break;

         // N bits
      case Type_AGGREGATE: {
         bits = ctype->aggr.bits;
      } break;

      default: {
         NotImplemented("ctype size.");
      }
   }
   return bits;
}

b32
nodeIsExpression(AstNode* node) {
   b32 isExpr = false;
   if (node->type == Ast_MUL || node->type == Ast_DIV ||
       node->type == Ast_ADD || node->type == Ast_SUB ||
       node->type == Ast_EQUALS || node->type == Ast_LESS ||
       node->type == Ast_GREATER || node->type == Ast_LEQ ||
       node->type == Ast_GEQ || node->type == Ast_NOT_EQUALS ||
       node->type == Ast_FUNCCALL || node->type == Ast_ASSIGN_EXPR ||
       node->type == Ast_NUMBER || node->type == Ast_ID ||
       node->type == Ast_POSTFIX_INC || node->type == Ast_POSTFIX_DEC ||
       node->type == Ast_STRUCT_MEMBER_ACCESS || node->type == Ast_ADDRESS ||
       node->type == Ast_LOGICAL_AND || node->type == Ast_LOGICAL_OR) {
      isExpr = true;
   }
   return isExpr;
}

int
pointerSizeBits() {
   return 64;
}

u64
numBytesForType(Ctype ctype) {
   switch (ctype.type) {
      case Type_INT: {
         return 4;
      } break;
      case Type_CHAR: {
         return 1;
      } break;
      case Type_FUNC: {
         return 8*pointerSizeBits();
      } break;
      default: {
         NotImplemented("Handle different size of types");
      }
   }
   return 4;
}


b32
isLiteral(AstNode* node) {
   b32 result = false;
   AstType t = node->type;
   if (t == Ast_NUMBER ) {
      result = true;
   }
   return result;
}

void
paramType(Ctype* out, AstNode* node) {
   Assert(node->type == Ast_PARAMETER);

   AstNode* type_spec = node->child;
   AstNode* declarator = type_spec->next;

   if (declarator->declarator.pointer_stars) {
      out->type = Type_POINTER;
      Assert(out->pointer.pointee);
      out->pointer.pointee = &type_spec->ctype;
   }
   else {
      *out = type_spec->ctype;
   }
}

b32
ctypeEquals(Ctype a, Ctype b) {
   b32 equals = (a.type == b.type);
   // TODO: What about const?
   return equals;
}

int
intRank(Ctype type) {
   int rank = 0;
   switch (type.type) {
      case Type_UCHAR:
      case Type_CHAR: {
         rank = 1;
      } break;
      case Type_USHORT:
      case Type_SHORT: {
         rank = 2;
      } break;
      case Type_UINT:
      case Type_INT: {
         rank = 3;
      } break;
      case Type_ULONG:
      case Type_LONG: {
         rank = 4;
      } break;
   }
   return rank;
}

Ctype
arithmeticTypeConversion(Ctype a, Ctype b) {
   Ctype result = {0};

   if (ctypeEquals(a, b)) {
      result = a;
   }
   else if (isRealType(&a) && isIntegerType(&b)) {
      result = a;
   }
   else if (isRealType(&b) && isIntegerType(&a)) {
      result = b;
   }
   else if (isIntegerType(&a) && isIntegerType(&b)) {
      Ctype* lesser  = (intRank(a) > intRank(b)) ? &b : &a;
      Ctype* greater = (intRank(a) > intRank(b)) ? &a : &b;
      // If both are unsigned or both are signed, use the one ranked higher.
      if ((lesser->type & Type_UNSIGNED) == (greater->type & Type_UNSIGNED)) {
         result = *greater;
      } else {
         NotImplemented("Continue with integer promotion");
      }
   }
   else if (isRealType(&a) && isRealType(&b)) {
      if (typeBits(&a) > typeBits(&b)) {
         result = a;
      }
      else {
         result = b;
      }
   }

   Assert (result.type != Type_NONE);
   return result;
}

b32
hasUnalignedMembers(Tag* tag) {
   // TODO: when adding alignment pragma, implement this function
   return false;
}
