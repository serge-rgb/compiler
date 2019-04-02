
// ====================================
// ======== Hungarian notation ========
// ====================================

// s_*  - stretchy buffer

// ==============================
// ======== Error codes =========
// ==============================

enum ErrorCode {
   Ok = 0,
   Fail = 1,

   CouldNotReadFile,
   CouldNotOpenDir,
   NotADirectory,
   WrongNumberOfArguments,
   InvalidArgument,

   // Argument parsing.
   Filename,
   Flag,

   // Compiler work flow
   IntParse,
   CouldNotAssemble,
   CouldNotLink,
} typedef ErrorCode;

static char* SccErrorMessage;

// ========================
// ======== Common ========
// ========================

#define Debug 1

typedef uint8_t  u8;
typedef uint8_t  b8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint32_t b32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int32_t i32;
typedef int64_t i64;

typedef float f32;
typedef double f64;

typedef size_t sz;

// Printf format strings.
#define FORMAT_I64 PLATFORM_FORMAT_I64
#define FORMAT_U64 PLATFORM_FORMAT_U64

#define true 1
#define false 0
#define Zero {0}

#define AsciiMax 128
#define LineMax  256
#define PathMax  256
#define LabelMax 128

#define MaxU64 0xffffffffffffffff

#define Max(a, b) ((a) < (b) ? (b) : (a))
#define Min(a, b) ((a) > (b) ? (b) : (a))

#define Kilobytes(n) (1024*n)
#define Megabytes(n) Kilobytes(1024)
#define Gigabytes(n) Megabytes(1024)

// SystemV parameter passing
#define Eightbytes(n) (n*8*8)

// Next multiple of p which is greater than v, or v if it is already aligned.
#define AlignPow2(v, p) ( \
	                        (((v) + (p) - 1) & ~((p) - 1)) \
                        )

#define Assert(expr) PlatformAssert(expr)

#define PrintString(...) PlatformPrintString(__VA_ARGS__)

#define InvalidCodePath Assert(!"Invalid code path.")

#define ArrayCount(arr) (sizeof((arr)) / sizeof(*(arr)))

#define ArrayErrorDefault(f, l) \
   fprintf(stderr, "Array overflow in %s:%d\n", f, l); \
   Assert(!"Array error.");

#define ArrayError(f, l) ArrayErrorDefault(f, l)
#define ArrayPush(arr, e)                  \
   if (ArrayCount(arr) > n_##arr) {   \
      (arr)[n_##arr++] = e;           \
   } else {                           \
      ArrayError(__FILE__, __LINE__); \
   }

#define Break PlatformBreak

#define DevBreak(message) \
   fprintf(stderr, "Compiler dev: Keep going here [%s]\n", message); \
   Break;

#define NotImplemented(message)                          \
   printf("Not Implemented! -- [%s]\n", message);   \
   Assert(0);

// ========================
// ======== String ========
// ========================
char* getString(char* orig);

// ==========================
// ======== Platform ========
// ==========================

#if defined(_WIN32)
   #include "platform_windows.h"
#else
   #include "platform_unix.h"
#endif

u64 platformFirstBitSet(u64 val);
ErrorCode platformListDirectory(char*** out_files, char* dirname, b32 (*filter)(char*));
ErrorCode platformCompileAndLinkAsmFile(char* filename_without_extension);

ErrorCode platformAssemble(char* nasm_path, char* asm_file);
ErrorCode platformLink(char* ld_path, char* filename_without_extension);


// ========================
// ======== Memory ========
// ========================


#define ARENA_DEFAULT_BLOCK_SIZE Megabytes(1)

#define AllocType(arena, type)         \
        allocate(arena, sizeof(type))

#define AllocArray(arena, type, count)          \
        allocate(arena, sizeof(type) * (count))

#define ArenaBootstrap(object, arenaName)    \
        {                                            \
         size_t sz = sizeof(*object);              \
         u8* block = arenaBlock(&sz);  \
         block += sizeof(ArenaHeader); \
         Arena a = {                   \
            .used = 0,                 \
            .size = sz,        \
            .block = block,    \
         };                            \
         Arena* p = allocate(&a, sizeof(Arena));      \
         *p = a;                              \
         object->arenaName = p;               \
      }


struct Arena {
   u8*     block;
   size_t  used;
   size_t  size;

   struct Arena* next;
} typedef Arena;

void* allocate(Arena* a, size_t num_bytes);
void  deallocate(Arena* a);

// =======================
// ======== Ctype ========
// =======================


struct AstNode;
struct RegVar;

struct Ctype {
   enum {
      Type_NONE = 0,

      // Arithmetic types
      Type_ARITH = (1<<0),
      Type_PEANO = (1<<1),  // NOTE: Type_INTEGER would be too easy to confuse with Type_INT
      Type_REAL  = (1<<2),

      // Integer types
      Type_CHAR = Type_ARITH | Type_PEANO | (1<<3),
      Type_SHORT  = Type_ARITH | Type_PEANO | (1<<4),
      Type_INT  = Type_ARITH | Type_PEANO | (1<<5),
      Type_LONG = Type_ARITH | Type_PEANO | (1<<6),

      Type_UNSIGNED = (1 << 7),

      Type_UCHAR  = Type_CHAR | Type_UNSIGNED,
      Type_USHORT  = Type_SHORT | Type_UNSIGNED,
      Type_UINT  = Type_INT | Type_UNSIGNED,
      Type_ULONG  = Type_LONG | Type_UNSIGNED,

      // Real types
      Type_FLOAT = Type_ARITH | Type_REAL | (1<<10),
      Type_DOUBLE = Type_ARITH | Type_REAL | (1<<11),


      Type_FUNC = (1<<12),

      // Structs and unions
      Type_AGGREGATE = (1<<13),

      // Pointers
      Type_POINTER = (1<<14),
      Type_ARRAY = (1<<15),

      Type_ENUM = (1<<16),
   } type;

   enum {
      Qual_CONST    = (1<<0),
      Qual_RESTRICT = (1<<1),
      Qual_VOLATILE = (1<<2),
   } qualifiers;

   // TODO: Move this into an associated struct.
   union {
      struct CtypeAggregate {
         char* tag;
         struct AstNode* decls;
         u64 bits;
      } aggr;

      struct CtypeFunc {
         struct Ctype* return_type;
         struct Ctype* s_params;
      } func;

      struct CtypePointer {
         struct Ctype* pointee;
      } pointer;
   };
} typedef Ctype;


// =======================
// ======== Lexer ========
// =======================


enum Keyword {
   #define X(keyword) Keyword_ ## keyword,
   #include "keywords.inl"
   #undef X
} typedef Keyword;

enum TokType {
   TType_NONE,
   TType_PUNCTUATOR,
   TType_STRING_LITERAL,
   TType_NUMBER,
   TType_FLOAT,
   TType_DOUBLE,
   TType_ID,
   TType_KEYWORD = 0xF000,
} typedef TokType;

enum Punctuator {
   Punctuator_BEGIN = /*...*/  128,  // ASCII codes are reserved for single-char punctuators.
#define X(op, name) name,
#include "punctuators.inl"
#undef X
   Punctuator_END,
} typedef Punctuator;

struct Token {
   TokType type;
   union {
      u64 value;
      union {
         char*    string;
         u8       character;
         u16      uint16;
         i32      int32;
         i64      int64;
         f32      real32;
         f64      real64;
      } cast;
   };

   u64 line_number;

   struct Token*    next;
} typedef Token;


// =====================
// ======== AST ========
// =====================


typedef enum AstType_n {
#define X(node) node,
#include "ast_nodes.inl"
#undef X
} AstType;

struct AstNode {
   AstType  type;
   union {
      // Ast_DECLARATION_SPECIFIER
      struct AstNodeDeclSpec {
         Ctype ctype;
      } as_decl_specifier;

      // Ast_DECLARATOR
      struct AstNodeDeclarator {
         char* id;
         struct AstNode* params;  // Ast_PARAMETER
         u32 pointer_stars;
      } as_declarator;

      // Ast_NUMBER
      struct {
         Token* tok;
      } as_number;

      // Ast_ID
      struct {
         Token* tok;
      } as_id;

      // Ast_IF
      struct {
         struct AstNode* condition;
         struct AstNode* then;
         struct AstNode* else_;
      } as_if /*lol*/;

      // Ast_ADDRESS
      // Ast_POSTFIX_INC
      // Ast_POSTFIX_DEC
      // Ast_RETURN
      struct {
         struct AstNode* expr;
      } as_single_expr;

      // Ast_PARAMETER
      struct AstNodeParameter {
         struct AstNode* decl_specifier;
         struct AstNodeDeclarator* declarator;
         struct AstNodeParameter* next;
      } as_parameter;

      // Ast_ARGUMENT_EXPR_LIST
      struct AstNodeArgument {
         struct AstNode* expr;
         struct AstNodeArgument* next;
      } as_arg_expr_list;

      // Ast_FUNCDEF
      struct {
         char* label;
         Ctype ctype;  // Function type (return type & param types)
         struct AstNodeDeclarator* declarator;
         struct AstNode* compound_stmt;
      } as_funcdef;

      // Ast_ASSIGN_EXPR
      struct AstNodeAssignExpr {
         struct AstNode* lhs;
         Token* op;
         struct AstNode* rhs;
      } as_assignment_expr;

      // Ast_FUNCCALL
      struct {
          struct AstNode* expr;
          struct AstNodeArgument* arg_expr_list;
      } as_funccall;

      // Ast_DECLARATOR_LIST
      struct AstNodeDeclaratorList {
         struct AstNodeDeclarator* declarator;
         struct AstNodeDeclaratorList* next;
      } as_declarator_list;

      // Ast_DECLARATION
      struct AstNodeDeclaration {
         struct AstNodeDeclSpec* decl_spec;
         struct AstNodeDeclaratorList* declarators;
         struct AstNode* rhs;
      } as_declaration;

      // Ast_DECLARATION_LIST
      struct AstNodeDeclarationList {
         struct AstNodeDeclaration* declaration;
         struct AstNodeDeclarationList* next;
      } as_declaration_list;

      // Ast_STRUCT_MEMBER_ACCESS
      struct {
         struct AstNode* primary_expr;
         char* field;
      } as_member_access;

      // Binary expressions
      struct {
         struct AstNode* left;
         struct AstNode* right;
      } as_binary_expr;

      // Ast_COMPOUND_STMT
      struct AstNodeCompoundStmt {
         struct AstNode* stmt;
         struct AstNodeCompoundStmt* next;
      } as_compound_stmt;

      struct {
         struct AstNodeDeclaration* declaration;
         struct AstNode* before_iteration;
         struct AstNode* after_iteration;
         struct AstNode* body;
      } as_iteration;

      // Ast_TRANSLATION_UNIT
      struct AstNodeTU {
         struct AstNode* node;
         struct AstNodeTU* next;
      } as_tu;
      // When the node corresponds to a token.
      // TODO: Get rid of all variables not inside a struct.
   };
   Token*   tok;

   u64 line_number;
} typedef AstNode;


// ========================
// ======== Parser ========
// ========================


struct AggregateSizes;
#define HashmapName AggregateSizes
#define HashmapPrefix aggr
#define HashmapKey char*
#define HashmapValue u64
#include "hashmap.inl"

struct Parser {
   Token* token;  // The next token to parse.
   Arena* arena;
   AstNode* tree;
   char* file_name;
   AggregateSizes sizes;

   enum {
     ParserFlag_FANSI = (1 << 0),
   } flags;
} typedef Parser;

// =========================
// ======== Codegen ========
// =========================

typedef enum RegisterEnum {
   Reg_RAX,
   Reg_RBX,
   Reg_RCX,
   Reg_RDX,
   Reg_RSI,
   Reg_RDI,
   Reg_R8,
   Reg_R9,
   Reg_R10,
   Reg_R11,
   Reg_R12,
   Reg_R13,
   Reg_R14,
   Reg_R15,

   // Floating point registers.
   Reg_XMM0,
   Reg_XMM1,
   Reg_XMM2,
   Reg_XMM3,
   Reg_XMM4,
   Reg_XMM5,
   Reg_XMM6,
   Reg_XMM7,

   Reg_Count,
} RegisterEnum;

enum EmitTarget {
   Target_NONE,
   Target_ACCUM,
   Target_STACK,
} typedef EmitTarget;

// TODO: Make Location fit in a register and use a hack to fit 64-bit values.
struct Location {
   enum {
      Location_INVALID,

      Location_IMMEDIATE,
      Location_REGISTER,
      Location_STACK,  // RSP relative

      Location_REG_POINTER,  // Pointer relative to register
   } type;
   union {
      // REGISTER
      struct {
         RegisterEnum reg;
         // STACK_FROM_REG
         i32          reg_offset;
      };
      // IMMEDIATE
      union {
         union {
            f64 real64;
            i64 int64;
         } cast;
         u64 immediate_value;  // Cast to appropriate value based on token type.
      };
      // STACK
      struct {
         i64 offset;
      };
   };
} typedef Location;

struct RegVar {
   Ctype    c;
   Location location;
} typedef RegVar;

// SymTable definition
struct SymTable;
#define HashmapName     SymTable
#define HashmapPrefix   sym
#define HashmapKey      char*
#define HashmapValue    RegVar
#define HashFunction    hashStrPtr
#define KeyCompareFunc  compareStringKey
#include "hashmap.inl"

typedef struct StackValue_s {
   unsigned int offset  : 6;
   enum {
      Stack_OFFSET,
      Stack_QWORD,
   } type : 2;
} StackValue;

struct TagMember {
   char* id;
   Ctype ctype;
   i64 offset;
} typedef TagMember;

struct Tag {
   RegVar rvar;
   TagMember* s_members;
} typedef Tag;

// TagTable definition
struct TagTable;
#define HashmapName     TagTable
#define HashmapPrefix   tag
#define HashmapKey      char*
#define HashmapValue    Tag
#define HashFunction    hashStrPtr
#define KeyCompareFunc  compareStringKey
#include "hashmap.inl"


#define SCOPE_HASH_SIZE 1024
struct Scope {
   Arena*      arena;

   int            if_count;
   struct Scope*  prev;

   // Name spaces

   //SymTable       label_table;

   // Structs, enums, unions
   TagTable       tag_table;

   SymTable       symbol_table;
} typedef Scope;

Tag* findTag(Scope* scope, char* name);

enum MachineConfigFlags {
   Config_TARGET_MACOS = (1<<0),
   Config_TARGET_LINUX = (1<<1),
   Config_TARGET_WIN   = (1<<2),

   Config_INSTR_OUTPUT_DISABLED = (1<<3),

} typedef MachineConfigFlags;

struct Machine typedef Machine;

struct Codegen {
   Arena*      arena;
   Scope*      scope;

   char*       file_name;
   u64         last_line_number;

   AstNode*    current_function;

   // Disable/Enable instruction output.
   #define MaxInstrOutputStack 8
   char instrOutputStack[MaxInstrOutputStack];
   char n_instrOutputStack;

   Machine*    m;
   // Constants
   AstNode* one;
} typedef Codegen;


// =====================================
// ======== Machine abstraction ========
// =====================================
struct Machine {
   u8 flags; // MachineConfigFlags

   void (*stackPop)(void* machine, RegVar* et);

   Location (*stackPush)(void* machine, Location location);
   Location (*stackPushReg)(void* machine, RegisterEnum reg);
   Location (*stackPushImm)(void* machine, i64 value);
   Location (*stackPushOffset)(void* machine, u64 bytes);

   void (*stackAddress)(void* machine, RegVar* entry, Location target_loc);

   void (*addressOf)(void* machine, Location* loc, Location target_loc);

   void (*functionPrelude)(void* machine, char* func_name);

   void (*beginFuncParams ) (void* machine);

   void (*pushParameter)(void* machine, Scope* scope, RegVar* rvar);
   Location (*popParameter)(void* machine, Scope* scope, Ctype* ctype);

   void (*endFuncParams ) (void* machine);

   void (*functionEpilogue)(void* machine);

   void (*mov)(void* machine, RegVar* dst, RegVar* src);

   void (*add)(void* machine, RegVar* dst, RegVar* src);
   void (*sub)(void* machine, RegVar* dst, RegVar* src);
   void (*mul)(void* machine, RegVar* dst, RegVar* src);
   void (*div)(void* machine, RegVar* dst, RegVar* src);
   void (*mod)(void* machine, RegVar* dst, RegVar* src);
   void (*bitAnd)(void* machine, RegVar* dst, RegVar* src);
   void (*bitOr)(void* machine, RegVar* dst, RegVar* src);
   void (*bitXor)(void* machine, RegVar* dst, RegVar* src);
   void (*shiftLeft)(void* machine, RegVar* dst, RegVar* src);
   void (*shiftRight)(void* machine, RegVar* dst, RegVar* src);

   void (*cmp)(void* machine, RegVar* dst, RegVar* src);
   void (*cmpSet)(void* machine, AstType type, Location dst);

   void (*cmpJmp)(void* machine, AstType ast_type, char* label);
   void (*cmpJmpStackTop)(void* machine, AstType ast_type, Location loc_to_compare, RegVar* type, char* then, char* els);
   void (*testAndJump)(void* machine, RegisterEnum reg, u32 bits, char* then, char* els);
   void (*jmp)(void* machine, char* label);

   void (*label)(void* machine, char* label);
   void (*call)(void* machine, char* func);

   void (*finish)(void* machine);

   // Registers

   RegVar* (*immediateFromToken)(void* machine, Token* tok);

   RegVar* (*helper)(void* machine, int type /*Ctype.type*/, u32 bits);
   RegVar* (*helperC)(void* machine, Ctype c);
   RegVar* (*accum)(void* machine, int type /*Ctype.type*/, u32 bits);
   RegVar* (*accumC)(void* machine, Ctype c);


   // Conversion. Result in stack
   void (*convertFloatToInt)(void* machine, Location from, EmitTarget target);
   void (*convertIntegerToFloat)(void* machine, Location from, EmitTarget target);
   void (*convertDoubleToInt)(void* machine, Location from, EmitTarget target);
   void (*convertIntegerToDouble)(void* machine, Location from, EmitTarget target);
   void (*convertDoubleToFloat)(void* machine, Location from, EmitTarget target);
   void (*convertFloatToDouble)(void* machine, Location from, EmitTarget target);
};

Machine* makeMachineX64(Arena* a, MachineConfigFlags mflags);

struct Codegen;

// ======================
// ======== User ========
// ======================

enum CompilerFlag {
   CompilerFlag_TEST_ALL = (1<<0),
} typedef CompilerFlag;
