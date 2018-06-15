#pragma once

// ========================
// ======== Common ========
// ========================

typedef uint8_t  u8;
typedef uint8_t  b8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint32_t b32;
typedef uint64_t u64;

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

#define MaxU64 0xffffffffffffffff

#define Max(a, b) ((a) < (b) ? (b) : (a))
#define Min(a, b) ((a) > (b) ? (b) : (a))

#define Kilobytes(n) (1024*n)
#define Megabytes(n) Kilobytes(1024)
#define Gigabytes(n) Megabytes(1024)

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
        do {                                             \
        printf("Not Implemented! -- [%s]\n", message);   \
        Assert(!"Not implemented.");                                \
        } while (0)

// ==============================
// ======== Error codes =========
// ==============================

enum ErrorCode {
  Ok = 0,

  Fail = 1,

  IntParse,
  CouldNotAssemble,
  CouldNotLink,
} typedef ErrorCode;

// ==========================
// ======== Platform ========
// ==========================

#if defined(_WIN32)
   #include "platform_windows.c"
#else
   #include "platform_unix.c"

   #if defined(__APPLE__) && defined(__MACH__)
      #include "platform_macos.c"
   #elif defined(__linux__)
      #include "platform_linux.c"
   #endif
#endif


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
struct ExprType;

struct Ctype {
   enum {
      Type_NONE = 0,

      // Arithmetic types
      Type_ARITH = (1<<0),
      Type_PEANO = (1<<1),  // NOTE: Type_INTEGER would be buggy af.
      Type_REAL  = (1<<2),

      // Integer types
      Type_CHAR = Type_ARITH | Type_PEANO | (1<<3),
      Type_INT  = Type_ARITH | Type_PEANO | (1<<4),

      // Real types
      Type_FLOAT = Type_ARITH | Type_REAL | (1<<5),
      Type_DOUBLE = Type_ARITH | Type_REAL | (1<<6),

      Type_FUNC = (1<<7),

      // Structs and unions
      Type_AGGREGATE = (1<<8),

      // Pointers
      Type_POINTER = (1<<9),
      Type_ARRAY = (1<<10),
      // TODO atomic
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
         struct StructMember {
            char* id;
            struct Ctype* ctype;
            u64 offset;
         } * members;
      } aggr;

      struct CtypeFunc {
         struct AstNode* node; // Funcdef ast node.
      } func;

      struct CtypePointer {
         struct ExprType* pointee;
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
      // When the node corresponds to a token.
      Token*   tok;
      // Otherwise..
      Ctype    ctype;
      // When the node is Ast_DECLARATOR
      b32 is_pointer;
   };
   struct AstNode* child;
   struct AstNode* next;

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


// =====================================
// ======== Machine abstraction ========
// =====================================

struct TypedRegister {
  enum {
    TypedR_NONE,

    TypedR_IMMEDIATE_INTEGER,
    TypedR_IMMEDIATE_FLOAT,

    TypedR_REGISTER, // <- threshold. never a value

    TypedR_INTEGER,
    TypedR_FLOAT,
    TypedR_STACK,
    TypedR_HEAP,

  } type;

  u32 value;

} typedef TypedRegister;

struct Instruction {
  enum {
    MOV,  // NOTE: Includes rep
    PUSH,
    SUB,
    POP,
    ADD,
  } name;

  TypedRegister dst;
  TypedRegister src;
} typedef Instruction;


// Special registers.
TypedRegister tr_ACCUM;
TypedRegister tr_HELPER;
TypedRegister tr_SP;

struct Machine typedef Machine;

struct Codegen;
void machineInit(struct Codegen* c);

// tr* -- Typed Register functions
TypedRegister trInt(u64 i);

// ir* -- Push IR instruction.
void ir(Instruction i, u32 bits);
void irImm (Instruction i);


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

   Reg_Count,
} RegisterEnum;

enum EmitTarget {
   Target_TMP, // Using while I port to DDCG

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
      Location_STACK,
      Location_POINTER,
   } type;
   union {
      // REGISTER
      struct {
         RegisterEnum reg;
      };
      // IMMEDIATE
      struct {
         u64 immediate_value;  // Cast to appropriate value based on token type.
      };
      // STACK
      struct {
         u64 offset;
      };
   };
} typedef Location;

struct ExprType {
   Ctype    c;
   Location location;
} typedef ExprType;

// SymTable define
struct SymTable;
#define HashmapName     SymTable
#define HashmapPrefix   sym
#define HashmapKey      char*
#define HashmapValue    ExprType
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

#define SCOPE_HASH_SIZE 1024
struct Scope {
   Arena*      arena;

   int            if_count;
   struct Scope*  prev;
   SymTable       label_table;
   SymTable       tag_table;
   SymTable       symbol_table;
} typedef Scope;

enum CodegenConfigFlags {
   Config_TARGET_MACOS = (1<<0),
   Config_TARGET_LINUX = (1<<1),
   Config_TARGET_WIN   = (1<<2),
} typedef CodegenConfigFlags;

struct Codegen {
   Arena*      arena;
   Scope*      scope;
   char*       waiting;
   char*       file_name;
   u64         last_line_number;
   u32         config;   // CodegenConfigFlags enum

   Machine*    m;
   // Constants
   AstNode* one;
} typedef Codegen;
