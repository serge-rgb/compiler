

typedef uint8_t  u8;
typedef uint8_t  b8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint32_t b32;
typedef uint64_t u64;

typedef int32_t i32;
typedef int64_t i64;

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
   raise(SIGTRAP);

#define ArrayError(f, l) ArrayErrorDefault(f, l)
#define ArrayPush(arr, e)                  \
        if (ArrayCount(arr) > n_##arr) {   \
           (arr)[n_##arr++] = e;           \
        } else {                           \
           ArrayError(__FILE__, __LINE__); \
        }

#define Break PlatformBreak

#define NotImplemented(message) \
        printf("Not Implemented! -- [%s]\n", message); \
        Assert(!message);


typedef enum ErrorCode_s {
    SUCCESS = 0,

    ERROR_PARSE_INT = 1,
} ErrorCode;
