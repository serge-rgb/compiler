
typedef uint8_t u8;
typedef uint32_t b32;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int32_t i32;

#define true 1
#define false 0

#define ASCII_MAX 128
#define LINE_MAX  256

#define Max(a, b) ((a) < (b) ? (b) : (a))
#define Min(a, b) ((a) > (b) ? (b) : (a))

#define Kilobytes(n) (1024*n)
#define Megabytes(n) Kilobytes(1024)
#define Gigabytes(n) Megabytes(1024)

#define Assert(expr) PlatformAssert(expr)

#define PrintString(...) PlatformPrintString(__VA_ARGS__)

#define INVALID_CODE_PATH Assert(!"Invalid code path.")

#define ArrayCount(arr) (sizeof((arr)) / sizeof(*(arr)))

typedef enum ErrorCode_s {
    SUCCESS = 0,

    ERROR_PARSE_INT = 1,
} ErrorCode;
