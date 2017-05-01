
typedef uint8_t u8;
typedef uint32_t b32;
typedef uint32_t u32;
typedef uint64_t u64;

#define true 1
#define false 0

#define MAX(a, b) ((a) < (b) ? (b) : (a))
#define MIN(a, b) ((a) > (b) ? (b) : (a))

#define KILOBYTES(n) (1024*n)
#define MEGABYTES(n) KILOBYTES(1024)
#define GIGABYTES(n) MEGABYTES(1024)


typedef enum ErrorCode_s {
    SUCCESS = 0,

    ERROR_PARSE_INT = 1,
} ErrorCode;
