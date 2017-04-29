#include <stdint.h>
#include <stddef.h>

typedef uint8_t u8;

#define MAX(a, b) ((a) < (b) ? (b) : (a))
#define MIN(a, b) ((a) > (b) ? (b) : (a))

#define KILOBYTES(n) (1024*n)
#define MEGABYTES(n) KILOBYTES(1024)
#define GIGABYTES(n) MEGABYTES(1024)