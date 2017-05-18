// This file declares all

#include <stdarg.h>  // varargs


typedef void FILE;
#if defined(_WIN32)
extern FILE* __acrt_iob_func(int idx);
    #define stdin  (__acrt_iob_func(0))
    #define stdout (__acrt_iob_func(1))
    #define stderr (__acrt_iob_func(2))
#else
    extern FILE* stderr;
    extern FILE* stdin;
    extern FILE* stdout;
#endif

#if defined(_WIN32)
#    define PRI_size "zi"
#else
    #define PRI_size "li"
#endif


extern size_t strlen(const char* str);
extern void*  memcpy( void *dest, const void *src, size_t count );
extern FILE*  fopen( const char* filename, const char* mode );
extern size_t fread( void          *buffer, size_t size, size_t count, FILE          *stream );
extern size_t fwrite( void          *buffer, size_t size, size_t count, FILE          *stream );
extern int    printf( const char *format, ... );
extern int    fprintf( FILE* stream, const char *format, ... );
extern int vprintf( const char *format, va_list vlist );
extern void*  calloc( size_t num, size_t size );
extern void   free( void* p );
extern void   exit( int error_code );
extern void   fclose ( FILE* fd );
